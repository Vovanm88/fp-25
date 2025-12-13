(* Audio decoder *)
open Codec_utilities

(* Decompress a single audio track *)
let decompress_track compressed_data original_min original_max =
  Audiomodel.decompress_track compressed_data original_min original_max

(* Decompress stereo tracks (mid and side channels) *)
let decompress_stereo_tracks compressed_mid compressed_side min_mid max_mid min_side max_side =
  let decompressed_mid = decompress_track compressed_mid min_mid max_mid in
  let decompressed_side = decompress_track compressed_side min_side max_side in
  (decompressed_mid, decompressed_side)

(* Convert Mid-Side to LR (skip if mono) *)
let mid_side_to_lr mid side is_stereo =
  if is_stereo then
    (* For stereo, perform Mid-Side → LR conversion *)
    Audiomodel.mid_side_to_lr mid side
  else
    (* For mono, return mid as both left and right *)
    (mid, side)

(* Pass-through for silence replacement (silence is already lost, so just pass data through) *)
let pass_through_silence data = data

(* Reconstruct signal from overlapping windows using overlap-add *)
let overlap_add_windows windows =
  if List.length windows = 0 then []
  else
    (* Extract window data and positions *)
    let windows_with_data = List.filter_map (fun win ->
      match win.Audiomodel.window_data, win.Audiomodel.start_sample, win.Audiomodel.end_sample with
      | Some data, Some start, Some end_pos -> Some (data, start, end_pos, win.Audiomodel.original_length)
      | _ -> None
    ) windows in
    
    if List.length windows_with_data = 0 then []
    else
      (* Find maximum end position from end_sample *)
      let max_end = List.fold_left (fun acc (_, _, end_pos, _) ->
        max acc (end_pos + 1)
      ) 0 windows_with_data in
      
      (* Get original_length from first window if available *)
      let original_length = match List.hd windows_with_data with
        | (_, _, _, Some len) -> Some len
        | _ -> None
      in
      
      (* For overlap-add, we need space for all windows, so use max_end *)
      (* Then trim to original_length if the track was padded *)
      let output_length = max_end in
      
      (* Initialize output array with zeros *)
      let output = Array.make output_length 0.0 in
      
      (* Apply overlap-add: sum all windows at their positions *)
      (* Note: windowing is already applied in encoder, so we just sum the windows *)
      (* Windowing functions are designed so that overlapping windows sum correctly *)
      List.iter (fun (window_data, start_pos, _end_pos, _) ->
        (* Add windowed data to output at correct position *)
        List.iteri (fun i sample ->
          let pos = start_pos + i in
          if pos < output_length then
            output.(pos) <- output.(pos) +. sample
        ) window_data
      ) windows_with_data;
      
      (* Convert to list and trim to original_length if padded *)
      let result = Array.to_list output in
      match original_length with
      | Some len when len < output_length ->
        (* Trim to original length (remove padding) - use efficient take *)
        let rec take n = function
          | [] -> []
          | x :: xs -> if n <= 0 then [] else x :: take (n - 1) xs
        in
        take len result
      | _ -> result

(* Dequantize data first if quantized *)
let dequantize_segments segments =
  List.map (fun seg ->
    match seg.Audiomodel.quantized_data with
    | None -> seg  (* No quantized data, data is in raw_data already *)
    | Some quantized_bands ->
      (* Check that we have matching counts and non-empty data *)
      let num_bands = List.length quantized_bands in
      let num_levels = List.length seg.Audiomodel.quantization_levels in
      let num_ranges = List.length seg.Audiomodel.band_ranges in
      if num_bands = 0 || num_levels = 0 || num_ranges = 0 then
        (* Empty data - return segment as-is *)
        seg
      else if num_bands <> num_levels || num_bands <> num_ranges then
        (* Mismatch - skip dequantization, return segment as-is *)
        seg
      else
        (* Dequantize each band, skipping invalid quantization levels *)
        let dequantized_bands = 
          let rec dequantize_all quant_bands quant_levels ranges acc =
            match quant_bands, quant_levels, ranges with
            | [], [], [] -> List.rev acc
            | [], _, _ -> List.rev acc  (* quant_bands exhausted *)
            | _, [], _ -> List.rev acc  (* quant_levels exhausted *)
            | _, _, [] -> List.rev acc  (* ranges exhausted *)
            | qb :: qbs, ql :: qls, (min_val, max_val) :: rs ->
              (* Check quantization level is valid before calling dequantize *)
              if ql <= 0 then
                (* Invalid quantization level - skip this band *)
                dequantize_all qbs qls rs ([] :: acc)
              else
                try
                  let deq = Quantization.dequantize qb ql min_val max_val in
                  dequantize_all qbs qls rs (deq :: acc)
                with
                | Failure _ ->
                  (* Quantization error - skip this band *)
                  dequantize_all qbs qls rs ([] :: acc)
          in
          dequantize_all quantized_bands seg.Audiomodel.quantization_levels seg.Audiomodel.band_ranges []
        in
      (* Update segment with dequantized data in raw_data *)
      {
        seg with
        Audiomodel.raw_data = Some dequantized_bands;
        quantized_data = None;  (* Clear quantized data *)
        window_data = seg.Audiomodel.window_data;
        start_sample = seg.Audiomodel.start_sample;
        end_sample = seg.Audiomodel.end_sample;
        original_length = seg.Audiomodel.original_length;
        frequency_bands = seg.Audiomodel.frequency_bands;  (* Preserve for merge_frequency_bands *)
      }
  ) segments

(* Apply IMDCT Level 2 to each band (reverse of MDCT Level 2) *)
let apply_imdct_level2 segments =
  List.map (fun seg ->
    match seg.Audiomodel.raw_data with
    | None -> seg  (* No band data, skip *)
    | Some mdct_level2_bands ->
      (* Apply IMDCT to each second-level MDCT band *)
      let time_domain_bands = List.map Dsp.imdct_transform mdct_level2_bands in
      (* Update segment with time-domain bands, preserving window_data *)
      {
        seg with
        Audiomodel.raw_data = Some time_domain_bands;
        (* Preserve all fields for merge_frequency_bands *)
        window_data = seg.Audiomodel.window_data;
        start_sample = seg.Audiomodel.start_sample;
        end_sample = seg.Audiomodel.end_sample;
        original_length = seg.Audiomodel.original_length;
        frequency_bands = seg.Audiomodel.frequency_bands;
      }
  ) segments

(* Apply MDCT Level 1 to each time-domain band (reverse of IMDCT bands) *)
let apply_mdct_level1_to_bands segments =
  List.map (fun seg ->
    match seg.Audiomodel.raw_data with
    | None -> seg  (* No band data, skip *)
    | Some time_domain_bands ->
      (* Apply MDCT to each time-domain band *)
      let mdct_level1_bands = List.map Dsp.mdct_transform time_domain_bands in
      (* Update segment with first-level MDCT coefficients per band, preserving window_data *)
      {
        seg with
        Audiomodel.raw_data = Some mdct_level1_bands;
        (* Preserve all fields for merge_frequency_bands *)
        window_data = seg.Audiomodel.window_data;
        start_sample = seg.Audiomodel.start_sample;
        end_sample = seg.Audiomodel.end_sample;
        original_length = seg.Audiomodel.original_length;
        frequency_bands = seg.Audiomodel.frequency_bands;
      }
  ) segments

(* Merge frequency bands back into single MDCT coefficient array *)
let merge_frequency_bands segments =
  List.map (fun seg ->
    match seg.Audiomodel.raw_data, seg.Audiomodel.frequency_bands with
    | None, _ -> seg  (* No band data, skip *)
    | Some bands, freq_bands when List.length bands = List.length freq_bands ->
      (* Reconstruct original MDCT coefficient array by merging bands *)
      (* We need to determine the original number of MDCT coefficients *)
      (* Calculate MDCT output size from window_type (window_size / 2) *)
      let num_coeffs = match seg.Audiomodel.window_data with
      | Some window_data -> 
        (* MDCT transforms n samples into n/2 coefficients *)
        List.length window_data / 2
      | None ->
        (* Calculate from window_type - this is the preferred method to save space *)
        (* window_data is no longer saved, so we always use window_type *)
        Audiomodel.get_window_size seg.Audiomodel.window_type / 2
      in
      
      let merged = Array.make num_coeffs 0.0 in
      
      (* Place each band at its correct frequency position *)
      (* MDCT coefficient k corresponds to normalized frequency: f = k / num_coeffs *)
      List.iter2 (fun band (start_freq, end_freq) ->
        let start_idx = int_of_float (start_freq *. float_of_int num_coeffs) in
        let end_idx = min (num_coeffs - 1) (int_of_float (end_freq *. float_of_int num_coeffs)) in
        (* Place band coefficients at correct positions *)
        List.iteri (fun i coeff ->
          let pos = start_idx + i in
          if pos >= 0 && pos < num_coeffs && pos <= end_idx then
            merged.(pos) <- coeff
        ) band
      ) bands freq_bands;
      
      (* Update segment with merged MDCT coefficients, preserving window_data *)
      {
        seg with
        Audiomodel.raw_data = Some [Array.to_list merged];
        num_bands = 1;
        frequency_bands = [];
        (* Preserve window_data, start_sample, end_sample, original_length (it's already there from encoder) *)
        window_data = seg.Audiomodel.window_data;
        start_sample = seg.Audiomodel.start_sample;
        end_sample = seg.Audiomodel.end_sample;
        original_length = seg.Audiomodel.original_length;
      }
    | _ -> seg  (* Mismatch, skip *)
  ) segments

(* Apply final IMDCT to restore window from frequency domain *)
let apply_imdct_final segments =
  List.map (fun seg ->
    match seg.Audiomodel.raw_data with
    | None -> seg  (* No MDCT data, skip *)
    | Some [mdct_coeffs] ->
      (* Apply IMDCT to restore window *)
      let window_data = Dsp.imdct_transform mdct_coeffs in
      (* Update segment with window_data *)
      {
        seg with
        Audiomodel.window_data = Some window_data;
        Audiomodel.raw_data = None;  (* Clear raw_data as we now have window_data *)
        start_sample = seg.Audiomodel.start_sample;
        end_sample = seg.Audiomodel.end_sample;
        original_length = seg.Audiomodel.original_length;
      }
    | Some _ -> seg  (* Unexpected format *)
  ) segments

(* Read AudioFile structure and extract segments *)
let read_audio_file audio_file =
  match audio_file.Audiomodel.tracks with
  | [] -> ([], audio_file.Audiomodel.sample_rate, None, None)
  | track :: _ ->
    let segments = track.Audiomodel.segments in
    let sample_rate = track.Audiomodel.sample_rate in
    let compression_params = track.Audiomodel.compression_params in
    (* Get original_length from first segment if available *)
    let original_length = match segments with
      | seg :: _ -> seg.Audiomodel.original_length
      | [] -> None
    in
    (segments, sample_rate, compression_params, original_length)

(* Decode audio from file and reconstruct signal *)
let decode_from_file input_file =
  (* Read AudioFile from file *)
  let audio_file = Serialization.deserialize_from_file input_file in
  let (segments, sample_rate, compression_params, _original_length) = read_audio_file audio_file in
  
  (* Full decoder pipeline: reverse of encoder *)
  (* segments have quantized second-level MDCT coefficients *)
  (* Step 0: Dequantize data *)
  let dequantized_segments = dequantize_segments segments in
  (* Step 1: IMDCT Level 2 *)
  let imdct2_segments = apply_imdct_level2 dequantized_segments in
  (* Step 2: MDCT Level 1 to bands *)
  let mdct1_segments = apply_mdct_level1_to_bands imdct2_segments in
  (* Step 3: Merge frequency bands *)
  let merged_segments = merge_frequency_bands mdct1_segments in
  (* Step 4: Final IMDCT to restore windows *)
  let window_segments = apply_imdct_final merged_segments in
  (* Step 5: Overlap-add windows *)
  let reconstructed = overlap_add_windows window_segments in
  (* Step 6: Pass-through silence (already lost, so just pass through) *)
  let processed = pass_through_silence reconstructed in
  (* Step 7: Mid-Side → LR (mono, so skip) *)
  let decompressed, _ = mid_side_to_lr processed processed false in
  (* Step 8: Decompress *)
  let final = match compression_params with
  | Some (min_val, max_val) -> decompress_track decompressed min_val max_val
  | None -> decompressed  (* No compression params, assume already decompressed *)
  in
  (final, sample_rate)
