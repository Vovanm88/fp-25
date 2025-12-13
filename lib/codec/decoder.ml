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
        (* Preserve window_data for merge_frequency_bands *)
        window_data = seg.Audiomodel.window_data;
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
        (* Preserve window_data for merge_frequency_bands *)
        window_data = seg.Audiomodel.window_data;
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
      (* Method 1: If we have window_data, MDCT output size = window_size / 2 *)
      let num_coeffs = match seg.Audiomodel.window_data with
      | Some window_data -> 
        (* MDCT transforms n samples into n/2 coefficients *)
        List.length window_data / 2
      | None ->
        (* Method 2: Find num_coeffs by checking which value makes all bands fit correctly *)
        (* For each band with (start_freq, end_freq) and size N: *)
        (* start_idx = int_of_float (start_freq * num_coeffs) *)
        (* end_idx = int_of_float (end_freq * num_coeffs) *)
        (* N should equal end_idx - start_idx + 1 *)
        let total_band_size = List.fold_left (fun acc band -> acc + List.length band) 0 bands in
        (* Start with minimum estimate based on total band size *)
        let min_estimate = total_band_size in
        (* Also try estimates from individual bands *)
        let band_estimates = List.map2 (fun band (start_freq, end_freq) ->
          let band_size = float_of_int (List.length band) in
          let freq_range = end_freq -. start_freq in
          if freq_range > 0.001 then  (* Avoid division by zero *)
            int_of_float (band_size /. freq_range)
          else
            List.length band
        ) bands freq_bands in
        let max_band_estimate = List.fold_left max 0 band_estimates in
        let start_estimate = max min_estimate max_band_estimate in
        (* Find the smallest num_coeffs that makes all bands fit correctly *)
        (* Optimize: use binary search or limit iterations *)
        (* Since window_data should always be available, this should rarely be called *)
        (* But if it is, we'll use a smarter approach: try only likely candidates *)
        let rec find_correct_num_coeffs candidate iterations =
          if iterations > 1000 then 
            (* Limit iterations to avoid slow performance *)
            (* Fallback: use maximum estimate if search fails *)
            max_band_estimate
          else if candidate > 100000 then 
            max_band_estimate
          else
            (* Check if this candidate works for all bands *)
            let all_match = try
              List.for_all2 (fun band (start_freq, end_freq) ->
                let start_idx = int_of_float (start_freq *. float_of_int candidate) in
                let end_idx = min (candidate - 1) (int_of_float (end_freq *. float_of_int candidate)) in
                let expected_size = end_idx - start_idx + 1 in
                expected_size = List.length band
              ) bands freq_bands
            with _ -> false
            in
            if all_match then candidate
            else find_correct_num_coeffs (candidate + 1) (iterations + 1)
        in
        find_correct_num_coeffs start_estimate 0
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
          if pos <= end_idx && pos < num_coeffs then
            merged.(pos) <- coeff
        ) band
      ) bands freq_bands;
      
      (* Update segment with merged MDCT coefficients, preserving window_data *)
      {
        seg with
        Audiomodel.raw_data = Some [Array.to_list merged];
        num_bands = 1;
        frequency_bands = [];
        (* Preserve window_data (it's already there from encoder) *)
        window_data = seg.Audiomodel.window_data;
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
  (* segments have second-level MDCT coefficients in raw_data *)
  (* Step 1: IMDCT Level 2 *)
  let imdct2_segments = apply_imdct_level2 segments in
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
