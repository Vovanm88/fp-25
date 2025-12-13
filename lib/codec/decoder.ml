open Codec_utilities

let decompress_track compressed_data original_min original_max =
  Audiomodel.decompress_track compressed_data original_min original_max

let decompress_stereo_tracks compressed_mid compressed_side min_mid max_mid
    min_side max_side =
  let decompressed_mid = decompress_track compressed_mid min_mid max_mid in
  let decompressed_side = decompress_track compressed_side min_side max_side in
  (decompressed_mid, decompressed_side)

let mid_side_to_lr mid side is_stereo =
  if is_stereo then Audiomodel.mid_side_to_lr mid side else (mid, side)

let pass_through_silence data = data

let overlap_add_windows windows =
  if List.length windows = 0 then []
  else
    let windows_with_data =
      List.filter_map
        (fun win ->
          match
            ( win.Audiomodel.window_data,
              win.Audiomodel.start_sample,
              win.Audiomodel.end_sample )
          with
          | Some data, Some start, Some end_pos ->
              Some (data, start, end_pos, win.Audiomodel.original_length)
          | _ -> None)
        windows
    in

    if List.length windows_with_data = 0 then []
    else
      let max_end =
        List.fold_left
          (fun acc (_, _, end_pos, _) -> max acc (end_pos + 1))
          0 windows_with_data
      in

      let original_length =
        match List.hd windows_with_data with
        | _, _, _, Some len -> Some len
        | _ -> None
      in

      let output_length = max_end in

      let output = Array.make output_length 0.0 in

      List.iter
        (fun (window_data, start_pos, _end_pos, _) ->
          List.iteri
            (fun i sample ->
              let pos = start_pos + i in
              if pos < output_length then output.(pos) <- output.(pos) +. sample)
            window_data)
        windows_with_data;

      let result = Array.to_list output in
      match original_length with
      | Some len when len < output_length ->
          let rec take n = function
            | [] -> []
            | x :: xs -> if n <= 0 then [] else x :: take (n - 1) xs
          in
          take len result
      | _ -> result

let dequantize_segments segments =
  List.map
    (fun seg ->
      match seg.Audiomodel.quantized_data with
      | None -> seg
      | Some quantized_bands ->
          let num_bands = List.length quantized_bands in
          let num_levels = List.length seg.Audiomodel.quantization_levels in
          let num_ranges = List.length seg.Audiomodel.band_ranges in
          if num_bands = 0 || num_levels = 0 || num_ranges = 0 then seg
          else if num_bands <> num_levels || num_bands <> num_ranges then seg
          else
            let dequantized_bands =
              let rec dequantize_all quant_bands quant_levels ranges acc =
                match (quant_bands, quant_levels, ranges) with
                | [], [], [] -> List.rev acc
                | [], _, _ -> List.rev acc
                | _, [], _ -> List.rev acc
                | _, _, [] -> List.rev acc
                | qb :: qbs, ql :: qls, (min_val, max_val) :: rs -> (
                    if ql <= 0 then dequantize_all qbs qls rs ([] :: acc)
                    else
                      try
                        let deq =
                          Quantization.dequantize qb ql min_val max_val
                        in
                        dequantize_all qbs qls rs (deq :: acc)
                      with Failure _ -> dequantize_all qbs qls rs ([] :: acc))
              in
              dequantize_all quantized_bands seg.Audiomodel.quantization_levels
                seg.Audiomodel.band_ranges []
            in

            {
              seg with
              Audiomodel.raw_data = Some dequantized_bands;
              quantized_data = None;
              window_data = seg.Audiomodel.window_data;
              start_sample = seg.Audiomodel.start_sample;
              end_sample = seg.Audiomodel.end_sample;
              original_length = seg.Audiomodel.original_length;
              frequency_bands = seg.Audiomodel.frequency_bands;
            })
    segments

let apply_imdct_level2 segments =
  List.map
    (fun seg ->
      match seg.Audiomodel.raw_data with
      | None -> seg
      | Some mdct_level2_bands ->
          let time_domain_bands =
            List.map Dsp.imdct_transform mdct_level2_bands
          in

          {
            seg with
            Audiomodel.raw_data = Some time_domain_bands;
            window_data = seg.Audiomodel.window_data;
            start_sample = seg.Audiomodel.start_sample;
            end_sample = seg.Audiomodel.end_sample;
            original_length = seg.Audiomodel.original_length;
            frequency_bands = seg.Audiomodel.frequency_bands;
          })
    segments

let apply_mdct_level1_to_bands segments =
  List.map
    (fun seg ->
      match seg.Audiomodel.raw_data with
      | None -> seg
      | Some time_domain_bands ->
          let mdct_level1_bands =
            List.map Dsp.mdct_transform time_domain_bands
          in

          {
            seg with
            Audiomodel.raw_data = Some mdct_level1_bands;
            window_data = seg.Audiomodel.window_data;
            start_sample = seg.Audiomodel.start_sample;
            end_sample = seg.Audiomodel.end_sample;
            original_length = seg.Audiomodel.original_length;
            frequency_bands = seg.Audiomodel.frequency_bands;
          })
    segments

let merge_frequency_bands segments =
  List.map
    (fun seg ->
      match (seg.Audiomodel.raw_data, seg.Audiomodel.frequency_bands) with
      | None, _ -> seg
      | Some bands, freq_bands when List.length bands = List.length freq_bands
        ->
          let num_coeffs =
            match seg.Audiomodel.window_data with
            | Some window_data -> List.length window_data / 2
            | None -> Audiomodel.get_window_size seg.Audiomodel.window_type / 2
          in

          let merged = Array.make num_coeffs 0.0 in

          List.iter2
            (fun band (start_freq, end_freq) ->
              let start_idx =
                int_of_float (start_freq *. float_of_int num_coeffs)
              in
              let end_idx =
                min (num_coeffs - 1)
                  (int_of_float (end_freq *. float_of_int num_coeffs))
              in

              List.iteri
                (fun i coeff ->
                  let pos = start_idx + i in
                  if pos >= 0 && pos < num_coeffs && pos <= end_idx then
                    merged.(pos) <- coeff)
                band)
            bands freq_bands;

          {
            seg with
            Audiomodel.raw_data = Some [ Array.to_list merged ];
            num_bands = 1;
            frequency_bands = [];
            window_data = seg.Audiomodel.window_data;
            start_sample = seg.Audiomodel.start_sample;
            end_sample = seg.Audiomodel.end_sample;
            original_length = seg.Audiomodel.original_length;
          }
      | _ -> seg)
    segments

let apply_imdct_final segments =
  List.map
    (fun seg ->
      match seg.Audiomodel.raw_data with
      | None -> seg
      | Some [ mdct_coeffs ] ->
          let window_data = Dsp.imdct_transform mdct_coeffs in

          {
            seg with
            Audiomodel.window_data = Some window_data;
            Audiomodel.raw_data = None;
            start_sample = seg.Audiomodel.start_sample;
            end_sample = seg.Audiomodel.end_sample;
            original_length = seg.Audiomodel.original_length;
          }
      | Some _ -> seg)
    segments

let read_audio_file audio_file =
  match audio_file.Audiomodel.tracks with
  | [] -> ([], audio_file.Audiomodel.sample_rate, None, None)
  | track :: _ ->
      let segments = track.Audiomodel.segments in
      let sample_rate = track.Audiomodel.sample_rate in
      let compression_params = track.Audiomodel.compression_params in

      let original_length =
        match segments with
        | seg :: _ -> seg.Audiomodel.original_length
        | [] -> None
      in
      (segments, sample_rate, compression_params, original_length)

let decode_from_file input_file =
  let audio_file = Serialization.deserialize_from_file input_file in
  let segments, sample_rate, compression_params, _original_length =
    read_audio_file audio_file
  in

  let dequantized_segments = dequantize_segments segments in

  let imdct2_segments = apply_imdct_level2 dequantized_segments in

  let mdct1_segments = apply_mdct_level1_to_bands imdct2_segments in

  let merged_segments = merge_frequency_bands mdct1_segments in

  let window_segments = apply_imdct_final merged_segments in

  let reconstructed = overlap_add_windows window_segments in

  let processed = pass_through_silence reconstructed in

  let decompressed, _ = mid_side_to_lr processed processed false in

  let final =
    match compression_params with
    | Some (min_val, max_val) -> decompress_track decompressed min_val max_val
    | None -> decompressed
  in
  (final, sample_rate)
