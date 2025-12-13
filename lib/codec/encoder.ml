open Codec_utilities
module Audio_analysis = Codec_utilities.Audio_analysis

let compress_track data =
  let compressed, original_min, original_max = Audiomodel.compress_track data in

  let decompressed =
    Audiomodel.decompress_track compressed original_min original_max
  in
  let snr_db = Dsp.snr data decompressed in
  (compressed, original_min, original_max, snr_db)

let compress_stereo_tracks left right =
  let compressed_left, min_l, max_l, snr_l = compress_track left in
  let compressed_right, min_r, max_r, snr_r = compress_track right in

  let snr_avg = (snr_l +. snr_r) /. 2.0 in
  ((compressed_left, compressed_right), (min_l, max_l), (min_r, max_r), snr_avg)

let lr_to_mid_side left right =
  let is_mono =
    List.length left = List.length right
    && List.for_all2 (fun l r -> abs_float (l -. r) < 1e-10) left right
  in
  if is_mono then (left, right, false)
  else
    let mid, side = Audiomodel.lr_to_mid_side left right in
    (mid, side, true)

let replace_silence_after_loud ?(window_size = 100) ?(loud_threshold = 0.1)
    ?(quiet_threshold = 0.01) ?(quiet_duration_ms = 50.0) sample_rate data =
  let n = List.length data in
  if n = 0 then ([], [])
  else
    let data_array = Array.of_list data in
    let quiet_duration_samples =
      int_of_float (quiet_duration_ms *. float_of_int sample_rate /. 1000.0)
    in

    let rms_array = Array.make n 0.0 in
    for i = 0 to n - 1 do
      let window_start = max 0 (i - (window_size / 2)) in
      let window_end = min n (i + (window_size / 2)) in
      let window =
        Array.sub data_array window_start (window_end - window_start)
      in
      rms_array.(i) <- Audio_analysis.calculate_rms (Array.to_list window)
    done;

    let mask = Array.make n false in
    let i = ref 0 in
    while !i < n do
      let rms = rms_array.(!i) in
      if rms >= loud_threshold then (
        let j = ref (!i + 1) in
        let quiet_start = ref (-1) in
        while !j < n do
          let rms_j = rms_array.(!j) in
          if rms_j <= quiet_threshold then (
            if !quiet_start < 0 then quiet_start := !j;
            let quiet_duration = !j - !quiet_start + 1 in
            if quiet_duration >= quiet_duration_samples then
              for k = !quiet_start to !j do
                mask.(k) <- true
              done)
          else if rms_j >= loud_threshold then j := n
          else quiet_start := -1;
          j := !j + 1
        done;

        while !i < n && rms_array.(!i) >= loud_threshold do
          i := !i + 1
        done)
      else i := !i + 1
    done;

    let processed =
      Array.mapi (fun i sample -> if mask.(i) then 0.0 else sample) data_array
    in
    (Array.to_list processed, Array.to_list mask)

type segment_info = Audiomodel.segment_info
type segment = Audiomodel.segment

let segment_track = Audiomodel.segment_track
let create_overlapping_windows = Audiomodel.create_overlapping_windows

let apply_mdct_level1 segments =
  List.map
    (fun seg ->
      match seg.Audiomodel.window_data with
      | None -> seg
      | Some window_data ->
          let mdct_coeffs = Dsp.mdct_transform window_data in

          let updated_seg =
            {
              seg with
              Audiomodel.raw_data = Some [ mdct_coeffs ];
              num_bands = 1;
              window_data = seg.Audiomodel.window_data;
            }
          in
          updated_seg)
    segments

let split_frequency_bands
    ?(frequency_boundaries_hz = Audiomodel.standard_10band_frequencies_hz)
    sample_rate segments =
  let nyquist = float_of_int sample_rate /. 2.0 in

  let normalized_boundaries =
    List.map (fun f -> max 0.0 (min 1.0 (f /. nyquist))) frequency_boundaries_hz
  in

  let rec create_band_pairs acc = function
    | [] -> List.rev acc
    | [ _ ] -> List.rev acc
    | start :: (end_freq :: _ as rest) ->
        create_band_pairs ((start, end_freq) :: acc) rest
  in
  let band_pairs = create_band_pairs [] normalized_boundaries in

  List.map
    (fun seg ->
      match seg.Audiomodel.raw_data with
      | None -> seg
      | Some [ mdct_coeffs ] ->
          let num_coeffs = List.length mdct_coeffs in
          if num_coeffs = 0 then seg
          else
            let split_band (start_freq, end_freq) =
              let start_idx =
                int_of_float (start_freq *. float_of_int num_coeffs)
              in
              let end_idx =
                min (num_coeffs - 1)
                  (int_of_float (end_freq *. float_of_int num_coeffs))
              in
              if start_idx > end_idx then []
              else
                let coeffs_array = Array.of_list mdct_coeffs in
                Array.to_list
                  (Array.sub coeffs_array start_idx (end_idx - start_idx + 1))
            in
            let band_coeffs = List.map split_band band_pairs in

            let valid_bands =
              List.filter (fun band -> List.length band > 0) band_coeffs
            in
            let valid_band_pairs =
              List.filter
                (fun (s, e) ->
                  let start_idx = int_of_float (s *. float_of_int num_coeffs) in
                  let end_idx =
                    min (num_coeffs - 1)
                      (int_of_float (e *. float_of_int num_coeffs))
                  in
                  start_idx <= end_idx)
                band_pairs
            in

            {
              seg with
              Audiomodel.raw_data = Some valid_bands;
              num_bands = List.length valid_bands;
              frequency_bands = valid_band_pairs;
              window_data = seg.Audiomodel.window_data;
              start_sample = seg.Audiomodel.start_sample;
              end_sample = seg.Audiomodel.end_sample;
              original_length = seg.Audiomodel.original_length;
            }
      | Some _ -> seg)
    segments

let apply_imdct_to_bands segments =
  List.map
    (fun seg ->
      match seg.Audiomodel.raw_data with
      | None -> seg
      | Some bands ->
          let time_domain_bands = List.map Dsp.imdct_transform bands in

          {
            seg with
            Audiomodel.raw_data = Some time_domain_bands;
            window_data = seg.Audiomodel.window_data;
            start_sample = seg.Audiomodel.start_sample;
            end_sample = seg.Audiomodel.end_sample;
            original_length = seg.Audiomodel.original_length;
          })
    segments

let apply_mdct_level2 segments =
  List.map
    (fun seg ->
      match seg.Audiomodel.raw_data with
      | None -> seg
      | Some time_domain_bands ->
          let mdct_level2_bands =
            List.map Dsp.mdct_transform time_domain_bands
          in

          {
            seg with
            Audiomodel.raw_data = Some mdct_level2_bands;
            window_data = seg.Audiomodel.window_data;
            start_sample = seg.Audiomodel.start_sample;
            end_sample = seg.Audiomodel.end_sample;
            original_length = seg.Audiomodel.original_length;
          })
    segments

let select_quantization_thresholds ?(snr_threshold_db = 40.0) segments =
  List.map
    (fun seg ->
      match seg.Audiomodel.raw_data with
      | None -> seg
      | Some mdct_level2_bands ->
          let results =
            List.map
              (fun band_coeffs ->
                let n, min_val, max_val =
                  Audiomodel.find_optimal_quantization
                    ~snr_threshold:snr_threshold_db band_coeffs
                in

                let quantized =
                  Quantization.quantize ~min_val ~max_val band_coeffs n
                in
                (n, (min_val, max_val), quantized))
              mdct_level2_bands
          in

          let valid_results = List.filter (fun (n, _, _) -> n > 0) results in
          if List.length valid_results = 0 then seg
          else
            let quantization_levels =
              List.map (fun (n, _, _) -> n) valid_results
            in
            let band_ranges =
              List.map (fun (_, range, _) -> range) valid_results
            in
            let quantized_data = List.map (fun (_, _, q) -> q) valid_results in

            {
              seg with
              quantization_levels;
              band_ranges;
              quantized_data = Some quantized_data;
              raw_data = None;
              window_data = seg.Audiomodel.window_data;
              start_sample = seg.Audiomodel.start_sample;
              end_sample = seg.Audiomodel.end_sample;
              original_length = seg.Audiomodel.original_length;
            })
    segments

let create_audio_file ?(bits_per_sample = 16) ?(compression_params = None)
    ?(original_length = None) sample_rate segments =
  let total_length =
    match original_length with
    | Some len -> len
    | None ->
        let max_end =
          List.fold_left
            (fun (acc : int) (seg : Audiomodel.segment) ->
              let end_pos : int =
                match seg.Audiomodel.end_sample with
                | Some pos -> pos
                | None -> 0
              in
              max acc (end_pos + 1))
            0 segments
        in
        max_end
  in

  let track =
    {
      Audiomodel.num_segments = List.length segments;
      segments;
      length_samples = total_length;
      sample_rate;
      compression_params;
    }
  in

  {
    Audiomodel.num_tracks = 1;
    tracks = [ track ];
    bits_per_sample;
    sample_rate;
  }

let encode_to_file ?(snr_threshold_db = 40.0) input sample_rate output_file =
  let compressed, min_val, max_val, _snr = compress_track input in
  let mid, _side, _is_stereo = lr_to_mid_side compressed compressed in
  let processed, _silence_mask = replace_silence_after_loud sample_rate mid in
  let segmented, segments_info = segment_track sample_rate processed in
  let windows =
    create_overlapping_windows segmented segments_info sample_rate
  in
  let mdct_segments = apply_mdct_level1 windows in
  let band_segments = split_frequency_bands sample_rate mdct_segments in
  let imdct_segments = apply_imdct_to_bands band_segments in
  let mdct2_segments = apply_mdct_level2 imdct_segments in
  let quant_segments =
    select_quantization_thresholds ~snr_threshold_db mdct2_segments
  in

  let audio_file =
    create_audio_file
      ~compression_params:(Some (min_val, max_val))
      sample_rate quant_segments
  in

  Serialization.serialize_to_file output_file audio_file;
  audio_file
