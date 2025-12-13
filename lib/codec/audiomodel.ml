open Codec_utilities

type window_type = Short | Long | VeryLong | EndFocused | StartFocused

type segment = {
  window_type : window_type;
  window_data : float list option;
  start_sample : int option;
  end_sample : int option;
  attack_ms : float option;
  release_ms : float option;
  original_length : int option;
  num_bands : int;
  frequency_bands : (float * float) list;
  band_ranges : (float * float) list;
  quantization_levels : int list;
  quantized_data : int list list option;
  raw_data : float list list option;
}

type track = {
  num_segments : int;
  segments : segment list;
  length_samples : int;
  sample_rate : int;
  compression_params : (float * float) option;
}

type audio_file = {
  num_tracks : int;
  tracks : track list;
  bits_per_sample : int;
  sample_rate : int;
}

let standard_10band_frequencies_hz =
  [
    20.0;
    60.0;
    170.0;
    310.0;
    600.0;
    1000.0;
    3000.0;
    6000.0;
    12000.0;
    14000.0;
    20000.0;
  ]

let polybank_filter signal frequency_boundaries_hz sample_rate =
  if List.length signal = 0 || List.length frequency_boundaries_hz < 2 then []
  else
    let nyquist = float_of_int sample_rate /. 2.0 in
    let normalized_boundaries =
      List.map (fun f -> f /. nyquist) frequency_boundaries_hz
    in

    let clamped_boundaries =
      List.map (fun f -> max 0.0 (min 1.0 f)) normalized_boundaries
    in

    let rec create_bands acc = function
      | [] -> List.rev acc
      | [ _ ] -> List.rev acc
      | start :: (end_freq :: _ as rest) ->
          create_bands ((start, end_freq) :: acc) rest
    in
    let bands = create_bands [] clamped_boundaries in

    Dsp.filter_bank_custom signal bands

let determine_window_type attack_ms release_ms =
  if attack_ms < 5.0 && release_ms > 20.0 then EndFocused
  else if attack_ms > 20.0 && release_ms < 5.0 then StartFocused
  else if attack_ms > 50.0 && release_ms > 50.0 then VeryLong
  else if attack_ms < 5.0 || release_ms < 5.0 then Short
  else Long

let find_optimal_quantization ?(snr_threshold = 40.0) data =
  if List.length data = 0 then (2, 0.0, 0.0)
  else
    let original_min = List.fold_left min infinity data in
    let original_max = List.fold_left max neg_infinity data in
    if original_max = original_min then (2, original_min, original_max)
    else
      let min_val = original_min in
      let max_val = original_max in

      let signal_power =
        List.fold_left (fun acc x -> acc +. (x *. x)) 0.0 data
      in

      let data_len = List.length data in
      let max_n =
        if data_len > 10000 then min 256 (max 2 (data_len / 10)) else 256
      in

      let calculate_quantization_noise_power n =
        let range = max_val -. min_val in
        if range = 0.0 then 0.0
        else
          let step = range /. float_of_int n in

          let mse_per_sample = step *. step /. 12.0 in
          mse_per_sample *. float_of_int data_len
      in

      let quantization_noise_to_snr noise_power =
        if noise_power = 0.0 then infinity
        else if signal_power = 0.0 then neg_infinity
        else
          let adjusted_noise = noise_power *. 2.0 in
          10.0 *. Float.log10 (signal_power /. adjusted_noise)
      in

      let rec binary_search low high best_n =
        if low > high then best_n
        else
          let mid = (low + high) / 2 in

          let noise_power = calculate_quantization_noise_power mid in
          let snr = quantization_noise_to_snr noise_power in
          if Float.is_infinite snr && snr > 0.0 then
            binary_search low (mid - 1) mid
          else if snr >= snr_threshold then binary_search low (mid - 1) mid
          else binary_search (mid + 1) high best_n
      in
      let optimal_n = binary_search 2 max_n max_n in

      (optimal_n, min_val, max_val)

let compress_track data =
  if List.length data = 0 then ([], 0.0, 0.0)
  else
    let original_min = List.fold_left min infinity data in
    let original_max = List.fold_left max neg_infinity data in
    if original_max = original_min then
      (List.init (List.length data) (fun _ -> 0.0), original_min, original_max)
    else
      let range = original_max -. original_min in
      let compressed =
        List.map (fun x -> ((x -. original_min) /. range *. 2.0) -. 1.0) data
      in
      (compressed, original_min, original_max)

let decompress_track compressed_data original_min original_max =
  if List.length compressed_data = 0 then []
  else if original_max = original_min then
    List.init (List.length compressed_data) (fun _ -> original_min)
  else
    let range = original_max -. original_min in
    List.map
      (fun x -> ((x +. 1.0) /. 2.0 *. range) +. original_min)
      compressed_data

let lr_to_mid_side left right =
  if List.length left <> List.length right then
    failwith "Left and right channels must have same length"
  else
    let mid = List.map2 (fun l r -> (l +. r) /. 2.0) left right in
    let side = List.map2 (fun l m -> l -. m) left mid in
    (mid, side)

let mid_side_to_lr mid side =
  if List.length mid <> List.length side then
    failwith "Mid and side channels must have same length"
  else
    let left = List.map2 (fun m s -> m +. s) mid side in
    let right = List.map2 (fun m s -> m -. s) mid side in
    (left, right)

let get_window_size window_type =
  match window_type with
  | Short -> 512
  | Long -> 2048
  | VeryLong -> 4096
  | EndFocused -> 1024
  | StartFocused -> 1024

let get_windowing_function window_type =
  match window_type with
  | Short -> Dsp.hamming_window
  | Long -> Dsp.hanning_window
  | VeryLong -> Dsp.hanning_window
  | EndFocused -> Dsp.hamming_window
  | StartFocused -> Dsp.hamming_window

type segment_info = {
  start_sample : int;
  end_sample : int;
  attack_ms : float;
  release_ms : float;
  window_type : window_type;
  original_length : int option;
}

let segment_track ?(min_segment_length = 512) sample_rate data =
  let n = List.length data in
  if n = 0 then ([], [])
  else
    let padded_data, original_length =
      if n < min_segment_length then (
        let padded_array = Array.make min_segment_length 0.0 in
        List.iteri (fun i x -> padded_array.(i) <- x) data;
        let padded = Array.to_list padded_array in
        (padded, Some n))
      else (data, None)
    in
    let padded_n = List.length padded_data in

    let window_size = min 1024 (padded_n / 4) in
    let hop_size = max 1 (window_size / 4) in
    let envelope =
      Codec_utilities.Audio_analysis.calculate_envelope padded_data window_size
    in
    let attack_ms, release_ms =
      Codec_utilities.Audio_analysis.calculate_attack_release envelope
        sample_rate hop_size
    in
    let window_type = determine_window_type attack_ms release_ms in

    let segment =
      {
        start_sample = 0;
        end_sample = padded_n - 1;
        attack_ms;
        release_ms;
        window_type;
        original_length;
      }
    in
    (padded_data, [ segment ])

let create_overlapping_windows data segments _sample_rate =
  let data_array = Array.of_list data in
  let n = Array.length data_array in

  let create_windows_for_segment (seg_info : segment_info) =
    let window_size = get_window_size seg_info.window_type in
    let hop_size = window_size / 2 in
    let start = seg_info.start_sample in
    let stop = seg_info.end_sample in

    let rec create_windows idx windows =
      if idx + window_size > n || idx > stop then List.rev windows
      else
        let window_end = min (idx + window_size) n in
        let window_samples = Array.sub data_array idx (window_end - idx) in
        let window_list = Array.to_list window_samples in

        let window_func = get_windowing_function seg_info.window_type in
        let windowed = window_func window_list in

        let window_seg =
          {
            window_type = seg_info.window_type;
            window_data = Some windowed;
            start_sample = Some idx;
            end_sample = Some (window_end - 1);
            attack_ms = Some seg_info.attack_ms;
            release_ms = Some seg_info.release_ms;
            original_length = seg_info.original_length;
            num_bands = 0;
            frequency_bands = [];
            band_ranges = [];
            quantization_levels = [];
            quantized_data = None;
            raw_data = None;
          }
        in
        create_windows (idx + hop_size) (window_seg :: windows)
    in
    create_windows start []
  in
  List.concat (List.map create_windows_for_segment segments)
