(* Audio model data structures and processing functions *)
open Codec_utilities

(* Window type for audio segments *)
type window_type = 
  | Short
  | Long
  | VeryLong
  | EndFocused
  | StartFocused

(* Segment: represents a windowed segment of audio with frequency bands *)
(* Can store both intermediate data (during encoding) and final data (with bands) *)
type segment = {
  window_type : window_type;
  (* Intermediate encoding data *)
  window_data : float list option;  (* Windowed time-domain data, None if not yet windowed *)
  start_sample : int option;  (* Start sample index in original track, None if not applicable *)
  end_sample : int option;  (* End sample index in original track, None if not applicable *)
  attack_ms : float option;  (* Attack time in ms, None if not calculated *)
  release_ms : float option;  (* Release time in ms, None if not calculated *)
  original_length : int option;  (* Original length before padding, None if not padded *)
  (* Final encoding data *)
  num_bands : int;
  frequency_bands : (float * float) list;  (* Normalized frequency ranges [0.0, 1.0] *)
  band_ranges : (float * float) list;  (* (min, max) for each band *)
  quantization_levels : int list;  (* n for each band *)
  quantized_data : int list list option;  (* Encoded quantized data per band, or None *)
  raw_data : float list list option;  (* Non-quantized data per band, or None *)
}

(* Track: represents a complete audio track *)
type track = {
  num_segments : int;
  segments : segment list;
  length_samples : int;
  sample_rate : int;
  compression_params : (float * float) option;  (* (original_min, original_max) for decompression *)
}

(* AudioFile: represents a complete audio file *)
type audio_file = {
  num_tracks : int;
  tracks : track list;
  bits_per_sample : int;
  sample_rate : int;
}

(* Standard 10-band equalizer frequency boundaries in Hz *)
(* Bands: 20Hz, 60Hz, 170Hz, 310Hz, 600Hz, 1kHz, 3kHz, 6kHz, 12kHz, 14kHz, 20kHz *)
let standard_10band_frequencies_hz = [
  20.0; 60.0; 170.0; 310.0; 600.0; 1000.0; 3000.0; 6000.0; 12000.0; 14000.0; 20000.0
]

(* Polybank filter: splits signal into frequency bands *)
(* Parameters:
   - signal: input signal (list of floats)
   - frequency_boundaries_hz: list of frequency boundaries in Hz
   - sample_rate: sample rate in Hz
   Returns: list of band signals
*)
let polybank_filter signal frequency_boundaries_hz sample_rate =
  if List.length signal = 0 || List.length frequency_boundaries_hz < 2 then []
  else
    (* Normalize frequencies to [0.0, 1.0] range based on Nyquist frequency *)
    let nyquist = float_of_int sample_rate /. 2.0 in
    let normalized_boundaries = List.map (fun f -> f /. nyquist) frequency_boundaries_hz in
    (* Clamp to [0.0, 1.0] *)
    let clamped_boundaries = List.map (fun f -> max 0.0 (min 1.0 f)) normalized_boundaries in
    (* Create frequency band pairs *)
    let rec create_bands acc = function
      | [] -> List.rev acc
      | [_] -> List.rev acc
      | start :: (end_freq :: _ as rest) ->
        create_bands ((start, end_freq) :: acc) rest
    in
    let bands = create_bands [] clamped_boundaries in
    (* Use Dsp.filter_bank_custom to split signal *)
    Dsp.filter_bank_custom signal bands

(* Determine window type based on attack and release times *)
(* Parameters:
   - attack_ms: attack time in milliseconds
   - release_ms: release time in milliseconds
   Returns: appropriate window type
   Uses standard MP3/AAC-like thresholds:
   - EndFocused: attack < 5ms AND release > 20ms
   - StartFocused: attack > 20ms AND release < 5ms
   - VeryLong: attack > 50ms AND release > 50ms
   - Short: attack < 5ms OR release < 5ms (but not EndFocused/StartFocused)
   - Long: default (all other cases)
*)
let determine_window_type attack_ms release_ms =
  if attack_ms < 5.0 && release_ms > 20.0 then
    EndFocused
  else if attack_ms > 20.0 && release_ms < 5.0 then
    StartFocused
  else if attack_ms > 50.0 && release_ms > 50.0 then
    VeryLong
  else if attack_ms < 5.0 || release_ms < 5.0 then
    Short
  else
    Long

(* Find optimal quantization levels using binary search *)
(* Parameters:
   - data: input signal (list of floats)
   - snr_threshold: minimum SNR in dB (default 40.0)
   Returns: (n, min_val, max_val) where n is optimal quantization levels
   Uses binary search to find minimum n that maintains SNR >= threshold
*)
let find_optimal_quantization ?(snr_threshold = 40.0) data =
  if List.length data = 0 then (2, 0.0, 0.0)
  else
    let original_min = List.fold_left min infinity data in
    let original_max = List.fold_left max neg_infinity data in
    if original_max = original_min then
      (* Constant signal, use minimal quantization *)
      (2, original_min, original_max)
    else
      (* Optimize: compute min/max once at the beginning instead of on each iteration *)
      let min_val = original_min in
      let max_val = original_max in
      (* Calculate signal power once for SNR conversion (total power, not average) *)
      let signal_power = List.fold_left (fun acc x -> acc +. x *. x) 0.0 data in
      
      (* Binary search for optimal n in range [2, 256] *)
      (* Optimize: limit search range for very large data, but keep full range for small data *)
      let data_len = List.length data in
      let max_n = if data_len > 10000 then min 256 (max 2 (data_len / 10)) else 256 in
      
      (* Calculate quantization noise for given n *)
      (* Quantization noise power ≈ (step²) / 12 for uniform quantization *)
      (* where step = (max - min) / n *)
      (* Total noise power = (step² / 12) * N, where N is number of samples *)
      let calculate_quantization_noise_power n =
        let range = max_val -. min_val in
        if range = 0.0 then 0.0
        else
          let step = range /. float_of_int n in
          (* MSE of uniform quantization per sample: step² / 12 *)
          (* Total noise power = MSE * number of samples *)
          let mse_per_sample = (step *. step) /. 12.0 in
          mse_per_sample *. float_of_int data_len
      in
      
      (* Convert quantization noise to SNR in dB *)
      (* SNR = 10 * log10(signal_power / noise_power) *)
      (* Add safety margin: theoretical noise may underestimate real noise *)
      (* Use higher noise estimate (multiply by 2.0) for conservative estimate *)
      (* This ensures we select n that will definitely meet the threshold *)
      let quantization_noise_to_snr noise_power =
        if noise_power = 0.0 then infinity
        else if signal_power = 0.0 then neg_infinity
        else
          (* Apply safety margin: use 2.0x noise power for very conservative estimate *)
          (* This accounts for non-uniform signal distribution and other factors *)
          let adjusted_noise = noise_power *. 2.0 in
          10.0 *. Float.log10 (signal_power /. adjusted_noise)
      in
      
      let rec binary_search low high best_n =
        if low > high then
          best_n
        else
          let mid = (low + high) / 2 in
          (* Calculate quantization noise directly without quantizing/dequantizing *)
          let noise_power = calculate_quantization_noise_power mid in
          let snr = quantization_noise_to_snr noise_power in
          if Float.is_infinite snr && snr > 0.0 then
            (* Perfect match, use smallest n *)
            binary_search low (mid - 1) mid
          else if snr >= snr_threshold then
            (* SNR is sufficient, try smaller n *)
            binary_search low (mid - 1) mid
          else
            (* SNR is insufficient, try larger n *)
            binary_search (mid + 1) high best_n
      in
      let optimal_n = binary_search 2 max_n max_n in
      (* Return optimal n with precomputed range *)
      (optimal_n, min_val, max_val)

(* Compress track: linear transformation to use full range [-1.0, 1.0] *)
(* Parameters:
   - data: raw audio samples
   Returns: (compressed_samples, original_min, original_max) for decompression
*)
let compress_track data =
  if List.length data = 0 then ([], 0.0, 0.0)
  else
    let original_min = List.fold_left min infinity data in
    let original_max = List.fold_left max neg_infinity data in
    if original_max = original_min then
      (* Constant signal, map to 0.0 *)
      (List.init (List.length data) (fun _ -> 0.0), original_min, original_max)
    else
      (* Linear transformation: (x - min) / (max - min) * 2.0 - 1.0 *)
      (* This maps [min, max] to [-1.0, 1.0] *)
      let range = original_max -. original_min in
      let compressed = List.map
        (fun x -> ((x -. original_min) /. range) *. 2.0 -. 1.0)
        data
      in
      (compressed, original_min, original_max)

(* Decompress track: inverse linear transformation *)
(* Parameters:
   - compressed_data: compressed samples in [-1.0, 1.0] range
   - original_min: original minimum value
   - original_max: original maximum value
   Returns: decompressed samples
*)
let decompress_track compressed_data original_min original_max =
  if List.length compressed_data = 0 then []
  else if original_max = original_min then
    (* Constant signal, map back to original value *)
    List.init (List.length compressed_data) (fun _ -> original_min)
  else
    (* Inverse transformation: (x + 1.0) / 2.0 * (max - min) + min *)
    (* This maps [-1.0, 1.0] back to [min, max] *)
    let range = original_max -. original_min in
    List.map
      (fun x -> ((x +. 1.0) /. 2.0) *. range +. original_min)
      compressed_data

(* Convert left-right channels to mid-side *)
(* Parameters:
   - left: left channel samples
   - right: right channel samples
   Returns: (mid, side) where mid = (left + right) / 2, side = left - mid
*)
let lr_to_mid_side left right =
  if List.length left <> List.length right then
    failwith "Left and right channels must have same length"
  else
    let mid = List.map2 (fun l r -> (l +. r) /. 2.0) left right in
    let side = List.map2 (fun l m -> l -. m) left mid in
    (mid, side)

(* Convert mid-side channels to left-right *)
(* Parameters:
   - mid: mid channel samples
   - side: side channel samples
   Returns: (left, right) where left = mid + side, right = mid - side
*)
let mid_side_to_lr mid side =
  if List.length mid <> List.length side then
    failwith "Mid and side channels must have same length"
  else
    let left = List.map2 (fun m s -> m +. s) mid side in
    let right = List.map2 (fun m s -> m -. s) mid side in
    (left, right)

(* Get window size based on window type *)
let get_window_size window_type =
  match window_type with
  | Short -> 512
  | Long -> 2048
  | VeryLong -> 4096
  | EndFocused -> 1024
  | StartFocused -> 1024

(* Get windowing function based on window type *)
let get_windowing_function window_type =
  match window_type with
  | Short -> Dsp.hamming_window
  | Long -> Dsp.hanning_window
  | VeryLong -> Dsp.hanning_window
  | EndFocused -> Dsp.hamming_window
  | StartFocused -> Dsp.hamming_window

(* Segment information for intermediate encoding stages *)
type segment_info = {
  start_sample : int;
  end_sample : int;
  attack_ms : float;
  release_ms : float;
  window_type : window_type;
  original_length : int option;  (* None if not padded, Some original_length if padded *)
}

(* Adaptive segmentation of track with attack/release detection *)
let segment_track ?(min_segment_length = 512) sample_rate data =
  let n = List.length data in
  if n = 0 then ([], [])
  else
    (* Check if track is too short *)
    let (padded_data, original_length) = 
      if n < min_segment_length then
        (* Pad with zeros - use efficient Array-based approach *)
        let padded_array = Array.make min_segment_length 0.0 in
        List.iteri (fun i x -> padded_array.(i) <- x) data;
        let padded = Array.to_list padded_array in
        (padded, Some n)
      else
        (data, None)
    in
    let padded_n = List.length padded_data in
    
    (* For now, create a single segment covering the entire track *)
    (* In a full implementation, this would analyze the signal and create multiple segments *)
    let window_size = min 1024 (padded_n / 4) in
    let envelope = Codec_utilities.Audio_analysis.calculate_envelope padded_data window_size in
    let attack_ms, release_ms = Codec_utilities.Audio_analysis.calculate_attack_release envelope sample_rate in
    let window_type = determine_window_type attack_ms release_ms in
    
    let segment = {
      start_sample = 0;
      end_sample = padded_n - 1;
      attack_ms;
      release_ms;
      window_type;
      original_length;
    } in
    (padded_data, [segment])

(* Create overlapping windows (50% overlap) with adaptive windowing *)
let create_overlapping_windows data segments _sample_rate =
  let data_array = Array.of_list data in
  let n = Array.length data_array in
  
  let create_windows_for_segment (seg_info : segment_info) =
    let window_size = get_window_size seg_info.window_type in
    let hop_size = window_size / 2 in  (* 50% overlap *)
    let start = seg_info.start_sample in
    let stop = seg_info.end_sample in
    
    let rec create_windows idx windows =
      if idx + window_size > n || idx > stop then List.rev windows
      else
        let window_end = min (idx + window_size) n in
        let window_samples = Array.sub data_array idx (window_end - idx) in
        let window_list = Array.to_list window_samples in
        (* Apply windowing function *)
        let window_func = get_windowing_function seg_info.window_type in
        let windowed = window_func window_list in
        
        (* Create segment with intermediate data *)
        let window_seg = {
          window_type = seg_info.window_type;
          window_data = Some windowed;
          start_sample = Some idx;
          end_sample = Some (window_end - 1);  (* Use actual window_end, not idx + window_size *)
          attack_ms = Some seg_info.attack_ms;
          release_ms = Some seg_info.release_ms;
          original_length = seg_info.original_length;
          num_bands = 0;
          frequency_bands = [];
          band_ranges = [];
          quantization_levels = [];
          quantized_data = None;
          raw_data = None;
        } in
        create_windows (idx + hop_size) (window_seg :: windows)
    in
    create_windows start []
  in
  List.concat (List.map create_windows_for_segment segments)

