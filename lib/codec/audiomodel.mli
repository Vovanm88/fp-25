(* Audio model data structures and processing functions *)

(* Window type for audio segments *)
type window_type = Short | Long | VeryLong | EndFocused | StartFocused

(* Segment: represents a windowed segment of audio with frequency bands *)
(* Can store both intermediate data (during encoding) and final data (with bands) *)
type segment = {
  window_type : window_type;
  (* Intermediate encoding data *)
  window_data : float list option;
      (* Windowed time-domain data, None if not yet windowed *)
  start_sample : int option;
      (* Start sample index in original track, None if not applicable *)
  end_sample : int option;
      (* End sample index in original track, None if not applicable *)
  attack_ms : float option; (* Attack time in ms, None if not calculated *)
  release_ms : float option; (* Release time in ms, None if not calculated *)
  original_length : int option;
  (* Original length before padding, None if not padded *)
  (* Final encoding data *)
  num_bands : int;
  frequency_bands : (float * float) list;
      (* Normalized frequency ranges [0.0, 1.0] *)
  band_ranges : (float * float) list; (* (min, max) for each band *)
  quantization_levels : int list; (* n for each band *)
  quantized_data : int list list option;
      (* Encoded quantized data per band, or None *)
  raw_data : float list list option; (* Non-quantized data per band, or None *)
}

(* Segment information for intermediate encoding stages *)
type segment_info = {
  start_sample : int;
  end_sample : int;
  attack_ms : float;
  release_ms : float;
  window_type : window_type;
  original_length : int option;
      (* None if not padded, Some original_length if padded *)
}

(* Get window size based on window type *)
val get_window_size : window_type -> int

(* Get windowing function based on window type *)
val get_windowing_function : window_type -> float list -> float list

(* Adaptive segmentation of track with attack/release detection *)
(* data: input signal *)
(* sample_rate: sample rate in Hz *)
(* min_segment_length: minimum segment length in samples (default: 512) *)
(* Returns: (segmented_data, segments_info) where segmented_data may be padded with zeros *)
val segment_track :
  ?min_segment_length:int -> int -> float list -> float list * segment_info list

(* Create overlapping windows (50% overlap) with adaptive windowing *)
(* data: input signal *)
(* segments: segment information *)
(* sample_rate: sample rate in Hz *)
(* Returns: list of segments with window_data filled *)
val create_overlapping_windows :
  float list -> segment_info list -> int -> segment list

(* Track: represents a complete audio track *)
type track = {
  num_segments : int;
  segments : segment list;
  length_samples : int;
  sample_rate : int;
  compression_params : (float * float) option;
      (* (original_min, original_max) for decompression *)
}

(* AudioFile: represents a complete audio file *)
type audio_file = {
  num_tracks : int;
  tracks : track list;
  bits_per_sample : int;
  sample_rate : int;
}

(* Standard 10-band equalizer frequency boundaries in Hz *)
val standard_10band_frequencies_hz : float list

(* Polybank filter: splits signal into frequency bands *)
(* signal: input signal, frequency_boundaries_hz: frequency boundaries in Hz, sample_rate: sample rate in Hz *)
val polybank_filter : float list -> float list -> int -> float list list

(* Determine window type based on attack and release times *)
(* attack_ms: attack time in milliseconds, release_ms: release time in milliseconds *)
val determine_window_type : float -> float -> window_type

(* Find optimal quantization levels using binary search *)
(* data: input signal, ?snr_threshold: minimum SNR in dB (default 40.0) *)
(* Returns: (n, min_val, max_val) where n is optimal quantization levels *)
val find_optimal_quantization :
  ?snr_threshold:float -> float list -> int * float * float

(* Compress track: linear transformation to use full range [-1.0, 1.0] *)
(* Returns: (compressed_samples, original_min, original_max) *)
val compress_track : float list -> float list * float * float

(* Decompress track: inverse linear transformation *)
(* compressed_data: samples in [-1.0, 1.0], original_min/max: original range *)
val decompress_track : float list -> float -> float -> float list

(* Convert left-right channels to mid-side *)
(* left: left channel, right: right channel *)
(* Returns: (mid, side) where mid = (left + right) / 2, side = left - mid *)
val lr_to_mid_side : float list -> float list -> float list * float list

(* Convert mid-side channels to left-right *)
(* mid: mid channel, side: side channel *)
(* Returns: (left, right) where left = mid + side, right = mid - side *)
val mid_side_to_lr : float list -> float list -> float list * float list
