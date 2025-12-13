(* Audio encoder interface *)

(* Compress a single audio track *)
(* Returns: (compressed_samples, original_min, original_max, snr_db) *)
val compress_track : float list -> float list * float * float * float

(* Compress stereo tracks (left and right channels) *)
(* Returns: ((compressed_left, compressed_right), (original_min_l, original_max_l), (original_min_r, original_max_r), snr_db) *)
val compress_stereo_tracks :
  float list ->
  float list ->
  (float list * float list) * (float * float) * (float * float) * float

(* Convert LR to Mid-Side (skip if mono) *)
(* left: left channel, right: right channel (can be same for mono) *)
(* Returns: (mid, side, is_stereo) where is_stereo indicates if conversion was performed *)
val lr_to_mid_side : float list -> float list -> float list * float list * bool

(* Replace quiet fragments after loud ones with silence *)
(* data: input signal *)
(* window_size: size of window for RMS calculation (default: 100 samples) *)
(* loud_threshold: RMS threshold for "loud" (default: 0.1) *)
(* quiet_threshold: RMS threshold for "quiet" (default: 0.01) *)
(* quiet_duration_ms: minimum duration of quiet after loud to replace (default: 50ms) *)
(* sample_rate: sample rate in Hz *)
(* Returns: (processed_data, silence_mask) where silence_mask indicates replaced samples *)
val replace_silence_after_loud :
  ?window_size:int ->
  ?loud_threshold:float ->
  ?quiet_threshold:float ->
  ?quiet_duration_ms:float ->
  int ->
  float list ->
  float list * bool list

(* Re-export types from Audiomodel for convenience *)
type segment_info = Audiomodel.segment_info
type segment = Audiomodel.segment

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

(* Apply first level MDCT to windows (window → frequency domain) *)
(* segments: list of segments with window_data *)
(* Returns: list of segments with MDCT coefficients stored in raw_data (first band) *)
val apply_mdct_level1 : segment list -> segment list

(* Split MDCT coefficients into frequency bands *)
(* segments: list of segments with MDCT coefficients in raw_data *)
(* sample_rate: sample rate in Hz *)
(* frequency_boundaries_hz: frequency boundaries in Hz (default: standard 10-band) *)
(* Returns: list of segments with frequency bands split *)
val split_frequency_bands :
  ?frequency_boundaries_hz:float list -> int -> segment list -> segment list

(* Apply IMDCT to each frequency band (bands → time domain) *)
(* segments: list of segments with frequency bands in raw_data *)
(* Returns: list of segments with time-domain bands (for second level MDCT) *)
val apply_imdct_to_bands : segment list -> segment list

(* Apply second level MDCT to each time-domain band *)
(* segments: list of segments with time-domain bands in raw_data *)
(* Returns: list of segments with second-level MDCT coefficients *)
val apply_mdct_level2 : segment list -> segment list

(* Select quantization thresholds for each band based on acceptable noise level *)
(* segments: list of segments with second-level MDCT coefficients *)
(* snr_threshold_db: minimum acceptable SNR in dB for each band (default: 40.0) *)
(* Returns: list of segments with quantization_levels and band_ranges filled *)
val select_quantization_thresholds :
  ?snr_threshold_db:float -> segment list -> segment list

(* Create AudioFile structure from encoded segments *)
(* segments: list of encoded segments (after all transformations) *)
(* sample_rate: sample rate in Hz *)
(* bits_per_sample: bits per sample (default: 16) *)
(* compression_params: (original_min, original_max) for decompression *)
(* original_length: original length before padding (if track was padded) *)
(* Returns: AudioFile structure *)
val create_audio_file :
  ?bits_per_sample:int ->
  ?compression_params:(float * float) option ->
  ?original_length:int option ->
  int ->
  segment list ->
  Audiomodel.audio_file

(* Encode audio data and write to file *)
(* input: input audio samples *)
(* sample_rate: sample rate in Hz *)
(* output_file: output file path *)
(* ?snr_threshold_db: minimum acceptable SNR in dB for quantization (default: 40.0) *)
(* Returns: AudioFile structure that was written *)
val encode_to_file :
  ?snr_threshold_db:float ->
  float list ->
  int ->
  string ->
  Audiomodel.audio_file
