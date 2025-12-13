(* Audio decoder interface *)

(* Decompress a single audio track *)
(* compressed_data: compressed samples in [-1.0, 1.0] range *)
(* original_min, original_max: original range for decompression *)
(* Returns: decompressed samples *)
val decompress_track : float list -> float -> float -> float list

(* Decompress stereo tracks (mid and side channels) *)
(* compressed_mid, compressed_side: compressed samples *)
(* min_mid, max_mid: original range for mid channel *)
(* min_side, max_side: original range for side channel *)
(* Returns: (decompressed_mid, decompressed_side) *)
val decompress_stereo_tracks : float list -> float list -> float -> float -> float -> float -> float list * float list

(* Convert Mid-Side to LR (skip if mono) *)
(* mid: mid channel, side: side channel *)
(* is_stereo: whether this is stereo (false for mono) *)
(* Returns: (left, right) *)
val mid_side_to_lr : float list -> float list -> bool -> float list * float list

(* Pass-through for silence replacement (silence is already lost, so just pass data through) *)
(* data: input signal *)
(* Returns: same data (pass-through) *)
val pass_through_silence : float list -> float list

(* Reconstruct signal from overlapping windows using overlap-add *)
(* windows: list of segments with window_data *)
(* Returns: reconstructed signal *)
val overlap_add_windows : Audiomodel.segment list -> float list

(* Dequantize segments (convert quantized_data to raw_data) *)
(* segments: list of segments with quantized_data *)
(* Returns: list of segments with raw_data (dequantized) and quantized_data cleared *)
val dequantize_segments : Audiomodel.segment list -> Audiomodel.segment list

(* Apply IMDCT Level 2 to each band (reverse of MDCT Level 2) *)
(* segments: list of segments with second-level MDCT coefficients in raw_data *)
(* Returns: list of segments with time-domain bands *)
val apply_imdct_level2 : Audiomodel.segment list -> Audiomodel.segment list

(* Apply MDCT Level 1 to each time-domain band (reverse of IMDCT bands) *)
(* segments: list of segments with time-domain bands in raw_data *)
(* Returns: list of segments with first-level MDCT coefficients per band *)
val apply_mdct_level1_to_bands : Audiomodel.segment list -> Audiomodel.segment list

(* Merge frequency bands back into single MDCT coefficient array *)
(* segments: list of segments with MDCT coefficients per band in raw_data *)
(* Returns: list of segments with merged MDCT coefficients *)
val merge_frequency_bands : Audiomodel.segment list -> Audiomodel.segment list

(* Apply final IMDCT to restore window from frequency domain *)
(* segments: list of segments with merged MDCT coefficients in raw_data *)
(* Returns: list of segments with window_data restored *)
val apply_imdct_final : Audiomodel.segment list -> Audiomodel.segment list

(* Read AudioFile structure and extract segments *)
(* audio_file: AudioFile structure *)
(* Returns: (segments, sample_rate, compression_params, original_length) *)
val read_audio_file : Audiomodel.audio_file -> 
  Audiomodel.segment list * int * (float * float) option * int option

(* Decode audio from file and reconstruct signal *)
(* input_file: input file path *)
(* Returns: (reconstructed_samples, sample_rate) *)
val decode_from_file : string -> float list * int
