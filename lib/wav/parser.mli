(* WAV file parser interface *)

exception Invalid_wav_file of string

type wav_info = {
  sample_rate : int;
  num_channels : int;
  bits_per_sample : int;
  num_samples : int;
}

type wav_data = {
  info : wav_info;
  samples : float list;
}

(** Read a WAV file from the given file path.
    Returns the WAV data structure containing header info and samples.
    Samples are normalized to [-1.0, 1.0] range. *)
val read_wav : string -> wav_data

(** Read WAV file info (header) without loading all samples into memory. *)
val read_wav_info : string -> wav_info
