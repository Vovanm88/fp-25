(* WAV file writer interface *)

type wav_info = {
  sample_rate : int;
  num_channels : int;
  bits_per_sample : int;
}

(** Write a WAV file to the given file path.
    [samples] should be a list of floats in the range [-1.0, 1.0].
    The samples will be converted to the specified bits_per_sample format. *)
val write_wav : string -> wav_info -> float list -> unit
