(* Command line arguments parser interface *)

type mode = Encode | Decode

type args = {
  mode : mode;
  input_file : string;
  output_file : string;
  snr_threshold_db : float;
}

exception Invalid_args of string

val parse_args : unit -> args
(** Parse command line arguments *)

val print_usage : unit -> unit
(** Print usage information *)
