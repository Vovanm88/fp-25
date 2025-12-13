(* Command line arguments parser interface *)

type mode = Encode | Decode

type args = {
  mode : mode;
  input_file : string;
  output_file : string;
  snr_threshold_db : float;
}

exception Invalid_args of string

(** Parse command line arguments *)
val parse_args : unit -> args

(** Print usage information *)
val print_usage : unit -> unit
