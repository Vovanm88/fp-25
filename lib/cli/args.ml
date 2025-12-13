(* Command line arguments parser *)

type mode = Encode | Decode

type args = {
  mode : mode;
  input_file : string;
  output_file : string;
  snr_threshold_db : float;
}

exception Invalid_args of string

let print_usage () =
  Printf.printf "Usage: %s [OPTIONS]\n" Sys.argv.(0);
  Printf.printf "\n";
  Printf.printf "Modes:\n";
  Printf.printf "  --encode, -e          Encode WAV file to MLAC format\n";
  Printf.printf "  --decode, -d          Decode MLAC file to WAV format\n";
  Printf.printf "\n";
  Printf.printf "Options:\n";
  Printf.printf "  --input, -i FILE      Input file (WAV for encode, MLAC for decode)\n";
  Printf.printf "  --output, -o FILE     Output file (MLAC for encode, WAV for decode)\n";
  Printf.printf "  --quality, -q FLOAT   Quality threshold (SNR in dB, default: 40.0)\n";
  Printf.printf "  --help, -h            Show this help message\n";
  Printf.printf "\n";
  Printf.printf "Examples:\n";
  Printf.printf "  %s --encode -i input.wav -o output.mlac\n" Sys.argv.(0);
  Printf.printf "  %s --decode -i input.mlac -o output.wav\n" Sys.argv.(0);
  Printf.printf "  %s -e -i input.wav -o output.mlac --quality 50.0\n" Sys.argv.(0)

let ends_with s suffix =
  let len_s = String.length s in
  let len_suffix = String.length suffix in
  if len_s < len_suffix then false
  else
    let sub = String.sub s (len_s - len_suffix) len_suffix in
    sub = suffix

let parse_args () =
  let mode = ref None in
  let input_file = ref None in
  let output_file = ref None in
  let snr_threshold_db = ref 40.0 in

  let set_mode m =
    if !mode <> None then
      raise (Invalid_args "Mode can only be specified once (use either --encode or --decode)")
    else
      mode := Some m
  in

  let set_input_file f =
    if !input_file <> None then
      raise (Invalid_args "Input file can only be specified once")
    else
      input_file := Some f
  in

  let set_output_file f =
    if !output_file <> None then
      raise (Invalid_args "Output file can only be specified once")
    else
      output_file := Some f
  in

  let set_quality q =
    let q_val = float_of_string q in
    if q_val <= 0.0 then
      raise (Invalid_args "Quality threshold must be positive")
    else
      snr_threshold_db := q_val
  in

  let spec = [
    ("--encode", Arg.Unit (fun () -> set_mode Encode), "Encode WAV to MLAC");
    ("-e", Arg.Unit (fun () -> set_mode Encode), "Encode WAV to MLAC");
    ("--decode", Arg.Unit (fun () -> set_mode Decode), "Decode MLAC to WAV");
    ("-d", Arg.Unit (fun () -> set_mode Decode), "Decode MLAC to WAV");
    ("--input", Arg.String set_input_file, "Input file");
    ("-i", Arg.String set_input_file, "Input file");
    ("--output", Arg.String set_output_file, "Output file");
    ("-o", Arg.String set_output_file, "Output file");
    ("--quality", Arg.String set_quality, "Quality threshold (SNR in dB)");
    ("-q", Arg.String set_quality, "Quality threshold (SNR in dB)");
    ("--help", Arg.Unit (fun () -> print_usage (); exit 0), "Show help");
    ("-h", Arg.Unit (fun () -> print_usage (); exit 0), "Show help");
  ] in

  let anon_fun s =
    raise (Invalid_args (Printf.sprintf "Unexpected argument: %s" s))
  in

  try
    Arg.parse spec anon_fun "Audio codec (MLAC)";
    
    (* Validate required arguments *)
    let mode_val = match !mode with
      | None -> raise (Invalid_args "Mode is required (use --encode or --decode)")
      | Some m -> m
    in

    let input_val = match !input_file with
      | None -> raise (Invalid_args "Input file is required (use --input or -i)")
      | Some f -> f
    in

    let output_val = match !output_file with
      | None -> raise (Invalid_args "Output file is required (use --output or -o)")
      | Some f -> f
    in

    (* Validate file extensions based on mode *)
    (match mode_val with
    | Encode ->
        if not (ends_with input_val ".wav") then
          raise (Invalid_args "Input file must be a .wav file for encode mode");
        if not (ends_with output_val ".mlac") then
          raise (Invalid_args "Output file must be a .mlac file for encode mode")
    | Decode ->
        if not (ends_with input_val ".mlac") then
          raise (Invalid_args "Input file must be a .mlac file for decode mode");
        if not (ends_with output_val ".wav") then
          raise (Invalid_args "Output file must be a .wav file for decode mode"));

    {
      mode = mode_val;
      input_file = input_val;
      output_file = output_val;
      snr_threshold_db = !snr_threshold_db;
    }
  with
  | Invalid_args msg ->
      Printf.eprintf "Error: %s\n\n" msg;
      print_usage ();
      (exit 1 [@nontail])
  | Failure msg ->
      Printf.eprintf "Error: %s\n\n" msg;
      print_usage ();
      (exit 1 [@nontail])
