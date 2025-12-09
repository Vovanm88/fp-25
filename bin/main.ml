(* Lab 3: Main entry point *)

open Lab3

type method_type = Interpolator.method_type = Linear | Lagrange | Newton

(* Parse command-line arguments *)
let parse_args () =
  let step = ref 1.0 in
  let methods = ref [] in
  let window_size = ref 4 in
  let args =
    [
      ( "--step",
        Arg.Set_float step,
        "STEP Step size for interpolation (default: 1.0)" );
      ( "--linear",
        Arg.Unit (fun () -> methods := Linear :: !methods),
        " Use linear interpolation" );
      ( "--lagrange",
        Arg.Unit (fun () -> methods := Lagrange :: !methods),
        " Use Lagrange interpolation" );
      ( "--newton",
        Arg.Unit (fun () -> methods := Newton :: !methods),
        " Use Newton interpolation" );
      ( "-n",
        Arg.Set_int window_size,
        "N Window size for Lagrange/Newton interpolation (default: 4)" );
    ]
  in
  Arg.parse args (fun _ -> ())
    "Usage: lab3 [--linear|--lagrange|--newton] [-n N] [--step STEP]";
  if !methods = [] then (
    prerr_endline "Error: at least one interpolation method must be specified";
    exit 1);
  (!methods, !step, !window_size)

let () =
  let methods, step, window_size = parse_args () in
  Pipeline.run ~methods ~step ~window_size ~parse_line:Input.parse_line
    ~output:Output.print_point
