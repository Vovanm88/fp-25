(* Lab 3: Golden tests for streaming interpolation *)

open Alcotest

let normalize_output s =
  String.split_on_char '\n' s
  |> List.map String.trim
  |> List.filter (fun s -> s <> "")
  |> String.concat "\n"

let run_program input step =
  let cmd = Printf.sprintf "dune exec -- lab3 --linear --step %g" step in
  let ic, oc = Unix.open_process cmd in
  output_string oc input;
  close_out oc;
  let output = Buffer.create 1024 in
  (try
     while true do
       Buffer.add_string output (input_line ic);
       Buffer.add_char output '\n'
     done
   with End_of_file -> ());
  ignore (Unix.close_process (ic, oc));
  normalize_output (Buffer.contents output)

let project_root =
  let rec find_root dir =
    if Sys.file_exists (Filename.concat dir "dune-project") then dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then failwith "Could not find project root"
      else find_root parent
  in
  find_root (Sys.getcwd ())

let read_file filename =
  let filename =
    if String.length filename > 0 && filename.[0] = '/' then filename
    else Filename.concat project_root filename
  in
  let ic = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line ic in
      read_lines (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc |> String.concat "\n"
  in
  normalize_output (read_lines [])

let test_golden test_name input_file expected_file step =
  let input = read_file input_file in
  let expected = read_file expected_file in
  let actual = run_program (input ^ "\n") step in
  check string test_name expected actual

let test_golden_1 () =
  test_golden "golden test 1: basic linear interpolation"
    "test/golden/test1_input.txt" "test/golden/test1_expected.txt" 0.7

let test_golden_2 () =
  test_golden "golden test 2: quadratic-like function"
    "test/golden/test2_input.txt" "test/golden/test2_expected.txt" 0.5

let test_golden_3 () =
  test_golden "golden test 3: negative values" "test/golden/test3_input.txt"
    "test/golden/test3_expected.txt" 0.5

let golden_tests =
  [
    ("golden_1", `Slow, test_golden_1);
    ("golden_2", `Slow, test_golden_2);
    ("golden_3", `Slow, test_golden_3);
  ]

let () = run "Lab3 Golden" [ ("golden", golden_tests) ]
