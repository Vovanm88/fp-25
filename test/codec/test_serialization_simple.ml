(* Simple test for serialization of quantization_levels *)

open Alcotest

module Writer = Io.Writer
module Reader = Io.Reader

let test_quantization_levels_roundtrip () =
  let levels = [2; 4; 8; 16; 32] in
  
  (* Serialize *)
  let bytes = Writer.write_list_int32 levels in
  
  (* Deserialize *)
  let (read_levels, remaining) = Reader.read_list_int32 bytes in
  
  check int "Remaining bytes is 0" (List.length remaining) 0;
  check int "Levels count matches" (List.length read_levels) (List.length levels);
  
  (* Check each level *)
  List.iter2 (fun orig read ->
    check int "Level matches" read orig
  ) levels read_levels

let () =
  run "Simple Serialization Tests" [
    "Basic", [
      test_case "quantization_levels roundtrip" `Quick test_quantization_levels_roundtrip;
    ];
  ]
