(* Unit tests for IO reader and writer modules *)
open Alcotest

module Reader = Io.Reader
module Writer = Io.Writer

(* Tests for int32 *)
let test_write_read_int32_basic () =
  let value = 12345 in
  let bytes = Writer.write_int32 value in
  check int "Write_int32 returns 4 bytes" (List.length bytes) 4;
  let (read_value, remaining) = Reader.read_int32 bytes in
  check int "Read_int32 roundtrip" read_value value;
  check (list int) "Read_int32 remaining empty" remaining []

let test_write_read_int32_zero () =
  let value = 0 in
  let bytes = Writer.write_int32 value in
  let (read_value, _) = Reader.read_int32 bytes in
  check int "Read_int32 zero" read_value 0

let test_write_read_int32_negative () =
  let value = -12345 in
  let bytes = Writer.write_int32 value in
  let (read_value, _) = Reader.read_int32 bytes in
  check int "Read_int32 negative" read_value value

let test_write_read_int32_max () =
  let value = 2147483647 in  (* Max int32 *)
  let bytes = Writer.write_int32 value in
  let (read_value, _) = Reader.read_int32 bytes in
  check int "Read_int32 max" read_value value

let test_write_read_int32_min () =
  let value = -2147483648 in  (* Min int32 *)
  let bytes = Writer.write_int32 value in
  let (read_value, _) = Reader.read_int32 bytes in
  check int "Read_int32 min" read_value value

let test_write_read_int32_multiple () =
  let values = [123; -456; 789; 0] in
  let bytes = List.concat (List.map Writer.write_int32 values) in
  let rec read_all acc remaining = function
    | [] -> (List.rev acc, remaining)
    | _ when List.length remaining < 4 -> (List.rev acc, remaining)
    | _ ->
      let (value, rest) = Reader.read_int32 remaining in
      read_all (value :: acc) rest (List.tl values)
  in
  let (read_values, remaining) = read_all [] bytes values in
  check (list int) "Read_int32 multiple values" read_values values;
  check (list int) "Read_int32 multiple remaining" remaining []

let test_read_int32_insufficient_bytes () =
  try
    let _ = Reader.read_int32 [1; 2; 3] in
    check bool "Should fail on insufficient bytes" false true
  with
  | Failure _ -> check bool "Fails on insufficient bytes" true true
  | _ -> check bool "Should fail with Failure" false true

(* Tests for int16 *)
let test_write_read_int16_basic () =
  let value = 12345 in
  let bytes = Writer.write_int16 value in
  check int "Write_int16 returns 2 bytes" (List.length bytes) 2;
  let (read_value, remaining) = Reader.read_int16 bytes in
  check int "Read_int16 roundtrip" read_value value;
  check (list int) "Read_int16 remaining empty" remaining []

let test_write_read_int16_zero () =
  let value = 0 in
  let bytes = Writer.write_int16 value in
  let (read_value, _) = Reader.read_int16 bytes in
  check int "Read_int16 zero" read_value 0

let test_write_read_int16_negative () =
  let value = -12345 in
  let bytes = Writer.write_int16 value in
  let (read_value, _) = Reader.read_int16 bytes in
  check int "Read_int16 negative" read_value value

let test_write_read_int16_max () =
  let value = 32767 in  (* Max int16 *)
  let bytes = Writer.write_int16 value in
  let (read_value, _) = Reader.read_int16 bytes in
  check int "Read_int16 max" read_value value

let test_write_read_int16_min () =
  let value = -32768 in  (* Min int16 *)
  let bytes = Writer.write_int16 value in
  let (read_value, _) = Reader.read_int16 bytes in
  check int "Read_int16 min" read_value value

let test_read_int16_insufficient_bytes () =
  try
    let _ = Reader.read_int16 [1] in
    check bool "Should fail on insufficient bytes" false true
  with
  | Failure _ -> check bool "Fails on insufficient bytes" true true
  | _ -> check bool "Should fail with Failure" false true

(* Tests for uint8 *)
let test_write_read_uint8_basic () =
  let value = 128 in
  let bytes = Writer.write_uint8 value in
  check int "Write_uint8 returns 1 byte" (List.length bytes) 1;
  let (read_value, remaining) = Reader.read_uint8 bytes in
  check int "Read_uint8 roundtrip" read_value value;
  check (list int) "Read_uint8 remaining empty" remaining []

let test_write_read_uint8_zero () =
  let value = 0 in
  let bytes = Writer.write_uint8 value in
  let (read_value, _) = Reader.read_uint8 bytes in
  check int "Read_uint8 zero" read_value 0

let test_write_read_uint8_max () =
  let value = 255 in
  let bytes = Writer.write_uint8 value in
  let (read_value, _) = Reader.read_uint8 bytes in
  check int "Read_uint8 max" read_value 255

let test_write_uint8_invalid_negative () =
  try
    let _ = Writer.write_uint8 (-1) in
    check bool "Should fail on negative uint8" false true
  with
  | Failure _ -> check bool "Fails on negative uint8" true true
  | _ -> check bool "Should fail with Failure" false true

let test_write_uint8_invalid_too_large () =
  try
    let _ = Writer.write_uint8 256 in
    check bool "Should fail on uint8 > 255" false true
  with
  | Failure _ -> check bool "Fails on uint8 > 255" true true
  | _ -> check bool "Should fail with Failure" false true

let test_read_uint8_insufficient_bytes () =
  try
    let _ = Reader.read_uint8 [] in
    check bool "Should fail on insufficient bytes" false true
  with
  | Failure _ -> check bool "Fails on insufficient bytes" true true
  | _ -> check bool "Should fail with Failure" false true

(* Tests for float *)
let test_write_read_float_basic () =
  let value = 3.14159 in
  let bytes = Writer.write_float value in
  check int "Write_float returns 4 bytes" (List.length bytes) 4;
  let (read_value, remaining) = Reader.read_float bytes in
  check (float 0.0001) "Read_float roundtrip" read_value value;
  check (list int) "Read_float remaining empty" remaining []

let test_write_read_float_zero () =
  let value = 0.0 in
  let bytes = Writer.write_float value in
  let (read_value, _) = Reader.read_float bytes in
  check (float 0.0001) "Read_float zero" read_value 0.0

let test_write_read_float_negative () =
  let value = -123.456 in
  let bytes = Writer.write_float value in
  let (read_value, _) = Reader.read_float bytes in
  check (float 0.0001) "Read_float negative" read_value value

let test_write_read_float_large () =
  let value = 1.0e10 in
  let bytes = Writer.write_float value in
  let (read_value, _) = Reader.read_float bytes in
  check (float 0.1) "Read_float large" read_value value

let test_write_read_float_small () =
  let value = 1.0e-10 in
  let bytes = Writer.write_float value in
  let (read_value, _) = Reader.read_float bytes in
  check (float 0.0001) "Read_float small" read_value value

let test_read_float_insufficient_bytes () =
  try
    let _ = Reader.read_float [1; 2; 3] in
    check bool "Should fail on insufficient bytes" false true
  with
  | Failure _ -> check bool "Fails on insufficient bytes" true true
  | _ -> check bool "Should fail with Failure" false true

(* Tests for string *)
let test_write_read_string_basic () =
  let value = "Hello, World!" in
  let bytes = Writer.write_string value in
  let (read_value, remaining) = Reader.read_string bytes in
  check string "Read_string roundtrip" read_value value;
  check (list int) "Read_string remaining empty" remaining []

let test_write_read_string_empty () =
  let value = "" in
  let bytes = Writer.write_string value in
  let (read_value, remaining) = Reader.read_string bytes in
  check string "Read_string empty" read_value "";
  check (list int) "Read_string empty remaining" remaining []

let test_write_read_string_unicode () =
  let value = "Привет, мир!" in
  let bytes = Writer.write_string value in
  let (read_value, remaining) = Reader.read_string bytes in
  check string "Read_string unicode roundtrip" read_value value;
  check (list int) "Read_string unicode remaining" remaining []

let test_write_read_string_special_chars () =
  let value = "Hello\nWorld\tTest\r\n" in
  let bytes = Writer.write_string value in
  let (read_value, remaining) = Reader.read_string bytes in
  check string "Read_string special chars" read_value value;
  check (list int) "Read_string special chars remaining" remaining []

let test_read_string_insufficient_bytes () =
  try
    let _ = Reader.read_string (Writer.write_int32 10) in
    check bool "Should fail on insufficient bytes" false true
  with
  | Failure _ -> check bool "Fails on insufficient bytes" true true
  | _ -> check bool "Should fail with Failure" false true

(* Tests for list_int32 *)
let test_write_read_list_int32_basic () =
  let value = [1; 2; 3; 4; 5] in
  let bytes = Writer.write_list_int32 value in
  let (read_value, remaining) = Reader.read_list_int32 bytes in
  check (list int) "Read_list_int32 roundtrip" read_value value;
  check (list int) "Read_list_int32 remaining empty" remaining []

let test_write_read_list_int32_empty () =
  let value = [] in
  let bytes = Writer.write_list_int32 value in
  let (read_value, remaining) = Reader.read_list_int32 bytes in
  check (list int) "Read_list_int32 empty" read_value [];
  check (list int) "Read_list_int32 empty remaining" remaining []

let test_write_read_list_int32_single () =
  let value = [42] in
  let bytes = Writer.write_list_int32 value in
  let (read_value, remaining) = Reader.read_list_int32 bytes in
  check (list int) "Read_list_int32 single" read_value value;
  check (list int) "Read_list_int32 single remaining" remaining []

let test_write_read_list_int32_negative () =
  let value = [-1; -2; -3] in
  let bytes = Writer.write_list_int32 value in
  let (read_value, remaining) = Reader.read_list_int32 bytes in
  check (list int) "Read_list_int32 negative" read_value value;
  check (list int) "Read_list_int32 negative remaining" remaining []

let test_write_read_list_int32_large () =
  let value = [2147483647; -2147483648; 0] in
  let bytes = Writer.write_list_int32 value in
  let (read_value, remaining) = Reader.read_list_int32 bytes in
  check (list int) "Read_list_int32 large" read_value value;
  check (list int) "Read_list_int32 large remaining" remaining []

(* Tests for list_float *)
let test_write_read_list_float_basic () =
  let value = [1.0; 2.5; 3.14159; -4.2] in
  let bytes = Writer.write_list_float value in
  let (read_value, remaining) = Reader.read_list_float bytes in
  check (list (float 0.0001)) "Read_list_float roundtrip" read_value value;
  check (list int) "Read_list_float remaining empty" remaining []

let test_write_read_list_float_empty () =
  let value = [] in
  let bytes = Writer.write_list_float value in
  let (read_value, remaining) = Reader.read_list_float bytes in
  check (list (float 0.0001)) "Read_list_float empty" read_value [];
  check (list int) "Read_list_float empty remaining" remaining []

let test_write_read_list_float_single () =
  let value = [3.14159] in
  let bytes = Writer.write_list_float value in
  let (read_value, remaining) = Reader.read_list_float bytes in
  check (list (float 0.0001)) "Read_list_float single" read_value value;
  check (list int) "Read_list_float single remaining" remaining []

(* Tests for file I/O *)
let test_write_read_int32_file () =
  let filename = "/tmp/test_int32.bin" in
  let value = 12345 in
  Writer.write_int32_to_file filename value;
  let read_value = Reader.read_int32_from_file filename in
  check int "Write-read int32 file roundtrip" read_value value

let test_write_read_float_file () =
  let filename = "/tmp/test_float.bin" in
  let value = 3.14159 in
  Writer.write_float_to_file filename value;
  let read_value = Reader.read_float_from_file filename in
  check (float 0.0001) "Write-read float file roundtrip" read_value value

let test_write_read_string_file () =
  let filename = "/tmp/test_string.bin" in
  let value = "Hello, World!" in
  Writer.write_string_to_file filename value;
  let read_value = Reader.read_string_from_file filename in
  check string "Write-read string file roundtrip" read_value value

let test_write_read_list_int32_file () =
  let filename = "/tmp/test_list_int32.bin" in
  let value = [1; 2; 3; 4; 5] in
  Writer.write_list_int32_to_file filename value;
  let read_value = Reader.read_list_int32_from_file filename in
  check (list int) "Write-read list_int32 file roundtrip" read_value value

let test_write_read_list_float_file () =
  let filename = "/tmp/test_list_float.bin" in
  let value = [1.0; 2.5; 3.14159] in
  Writer.write_list_float_to_file filename value;
  let read_value = Reader.read_list_float_from_file filename in
  check (list (float 0.0001)) "Write-read list_float file roundtrip" read_value value

let test_write_read_bytes_file () =
  let filename = "/tmp/test_bytes.bin" in
  let value = [65; 66; 67; 68; 69] in
  Writer.write_to_file filename value;
  let read_value = Reader.read_from_file filename in
  check (list int) "Write-read bytes file roundtrip" read_value value

let test_write_read_complex_file () =
  let filename = "/tmp/test_complex.bin" in
  let int32_val = 12345 in
  let float_val = 3.14159 in
  let string_val = "Test" in
  let bytes = Writer.write_int32 int32_val @
              Writer.write_float float_val @
              Writer.write_string string_val in
  Writer.write_to_file filename bytes;
  let read_bytes = Reader.read_from_file filename in
  let (read_int32, remaining) = Reader.read_int32 read_bytes in
  let (read_float, remaining) = Reader.read_float remaining in
  let (read_string, remaining) = Reader.read_string remaining in
  check int "Complex file int32" read_int32 int32_val;
  check (float 0.0001) "Complex file float" read_float float_val;
  check string "Complex file string" read_string string_val;
  check (list int) "Complex file remaining" remaining []

(* Tests for chunks *)
let test_write_read_chunk_header () =
  let chunk_id = "TEST" in
  let size = 42 in
  let bytes = Writer.write_chunk_header chunk_id size in
  check int "Write_chunk_header returns 8 bytes" (List.length bytes) 8;
  let (read_id, read_size, remaining) = Reader.read_chunk_header bytes in
  check string "Read_chunk_header ID" read_id chunk_id;
  check int "Read_chunk_header size" read_size size;
  check (list int) "Read_chunk_header remaining" remaining []

let test_write_read_chunk () =
  let chunk_id = "DATA" in
  let data = [1; 2; 3; 4; 5] in
  let bytes = Writer.write_chunk chunk_id data in
  let (read_id, read_data, remaining) = Reader.read_chunk bytes in
  check string "Read_chunk ID" read_id chunk_id;
  check (list int) "Read_chunk data" read_data data;
  check (list int) "Read_chunk remaining" remaining []

let test_write_read_chunk_file () =
  let filename = "/tmp/test_chunk.bin" in
  let chunk_id = "TEST" in
  let data = [65; 66; 67; 68] in
  Writer.write_chunk_to_file filename chunk_id data;
  let (read_id, read_data, remaining) = Reader.read_chunk_from_file filename in
  check string "Read_chunk_from_file ID" read_id chunk_id;
  check (list int) "Read_chunk_from_file data" read_data data;
  check (list int) "Read_chunk_from_file remaining" remaining []

let test_write_read_multiple_chunks () =
  let chunk1_id = "CHK1" in
  let chunk1_data = [1; 2; 3] in
  let chunk2_id = "CHK2" in
  let chunk2_data = [4; 5; 6; 7] in
  let bytes = Writer.write_chunk chunk1_id chunk1_data @
              Writer.write_chunk chunk2_id chunk2_data in
  let (read_id1, read_data1, remaining) = Reader.read_chunk bytes in
  let (read_id2, read_data2, remaining) = Reader.read_chunk remaining in
  check string "Multiple chunks ID1" read_id1 chunk1_id;
  check (list int) "Multiple chunks data1" read_data1 chunk1_data;
  check string "Multiple chunks ID2" read_id2 chunk2_id;
  check (list int) "Multiple chunks data2" read_data2 chunk2_data;
  check (list int) "Multiple chunks remaining" remaining []

let test_find_chunk () =
  let chunk1_id = "CHK1" in
  let chunk1_data = [1; 2; 3] in
  let chunk2_id = "CHK2" in
  let chunk2_data = [4; 5; 6] in
  let chunk3_id = "CHK3" in
  let chunk3_data = [7; 8; 9] in
  let bytes = Writer.write_chunk chunk1_id chunk1_data @
              Writer.write_chunk chunk2_id chunk2_data @
              Writer.write_chunk chunk3_id chunk3_data in
  match Reader.find_chunk bytes chunk2_id with
  | Some (id, data, remaining) ->
    check string "Find_chunk ID" id chunk2_id;
    check (list int) "Find_chunk data" data chunk2_data;
    check bool "Find_chunk has remaining" (List.length remaining > 0) true
  | None -> check bool "Find_chunk found" false true

let test_find_chunk_not_found () =
  let chunk_id = "CHK1" in
  let data = [1; 2; 3] in
  let bytes = Writer.write_chunk chunk_id data in
  match Reader.find_chunk bytes "NOPE" with
  | Some _ -> check bool "Find_chunk should not find" false true
  | None -> check bool "Find_chunk not found" true true

let test_find_chunk_in_file () =
  let filename = "/tmp/test_find_chunk.bin" in
  let chunk1_id = "CHK1" in
  let chunk1_data = [1; 2; 3] in
  let chunk2_id = "CHK2" in
  let chunk2_data = [4; 5; 6] in
  let bytes = Writer.write_chunk chunk1_id chunk1_data @
              Writer.write_chunk chunk2_id chunk2_data in
  Writer.write_to_file filename bytes;
  match Reader.find_chunk_in_file filename chunk2_id with
  | Some (id, data, _) ->
    check string "Find_chunk_in_file ID" id chunk2_id;
    check (list int) "Find_chunk_in_file data" data chunk2_data
  | None -> check bool "Find_chunk_in_file found" false true

(* Tests for file headers *)
let test_write_read_file_header () =
  let magic = "ACOD" in
  let version = 1 in
  let bytes = Writer.write_file_header magic version in
  check int "Write_file_header returns 8 bytes" (List.length bytes) 8;
  let (read_magic, read_version, remaining) = Reader.read_file_header bytes in
  check string "Read_file_header magic" read_magic magic;
  check int "Read_file_header version" read_version version;
  check (list int) "Read_file_header remaining" remaining []

let test_write_read_file_header_file () =
  let filename = "/tmp/test_header.bin" in
  let magic = "TEST" in
  let version = 42 in
  Writer.write_file_header_to_file filename magic version;
  let (read_magic, read_version, remaining) = Reader.read_file_header_from_file filename in
  check string "Read_file_header_from_file magic" read_magic magic;
  check int "Read_file_header_from_file version" read_version version;
  check (list int) "Read_file_header_from_file remaining" remaining []

let test_file_header_with_chunks () =
  let magic = "ACOD" in
  let version = 1 in
  let chunk_id = "DATA" in
  let chunk_data = [1; 2; 3; 4; 5] in
  let bytes = Writer.write_file_header magic version @
              Writer.write_chunk chunk_id chunk_data in
  let (read_magic, read_version, remaining) = Reader.read_file_header bytes in
  let (read_id, read_data, remaining) = Reader.read_chunk remaining in
  check string "Header+chunk magic" read_magic magic;
  check int "Header+chunk version" read_version version;
  check string "Header+chunk ID" read_id chunk_id;
  check (list int) "Header+chunk data" read_data chunk_data;
  check (list int) "Header+chunk remaining" remaining []

(* Integration test: multiple types together *)
let test_io_integration_multiple_types () =
  let int32_val = 12345 in
  let int16_val = -1234 in
  let uint8_val = 128 in
  let float_val = 3.14159 in
  let string_val = "Test" in
  let list_int32_val = [1; 2; 3] in
  
  let bytes = Writer.write_int32 int32_val @
              Writer.write_int16 int16_val @
              Writer.write_uint8 uint8_val @
              Writer.write_float float_val @
              Writer.write_string string_val @
              Writer.write_list_int32 list_int32_val in
  
  let (read_int32, remaining) = Reader.read_int32 bytes in
  let (read_int16, remaining) = Reader.read_int16 remaining in
  let (read_uint8, remaining) = Reader.read_uint8 remaining in
  let (read_float, remaining) = Reader.read_float remaining in
  let (read_string, remaining) = Reader.read_string remaining in
  let (read_list_int32, remaining) = Reader.read_list_int32 remaining in
  
  check int "Integration int32" read_int32 int32_val;
  check int "Integration int16" read_int16 int16_val;
  check int "Integration uint8" read_uint8 uint8_val;
  check (float 0.0001) "Integration float" read_float float_val;
  check string "Integration string" read_string string_val;
  check (list int) "Integration list_int32" read_list_int32 list_int32_val;
  check (list int) "Integration remaining empty" remaining []

let () =
  run "IO Reader Writer Tests" [
    "Int32", [
      test_case "Write-read int32 basic" `Quick test_write_read_int32_basic;
      test_case "Write-read int32 zero" `Quick test_write_read_int32_zero;
      test_case "Write-read int32 negative" `Quick test_write_read_int32_negative;
      test_case "Write-read int32 max" `Quick test_write_read_int32_max;
      test_case "Write-read int32 min" `Quick test_write_read_int32_min;
      test_case "Write-read int32 multiple" `Quick test_write_read_int32_multiple;
      test_case "Read int32 insufficient bytes" `Quick test_read_int32_insufficient_bytes;
    ];
    "Int16", [
      test_case "Write-read int16 basic" `Quick test_write_read_int16_basic;
      test_case "Write-read int16 zero" `Quick test_write_read_int16_zero;
      test_case "Write-read int16 negative" `Quick test_write_read_int16_negative;
      test_case "Write-read int16 max" `Quick test_write_read_int16_max;
      test_case "Write-read int16 min" `Quick test_write_read_int16_min;
      test_case "Read int16 insufficient bytes" `Quick test_read_int16_insufficient_bytes;
    ];
    "Uint8", [
      test_case "Write-read uint8 basic" `Quick test_write_read_uint8_basic;
      test_case "Write-read uint8 zero" `Quick test_write_read_uint8_zero;
      test_case "Write-read uint8 max" `Quick test_write_read_uint8_max;
      test_case "Write uint8 invalid negative" `Quick test_write_uint8_invalid_negative;
      test_case "Write uint8 invalid too large" `Quick test_write_uint8_invalid_too_large;
      test_case "Read uint8 insufficient bytes" `Quick test_read_uint8_insufficient_bytes;
    ];
    "Float", [
      test_case "Write-read float basic" `Quick test_write_read_float_basic;
      test_case "Write-read float zero" `Quick test_write_read_float_zero;
      test_case "Write-read float negative" `Quick test_write_read_float_negative;
      test_case "Write-read float large" `Quick test_write_read_float_large;
      test_case "Write-read float small" `Quick test_write_read_float_small;
      test_case "Read float insufficient bytes" `Quick test_read_float_insufficient_bytes;
    ];
    "String", [
      test_case "Write-read string basic" `Quick test_write_read_string_basic;
      test_case "Write-read string empty" `Quick test_write_read_string_empty;
      test_case "Write-read string unicode" `Quick test_write_read_string_unicode;
      test_case "Write-read string special chars" `Quick test_write_read_string_special_chars;
      test_case "Read string insufficient bytes" `Quick test_read_string_insufficient_bytes;
    ];
    "List Int32", [
      test_case "Write-read list_int32 basic" `Quick test_write_read_list_int32_basic;
      test_case "Write-read list_int32 empty" `Quick test_write_read_list_int32_empty;
      test_case "Write-read list_int32 single" `Quick test_write_read_list_int32_single;
      test_case "Write-read list_int32 negative" `Quick test_write_read_list_int32_negative;
      test_case "Write-read list_int32 large" `Quick test_write_read_list_int32_large;
    ];
    "List Float", [
      test_case "Write-read list_float basic" `Quick test_write_read_list_float_basic;
      test_case "Write-read list_float empty" `Quick test_write_read_list_float_empty;
      test_case "Write-read list_float single" `Quick test_write_read_list_float_single;
    ];
    "Integration", [
      test_case "IO integration multiple types" `Quick test_io_integration_multiple_types;
    ];
    "File I/O", [
      test_case "Write-read int32 file" `Quick test_write_read_int32_file;
      test_case "Write-read float file" `Quick test_write_read_float_file;
      test_case "Write-read string file" `Quick test_write_read_string_file;
      test_case "Write-read list_int32 file" `Quick test_write_read_list_int32_file;
      test_case "Write-read list_float file" `Quick test_write_read_list_float_file;
      test_case "Write-read bytes file" `Quick test_write_read_bytes_file;
      test_case "Write-read complex file" `Quick test_write_read_complex_file;
    ];
    "Chunks", [
      test_case "Write-read chunk header" `Quick test_write_read_chunk_header;
      test_case "Write-read chunk" `Quick test_write_read_chunk;
      test_case "Write-read chunk file" `Quick test_write_read_chunk_file;
      test_case "Write-read multiple chunks" `Quick test_write_read_multiple_chunks;
      test_case "Find chunk" `Quick test_find_chunk;
      test_case "Find chunk not found" `Quick test_find_chunk_not_found;
      test_case "Find chunk in file" `Quick test_find_chunk_in_file;
    ];
    "File Headers", [
      test_case "Write-read file header" `Quick test_write_read_file_header;
      test_case "Write-read file header file" `Quick test_write_read_file_header_file;
      test_case "File header with chunks" `Quick test_file_header_with_chunks;
    ];
  ]

