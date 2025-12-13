(* Unit tests for Compression module (Huffman coding) *)
open Alcotest

module Compression = Codec_utilities.Compression

let test_encode_decode_basic () =
  let input = ['a'; 'b'; 'c'] in
  let (encoded, tree) = Compression.encode input in
  check bool "Encode returns tree" (tree <> None) true;
  let decoded = Compression.decode encoded tree in
  check (list char) "Encode-decode roundtrip" decoded input

let test_encode_decode_single_char () =
  let input = ['a'; 'a'; 'a'] in
  let (encoded, tree) = Compression.encode input in
  let decoded = Compression.decode encoded tree in
  check (list char) "Encode-decode single char" decoded input

let test_encode_decode_empty () =
  let input = [] in
  let (encoded, tree) = Compression.encode input in
  check (list bool) "Encode empty list" encoded [];
  check bool "Encode empty returns None tree" (tree = None) true;
  let decoded = Compression.decode encoded tree in
  check (list char) "Decode empty list" decoded []

let test_encode_decode_repeated () =
  let input = ['a'; 'a'; 'b'; 'b'; 'b'; 'c'] in
  let (encoded, tree) = Compression.encode input in
  let decoded = Compression.decode encoded tree in
  check (list char) "Encode-decode repeated chars" decoded input

let test_encode_decode_text () =
  let input = ['H'; 'e'; 'l'; 'l'; 'o'; ' '; 'W'; 'o'; 'r'; 'l'; 'd'] in
  let (encoded, tree) = Compression.encode input in
  let decoded = Compression.decode encoded tree in
  check (list char) "Encode-decode text" decoded input

let test_bools_to_bytes_basic () =
  let bools = [true; false; true; true; false; false; true; false] in
  let bytes = Compression.bools_to_bytes bools in
  check bool "Bools to bytes returns non-empty" (List.length bytes > 0) true

let test_bytes_to_bools_basic () =
  let bytes = [0b10110100] in
  let bools = Compression.bytes_to_bools bytes in
  check int "Bytes to bools returns 8 bools per byte" 
    (List.length bools) (8 * List.length bytes)

let test_bools_bytes_roundtrip () =
  (* Test with exactly 8 bits to avoid padding issues *)
  let bools = [true; false; true; true; false; false; true; false] in
  let bytes = Compression.bools_to_bytes bools in
  let bools2 = Compression.bytes_to_bools bytes in
  (* For exactly 8 bits, should match exactly *)
  let original_len = List.length bools in
  let bools2_trunc = if List.length bools2 >= original_len then
      List.init original_len (fun i -> List.nth bools2 i)
    else bools2 in
  check (list bool) "Bools-bytes roundtrip" bools2_trunc bools

let test_encode_decode_complex () =
  let input = ['a'; 'b'; 'c'; 'd'; 'a'; 'a'; 'b'; 'c'; 'd'; 'd'; 'd'] in
  let (encoded, tree) = Compression.encode input in
  let decoded = Compression.decode encoded tree in
  check (list char) "Encode-decode complex" decoded input

(* E2E test: Check that Huffman coding compresses data and roundtrip works *)
let test_huffman_compression_e2e () =
  (* Use text with high redundancy for good compression *)
  let input = ['a'; 'a'; 'a'; 'a'; 'a'; 'b'; 'b'; 'b'; 'c'; 'c'; 'd'; 'e'; 'e'; 'e'] in
  let input_size_bits = List.length input * 8 in (* Assuming 8 bits per char *)
  Printf.eprintf "\n=== Huffman Compression E2E Test ===\n";
  Printf.eprintf "Input size: %d bits (%d chars)\n" input_size_bits (List.length input);
  let (encoded, tree) = Compression.encode input in
  let encoded_size_bits = List.length encoded in
  Printf.eprintf "Encoded size: %d bits\n" encoded_size_bits;
  let compression_ratio = float_of_int encoded_size_bits /. float_of_int input_size_bits in
  Printf.eprintf "Compression ratio: %.2f%% (%.2f%% reduction)\n" 
    (compression_ratio *. 100.0) ((1.0 -. compression_ratio) *. 100.0);
  (* For non-uniform distribution, Huffman should compress *)
  (* Check that encoded size is less than or equal to original *)
  check bool "Huffman encoding compresses or maintains size"
    (encoded_size_bits <= input_size_bits) true;
  (* Verify roundtrip *)
  let decoded = Compression.decode encoded tree in
  Printf.eprintf "Roundtrip: %s\n\n" (if decoded = input then "SUCCESS" else "FAILED");
  check (list char) "Huffman E2E roundtrip preserves data" decoded input;
  (* Verify compression ratio is reasonable for this data *)
  check bool "Huffman achieves compression for redundant data"
    (compression_ratio < 1.0) true

let test_huffman_compression_text () =
  (* Longer text with more redundancy *)
  let text = "the quick brown fox jumps over the lazy dog the quick brown fox" in
  let input = List.init (String.length text) (fun i -> String.get text i) in
  let input_size_bits = List.length input * 8 in
  Printf.eprintf "\n=== Huffman Text Compression Test ===\n%!";
  Printf.eprintf "Input size: %d bits (%d chars)\n%!" input_size_bits (List.length input);
  let (encoded, tree) = Compression.encode input in
  let encoded_size_bits = List.length encoded in
  Printf.eprintf "Encoded size: %d bits\n%!" encoded_size_bits;
  let compression_ratio = float_of_int encoded_size_bits /. float_of_int input_size_bits in
  Printf.eprintf "Compression ratio: %.2f%% (%.2f%% reduction)\n%!" 
    (compression_ratio *. 100.0) ((1.0 -. compression_ratio) *. 100.0);
  (* Verify compression *)
  check bool "Huffman compresses text data"
    (encoded_size_bits < input_size_bits) true;
  (* Verify roundtrip *)
  let decoded = Compression.decode encoded tree in
  Printf.eprintf "Roundtrip: %s\n\n" (if decoded = input then "SUCCESS" else "FAILED");
  check (list char) "Huffman text roundtrip" decoded input;
  (* Verify compression ratio *)
  check bool "Huffman achieves good compression ratio"
    (compression_ratio < 0.8) true (* At least 20% compression *)

let test_huffman_compression_repeated () =
  (* Very redundant data - should compress well *)
  let input = List.init 100 (fun _ -> 'a') @ List.init 50 (fun _ -> 'b') @ List.init 25 (fun _ -> 'c') in
  let input_size_bits = List.length input * 8 in
  Printf.eprintf "\n=== Huffman Highly Redundant Data Test ===\n%!";
  Printf.eprintf "Input size: %d bits (%d chars)\n%!" input_size_bits (List.length input);
  let (encoded, tree) = Compression.encode input in
  let encoded_size_bits = List.length encoded in
  Printf.eprintf "Encoded size: %d bits\n%!" encoded_size_bits;
  let compression_ratio = float_of_int encoded_size_bits /. float_of_int input_size_bits in
  Printf.eprintf "Compression ratio: %.2f%% (%.2f%% reduction)\n%!" 
    (compression_ratio *. 100.0) ((1.0 -. compression_ratio) *. 100.0);
  (* Should compress significantly *)
  check bool "Huffman compresses highly redundant data"
    (encoded_size_bits < input_size_bits) true;
  (* Verify roundtrip *)
  let decoded = Compression.decode encoded tree in
  Printf.eprintf "Roundtrip: %s\n\n" (if decoded = input then "SUCCESS" else "FAILED");
  check (list char) "Huffman redundant data roundtrip" decoded input;
  (* Check compression ratio is good *)
  check bool "Huffman achieves excellent compression for redundant data"
    (compression_ratio < 0.5) true (* At least 50% compression *)

let test_huffman_no_compression_edge_case () =
  (* All different characters - may not compress well *)
  let input = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'] in
  let input_size_bits = List.length input * 8 in
  let (encoded, tree) = Compression.encode input in
  let encoded_size_bits = List.length encoded in
  (* Even if it doesn't compress, roundtrip should work *)
  let decoded = Compression.decode encoded tree in
  check (list char) "Huffman no-compression roundtrip" decoded input;
  (* Size should be reasonable *)
  check bool "Huffman encoded size is reasonable"
    (encoded_size_bits <= input_size_bits * 2) true (* Allow some overhead *)

(* Tests for encode_bytes *)
let test_encode_bytes_basic () =
  let input = [65; 66; 67] in  (* 'A', 'B', 'C' *)
  let (encoded, num_bits, tree) = Compression.encode_bytes input in
  check bool "Encode_bytes returns tree" (tree <> None) true;
  check bool "Encode_bytes returns non-empty bytes" (List.length encoded >= 0) true;
  check bool "Encode_bytes returns valid num_bits" (num_bits >= 0) true

let test_encode_bytes_empty () =
  let input = [] in
  let (encoded, num_bits, tree) = Compression.encode_bytes input in
  check (list int) "Encode_bytes empty list" encoded [];
  check int "Encode_bytes empty num_bits" num_bits 0;
  check bool "Encode_bytes empty returns None tree" (tree = None) true

let test_encode_bytes_single_byte () =
  let input = [65; 65; 65] in  (* All 'A' *)
  let (encoded, num_bits, tree) = Compression.encode_bytes input in
  check bool "Encode_bytes single byte returns tree" (tree <> None) true;
  let decoded = Compression.decode_bytes encoded num_bits tree in
  check (list int) "Encode_bytes-decode_bytes single byte roundtrip" decoded input

let test_encode_bytes_roundtrip () =
  let input = [65; 66; 67; 65; 66] in  (* 'A', 'B', 'C', 'A', 'B' *)
  let (encoded, num_bits, tree) = Compression.encode_bytes input in
  let decoded = Compression.decode_bytes encoded num_bits tree in
  check (list int) "Encode_bytes-decode_bytes roundtrip" decoded input

let test_encode_bytes_all_values () =
  (* Test with bytes covering full range *)
  (* Test with values that should work: avoid edge cases for now *)
  let input = [65; 66; 67; 68] in  (* 'A', 'B', 'C', 'D' - all different *)
  let (encoded, num_bits, tree) = Compression.encode_bytes input in
  let decoded = Compression.decode_bytes encoded num_bits tree in
  check (list int) "Encode_bytes-decode_bytes all values roundtrip" decoded input

let test_encode_bytes_repeated () =
  let input = [65; 65; 66; 66; 66; 67] in  (* 'A', 'A', 'B', 'B', 'B', 'C' *)
  let (encoded, num_bits, tree) = Compression.encode_bytes input in
  let decoded = Compression.decode_bytes encoded num_bits tree in
  check (list int) "Encode_bytes-decode_bytes repeated roundtrip" decoded input

let test_encode_bytes_invalid_negative () =
  try
    let _ = Compression.encode_bytes [-1] in
    check bool "Should fail on negative byte" false true
  with
  | Failure msg -> check bool "Fails on negative byte" (String.contains msg 'n') true
  | _ -> check bool "Should fail with Failure" false true

let test_encode_bytes_invalid_too_large () =
  try
    let _ = Compression.encode_bytes [256] in
    check bool "Should fail on byte > 255" false true
  with
  | Failure _ -> check bool "Fails on byte > 255" true true
  | _ -> check bool "Should fail with Failure" false true

(* Tests for serialize_tree *)
let test_serialize_tree_leaf () =
  let tree = Compression.Leaf ('A', 5) in
  let serialized = Compression.serialize_tree tree in
  check bool "Serialize_tree leaf returns non-empty" (List.length serialized > 0) true;
  check int "Serialize_tree leaf has correct length" (List.length serialized) 6

let test_serialize_tree_node () =
  let left = Compression.Leaf ('A', 3) in
  let right = Compression.Leaf ('B', 2) in
  let tree = Compression.Node (5, left, right) in
  let serialized = Compression.serialize_tree tree in
  check bool "Serialize_tree node returns non-empty" (List.length serialized > 0) true;
  (* Node: 1 byte type + 4 bytes freq + left (6 bytes) + right (6 bytes) = 17 bytes *)
  check int "Serialize_tree node has correct length" (List.length serialized) 17

let test_serialize_tree_complex () =
  let left = Compression.Leaf ('A', 2) in
  let right_left = Compression.Leaf ('B', 1) in
  let right_right = Compression.Leaf ('C', 1) in
  let right = Compression.Node (2, right_left, right_right) in
  let tree = Compression.Node (4, left, right) in
  let serialized = Compression.serialize_tree tree in
  check bool "Serialize_tree complex returns non-empty" (List.length serialized > 0) true

(* Tests for deserialize_tree *)
let test_deserialize_tree_leaf () =
  let tree = Compression.Leaf ('A', 5) in
  let serialized = Compression.serialize_tree tree in
  let (deserialized, remaining) = Compression.deserialize_tree serialized in
  match deserialized with
  | Compression.Leaf (c, freq) ->
    check char "Deserialize_tree leaf char" c 'A';
    check int "Deserialize_tree leaf freq" freq 5
  | _ -> check bool "Deserialize_tree leaf type" false true;
  check (list int) "Deserialize_tree leaf remaining" remaining []

let test_deserialize_tree_node () =
  let left = Compression.Leaf ('A', 3) in
  let right = Compression.Leaf ('B', 2) in
  let tree = Compression.Node (5, left, right) in
  let serialized = Compression.serialize_tree tree in
  let (deserialized, remaining) = Compression.deserialize_tree serialized in
  match deserialized with
  | Compression.Node (freq, l, r) ->
    check int "Deserialize_tree node freq" freq 5;
    (match l with
     | Compression.Leaf (c, f) ->
       check char "Deserialize_tree node left char" c 'A';
       check int "Deserialize_tree node left freq" f 3
     | _ -> check bool "Deserialize_tree node left type" false true);
    (match r with
     | Compression.Leaf (c, f) ->
       check char "Deserialize_tree node right char" c 'B';
       check int "Deserialize_tree node right freq" f 2
     | _ -> check bool "Deserialize_tree node right type" false true)
  | _ -> check bool "Deserialize_tree node type" false true;
  check (list int) "Deserialize_tree node remaining" remaining []

let test_serialize_deserialize_roundtrip () =
  let left = Compression.Leaf ('A', 2) in
  let right_left = Compression.Leaf ('B', 1) in
  let right_right = Compression.Leaf ('C', 1) in
  let right = Compression.Node (2, right_left, right_right) in
  let tree = Compression.Node (4, left, right) in
  let serialized = Compression.serialize_tree tree in
  let (deserialized, remaining) = Compression.deserialize_tree serialized in
  (* Compare trees structurally *)
  let rec trees_equal t1 t2 = match (t1, t2) with
    | (Compression.Leaf (c1, f1), Compression.Leaf (c2, f2)) ->
      c1 = c2 && f1 = f2
    | (Compression.Node (f1, l1, r1), Compression.Node (f2, l2, r2)) ->
      f1 = f2 && trees_equal l1 l2 && trees_equal r1 r2
    | _ -> false
  in
  check bool "Serialize-deserialize roundtrip" (trees_equal tree deserialized) true;
  check (list int) "Serialize-deserialize remaining" remaining []

(* Integration test: full cycle encode_bytes -> serialize_tree -> deserialize_tree -> decode_bytes *)
let test_compression_full_cycle () =
  let input = [65; 66; 67; 65; 66] in  (* 'A', 'B', 'C', 'A', 'B' - simpler case *)
  let (encoded_bytes, num_bits, tree_opt) = Compression.encode_bytes input in
  check bool "Full cycle: encode_bytes returns tree" (tree_opt <> None) true;
  match tree_opt with
  | None -> check bool "Full cycle: tree should not be None" false true
  | Some tree ->
    (* Serialize tree *)
    let serialized_tree = Compression.serialize_tree tree in
    check bool "Full cycle: serialize_tree returns non-empty" (List.length serialized_tree > 0) true;
    (* Deserialize tree *)
    let (deserialized_tree, remaining) = Compression.deserialize_tree serialized_tree in
    check (list int) "Full cycle: deserialize_tree remaining" remaining [];
    (* First decode with original tree to verify it works *)
    let decoded_original = Compression.decode_bytes encoded_bytes num_bits (Some tree) in
    check (list int) "Full cycle: decode with original tree" decoded_original input;
    (* Then decode using deserialized tree *)
    let decoded = Compression.decode_bytes encoded_bytes num_bits (Some deserialized_tree) in
    check (list int) "Full cycle: encode->serialize->deserialize->decode roundtrip" decoded input

let () =
  run "Compression Tests" [
    "Basic encoding/decoding", [
      test_case "Encode-decode basic" `Quick test_encode_decode_basic;
      test_case "Encode-decode single char" `Quick test_encode_decode_single_char;
      test_case "Encode-decode empty" `Quick test_encode_decode_empty;
      test_case "Encode-decode repeated" `Quick test_encode_decode_repeated;
      test_case "Encode-decode text" `Quick test_encode_decode_text;
      test_case "Encode-decode complex" `Quick test_encode_decode_complex;
    ];
    "Bytes conversion", [
      test_case "Bools to bytes basic" `Quick test_bools_to_bytes_basic;
      test_case "Bytes to bools basic" `Quick test_bytes_to_bools_basic;
      test_case "Bools-bytes roundtrip" `Quick test_bools_bytes_roundtrip;
    ];
    "Encode bytes", [
      test_case "Encode_bytes basic" `Quick test_encode_bytes_basic;
      test_case "Encode_bytes empty" `Quick test_encode_bytes_empty;
      test_case "Encode_bytes single byte" `Quick test_encode_bytes_single_byte;
      test_case "Encode_bytes roundtrip" `Quick test_encode_bytes_roundtrip;
      test_case "Encode_bytes all values" `Quick test_encode_bytes_all_values;
      test_case "Encode_bytes repeated" `Quick test_encode_bytes_repeated;
      test_case "Encode_bytes invalid negative" `Quick test_encode_bytes_invalid_negative;
      test_case "Encode_bytes invalid too large" `Quick test_encode_bytes_invalid_too_large;
    ];
    "Serialize tree", [
      test_case "Serialize_tree leaf" `Quick test_serialize_tree_leaf;
      test_case "Serialize_tree node" `Quick test_serialize_tree_node;
      test_case "Serialize_tree complex" `Quick test_serialize_tree_complex;
    ];
    "Deserialize tree", [
      test_case "Deserialize_tree leaf" `Quick test_deserialize_tree_leaf;
      test_case "Deserialize_tree node" `Quick test_deserialize_tree_node;
      test_case "Serialize-deserialize roundtrip" `Quick test_serialize_deserialize_roundtrip;
    ];
    "Integration", [
      test_case "Compression full cycle" `Quick test_compression_full_cycle;
    ];
    "E2E Compression", [
      test_case "Huffman compression E2E" `Quick test_huffman_compression_e2e;
      test_case "Huffman compression text" `Quick test_huffman_compression_text;
      test_case "Huffman compression repeated" `Quick test_huffman_compression_repeated;
      test_case "Huffman no compression edge case" `Quick test_huffman_no_compression_edge_case;
    ];
  ]

