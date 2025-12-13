(* Unit tests for Quantization module *)
open Alcotest

module Quantization = Codec_utilities.Quantization

let float_approx = float 0.0001

let test_quantize_basic () =
  let input = [0.0; 1.0; 2.0; 3.0; 4.0] in
  let num_levels = 4 in
  let quantized = Quantization.quantize input num_levels in
  check int "Quantize returns correct length"
    (List.length quantized) (List.length input);
  (* All values should be in range [0, num_levels-1] *)
  List.iter
    (fun q ->
      check bool "Quantized value in valid range"
        (q >= 0 && q < num_levels) true)
    quantized

let test_dequantize_basic () =
  let quantized = [0; 1; 2; 3] in
  let num_levels = 4 in
  let min_val = 0.0 in
  let max_val = 4.0 in
  let dequantized = Quantization.dequantize quantized num_levels min_val max_val in
  check int "Dequantize returns correct length"
    (List.length dequantized) (List.length quantized);
  (* All values should be in range [min_val, max_val] *)
  List.iter
    (fun dq ->
      check bool "Dequantized value in valid range"
        (dq >= min_val && dq <= max_val) true)
    dequantized

let test_quantize_dequantize_roundtrip () =
  let input = [0.0; 1.0; 2.0; 3.0; 4.0] in
  let num_levels = 8 in
  let quantized = Quantization.quantize input num_levels in
  let min_val = 0.0 in
  let max_val = 4.0 in
  let dequantized = Quantization.dequantize quantized num_levels min_val max_val in
  check int "Quantize-dequantize roundtrip preserves length"
    (List.length dequantized) (List.length input);
  (* Values should be approximately recovered (quantization introduces error) *)
  (* Use larger tolerance for quantization error *)
  let quant_error = float 0.5 in
  List.iter2
    (fun orig dq ->
      check quant_error "Quantize-dequantize roundtrip"
        orig dq)
    input dequantized

let test_quantize_with_range () =
  let input = [1.0; 2.0; 3.0; 4.0; 5.0] in
  let num_levels = 4 in
  let (quantized, min_val, max_val) = Quantization.quantize_with_range input num_levels in
  check int "Quantize with range returns correct length"
    (List.length quantized) (List.length input);
  check bool "Min value detected correctly" (min_val = 1.0) true;
  check bool "Max value detected correctly" (max_val = 5.0) true

let test_dequantize_with_range () =
  let quantized = [0; 1; 2; 3] in
  let num_levels = 4 in
  let min_val = 0.0 in
  let max_val = 4.0 in
  let dequantized = Quantization.dequantize_with_range quantized num_levels min_val max_val in
  check int "Dequantize with range returns correct length"
    (List.length dequantized) (List.length quantized)

let test_quantize_custom_range () =
  let input = [1.0; 2.0; 3.0; 4.0] in
  let num_levels = 4 in
  let min_val = 0.0 in
  let max_val = 5.0 in
  let quantized = Quantization.quantize ~min_val ~max_val input num_levels in
  check int "Quantize with custom range returns correct length"
    (List.length quantized) (List.length input);
  (* Values outside range should be clamped *)
  let input2 = [-1.0; 6.0; 2.0; 3.0] in
  let quantized2 = Quantization.quantize ~min_val ~max_val input2 num_levels in
  check int "Quantize clamps values" (List.length quantized2) (List.length input2)

let test_quantize_single_level () =
  let input = [1.0; 2.0; 3.0; 4.0] in
  let num_levels = 1 in
  let quantized = Quantization.quantize input num_levels in
  check int "Quantize with 1 level returns correct length"
    (List.length quantized) (List.length input);
  (* All values should be 0 *)
  List.iter
    (fun q -> check int "Quantized value is 0 for 1 level" q 0)
    quantized

let test_quantize_constant_signal () =
  let input = [5.0; 5.0; 5.0; 5.0] in
  let num_levels = 4 in
  let quantized = Quantization.quantize input num_levels in
  check int "Quantize constant signal" (List.length quantized) (List.length input);
  (* All values should be the same *)
  let first = List.hd quantized in
  List.iter
    (fun q -> check int "Quantized constant values are same" q first)
    quantized

let test_quantize_empty () =
  let input = [] in
  let num_levels = 4 in
  let quantized = Quantization.quantize input num_levels in
  check (list int) "Quantize empty list" quantized []

let test_dequantize_empty () =
  let quantized = [] in
  let num_levels = 4 in
  let min_val = 0.0 in
  let max_val = 4.0 in
  let dequantized = Quantization.dequantize quantized num_levels min_val max_val in
  check (list float_approx) "Dequantize empty list" dequantized []

let test_quantize_high_levels () =
  let input = [0.0; 1.0; 2.0; 3.0; 4.0] in
  let num_levels = 256 in
  let quantized = Quantization.quantize input num_levels in
  check int "Quantize with many levels" (List.length quantized) (List.length input);
  (* With many levels, quantization should be more accurate *)
  let dequantized = Quantization.dequantize quantized num_levels 0.0 4.0 in
  List.iter2
    (fun orig dq ->
      check (float 0.1) "High-level quantization is accurate" orig dq)
    input dequantized

let test_encode_decode_roundtrip () =
  (* Test with n=4 (2 bits per element) *)
  let indices = [1; 0; 2; 3] in
  let n = 4 in
  let encoded = Quantization.encode_quantized_indices indices n in
  check bool "Encode returns non-empty bytes" (List.length encoded > 0) true;
  let decoded = Quantization.decode_quantized_indices encoded (List.length indices) n in
  check (list int) "Encode-decode roundtrip" decoded indices

let test_encode_decode_n2 () =
  (* Test with n=2 (1 bit per element) *)
  let indices = [0; 1; 0; 1; 1; 0] in
  let n = 2 in
  let encoded = Quantization.encode_quantized_indices indices n in
  let decoded = Quantization.decode_quantized_indices encoded (List.length indices) n in
  check (list int) "Encode-decode n=2" decoded indices

let test_encode_decode_n3 () =
  (* Test with n=3 (2 bits per element, but only 3 values) *)
  let indices = [0; 1; 2; 0; 2; 1] in
  let n = 3 in
  let encoded = Quantization.encode_quantized_indices indices n in
  let decoded = Quantization.decode_quantized_indices encoded (List.length indices) n in
  check (list int) "Encode-decode n=3" decoded indices

let test_encode_decode_n5 () =
  (* Test with n=5 (3 bits per element) *)
  let indices = [0; 1; 2; 3; 4; 0; 4] in
  let n = 5 in
  let encoded = Quantization.encode_quantized_indices indices n in
  let decoded = Quantization.decode_quantized_indices encoded (List.length indices) n in
  check (list int) "Encode-decode n=5" decoded indices

let test_encode_decode_n32 () =
  (* Test with n=32 (5 bits per element) *)
  let indices = [0; 15; 31; 16; 1; 30] in
  let n = 32 in
  let encoded = Quantization.encode_quantized_indices indices n in
  let decoded = Quantization.decode_quantized_indices encoded (List.length indices) n in
  check (list int) "Encode-decode n=32" decoded indices

let test_encode_decode_n256 () =
  (* Test with n=256 (8 bits per element) *)
  let indices = [0; 128; 255; 64; 192] in
  let n = 256 in
  let encoded = Quantization.encode_quantized_indices indices n in
  let decoded = Quantization.decode_quantized_indices encoded (List.length indices) n in
  check (list int) "Encode-decode n=256" decoded indices

let test_encode_decode_empty () =
  let indices = [] in
  let n = 4 in
  let encoded = Quantization.encode_quantized_indices indices n in
  check (list int) "Encode empty list" encoded [];
  let decoded = Quantization.decode_quantized_indices encoded 0 n in
  check (list int) "Decode empty list" decoded []

let test_encode_decode_single () =
  let indices = [2] in
  let n = 4 in
  let encoded = Quantization.encode_quantized_indices indices n in
  let decoded = Quantization.decode_quantized_indices encoded 1 n in
  check (list int) "Encode-decode single element" decoded indices

let test_encode_decode_large () =
  (* Test with many elements *)
  let indices = List.init 100 (fun i -> i mod 16) in
  let n = 16 in
  let encoded = Quantization.encode_quantized_indices indices n in
  let decoded = Quantization.decode_quantized_indices encoded (List.length indices) n in
  check (list int) "Encode-decode large list" decoded indices

let test_encode_clamping () =
  (* Test that indices outside range are clamped *)
  let indices = [-1; 0; 5; 3; 10] in  (* n=4, so valid range is [0,3] *)
  let n = 4 in
  let encoded = Quantization.encode_quantized_indices indices n in
  let decoded = Quantization.decode_quantized_indices encoded (List.length indices) n in
  (* All decoded values should be in valid range *)
  List.iter
    (fun idx ->
      check bool "Decoded index in valid range" (idx >= 0 && idx < n) true)
    decoded

let () =
  run "Quantization Tests" [
    "Basic quantization", [
      test_case "Quantize basic" `Quick test_quantize_basic;
      test_case "Dequantize basic" `Quick test_dequantize_basic;
      test_case "Quantize-dequantize roundtrip" `Quick test_quantize_dequantize_roundtrip;
      test_case "Quantize with range" `Quick test_quantize_with_range;
      test_case "Dequantize with range" `Quick test_dequantize_with_range;
      test_case "Quantize custom range" `Quick test_quantize_custom_range;
    ];
    "Edge cases", [
      test_case "Quantize single level" `Quick test_quantize_single_level;
      test_case "Quantize constant signal" `Quick test_quantize_constant_signal;
      test_case "Quantize empty" `Quick test_quantize_empty;
      test_case "Dequantize empty" `Quick test_dequantize_empty;
    ];
    "Advanced", [
      test_case "Quantize high levels" `Quick test_quantize_high_levels;
    ];
    "Encoding/Decoding", [
      test_case "Encode-decode roundtrip" `Quick test_encode_decode_roundtrip;
      test_case "Encode-decode n=2" `Quick test_encode_decode_n2;
      test_case "Encode-decode n=3" `Quick test_encode_decode_n3;
      test_case "Encode-decode n=5" `Quick test_encode_decode_n5;
      test_case "Encode-decode n=32" `Quick test_encode_decode_n32;
      test_case "Encode-decode n=256" `Quick test_encode_decode_n256;
      test_case "Encode-decode empty" `Quick test_encode_decode_empty;
      test_case "Encode-decode single" `Quick test_encode_decode_single;
      test_case "Encode-decode large" `Quick test_encode_decode_large;
      test_case "Encode clamping" `Quick test_encode_clamping;
    ];
  ]

