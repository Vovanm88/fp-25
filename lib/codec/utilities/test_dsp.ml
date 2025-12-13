(* Unit tests for DSP module *)
open Alcotest

module Dsp = Lab4_mlac.Codec.Dsp

let float_approx = float 0.0001

let test_dft_basic () =
  let input = [1.0; 2.0; 3.0; 4.0] in
  let result = Dsp.dft_transform input in
  check (list float_approx) "DFT returns list of same length"
    (List.length result) (List.length input);
  check (list float_approx) "DFT returns non-negative values"
    (List.map (fun x -> if x >= 0.0 then 1.0 else 0.0) result)
    (List.map (fun _ -> 1.0) result)

let test_idft_basic () =
  let input = [1.0; 2.0; 3.0; 4.0] in
  let result = Dsp.idft_transform input in
  check (list float_approx) "IDFT returns list of same length"
    (List.length result) (List.length input)

let test_fft_basic () =
  let input = [1.0; 2.0; 3.0; 4.0] in
  let result = Dsp.fft_transform input in
  check (list float_approx) "FFT returns list of same length"
    (List.length result) (List.length input)

let test_ifft_basic () =
  let input = [1.0; 2.0; 3.0; 4.0] in
  let result = Dsp.ifft_transform input in
  check (list float_approx) "IFFT returns list of same length"
    (List.length result) (List.length input)

let test_fft_roundtrip () =
  let input = [1.0; 2.0; 3.0; 4.0] in
  let fft_result = Dsp.fft_transform input in
  let ifft_result = Dsp.ifft_transform fft_result in
  check (list float_approx) "FFT-IFFT roundtrip preserves length"
    (List.length ifft_result) (List.length input)

let test_mdct_basic () =
  let input = [1.0; 2.0; 3.0; 4.0] in
  let result = Dsp.mdct_transform input in
  let expected_len = List.length input / 2 in
  check int "MDCT returns half the length"
    (List.length result) expected_len

let test_imdct_basic () =
  let input = [1.0; 2.0] in
  let result = Dsp.imdct_transform input in
  let expected_len = 2 * List.length input in
  check int "IMDCT returns double the length"
    (List.length result) expected_len

let test_fft_shift () =
  let input = [1.0; 2.0; 3.0; 4.0] in
  let result = Dsp.fft_shift input in
  check (list float_approx) "FFT shift preserves length"
    (List.length result) (List.length input);
  check (list float_approx) "FFT shift moves elements"
    result [3.0; 4.0; 1.0; 2.0]

let test_hamming_window () =
  let input = [1.0; 1.0; 1.0; 1.0] in
  let result = Dsp.hamming_window input in
  check (list float_approx) "Hamming window preserves length"
    (List.length result) (List.length input);
  (* Hamming window should modify values *)
  check bool "Hamming window modifies values"
    (result <> input) true

let test_hanning_window () =
  let input = [1.0; 1.0; 1.0; 1.0] in
  let result = Dsp.hanning_window input in
  check (list float_approx) "Hanning window preserves length"
    (List.length result) (List.length input);
  (* Hanning window should modify values *)
  check bool "Hanning window modifies values"
    (result <> input) true

let test_rectangular_window () =
  let input = [1.0; 2.0; 3.0; 4.0] in
  let result = Dsp.rectangular_window input in
  check (list float_approx) "Rectangular window returns same list"
    result input

let test_empty_list () =
  let empty = [] in
  check (list float_approx) "DFT on empty list" (Dsp.dft_transform empty) [];
  check (list float_approx) "FFT on empty list" (Dsp.fft_transform empty) [];
  check (list float_approx) "Hamming window on empty list"
    (Dsp.hamming_window empty) []

let test_single_element () =
  let single = [5.0] in
  check (list float_approx) "DFT on single element"
    (Dsp.dft_transform single) [5.0];
  check (list float_approx) "FFT on single element"
    (Dsp.fft_transform single) [5.0];
  check (list float_approx) "Hamming window on single element"
    (Dsp.hamming_window single) [5.0]

let test_window_edge_cases () =
  (* Test with n=1 for windows *)
  let single = [1.0] in
  check (list float_approx) "Hamming window single element"
    (Dsp.hamming_window single) [1.0];
  check (list float_approx) "Hanning window single element"
    (Dsp.hanning_window single) [1.0]

let () =
  run "DSP Tests" [
    "Basic transforms", [
      test_case "DFT basic" `Quick test_dft_basic;
      test_case "IDFT basic" `Quick test_idft_basic;
      test_case "FFT basic" `Quick test_fft_basic;
      test_case "IFFT basic" `Quick test_ifft_basic;
      test_case "FFT roundtrip" `Quick test_fft_roundtrip;
    ];
    "MDCT transforms", [
      test_case "MDCT basic" `Quick test_mdct_basic;
      test_case "IMDCT basic" `Quick test_imdct_basic;
    ];
    "FFT shift", [
      test_case "FFT shift" `Quick test_fft_shift;
    ];
    "Window functions", [
      test_case "Hamming window" `Quick test_hamming_window;
      test_case "Hanning window" `Quick test_hanning_window;
      test_case "Rectangular window" `Quick test_rectangular_window;
      test_case "Window edge cases" `Quick test_window_edge_cases;
    ];
    "Edge cases", [
      test_case "Empty list" `Quick test_empty_list;
      test_case "Single element" `Quick test_single_element;
    ];
  ]

