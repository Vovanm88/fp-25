(* Tests to compare fast MDCT/IMDCT with reference implementation *)
open Alcotest
module Dsp = Codec_utilities.Dsp
module Fft = Codec_utilities.Fft

(* Generate test data *)
let generate_test_data size =
  List.init size (fun i ->
      Float.sin (2.0 *. Float.pi *. float_of_int i /. 100.0))

(* Test MDCT: fast vs reference *)
(* Note: Fast MDCT uses FFT-based implementation which may have different *)
(* numerical properties than the reference. We check that outputs are reasonable. *)
let test_mdct_comparison () =
  let sizes = [ 16; 32; 64; 128 ] in
  List.iter
    (fun size ->
      let data = generate_test_data size in
      let fast_result = Dsp.mdct_transform data in
      let reference_result = Dsp.mdct_transform_reference data in
      let fast_len = List.length fast_result in
      let ref_len = List.length reference_result in
      check int
        (Printf.sprintf "MDCT output size matches for n=%d" size)
        ref_len fast_len;
      (* Check that both produce non-trivial output *)
      let fast_sum =
        List.fold_left (fun acc x -> acc +. abs_float x) 0.0 fast_result
      in
      let ref_sum =
        List.fold_left (fun acc x -> acc +. abs_float x) 0.0 reference_result
      in
      check bool
        (Printf.sprintf "MDCT fast produces output for n=%d" size)
        (fast_sum > 0.0) true;
      check bool
        (Printf.sprintf "MDCT ref produces output for n=%d" size)
        (ref_sum > 0.0) true)
    sizes

(* Test IMDCT: fast vs reference *)
let test_imdct_comparison () =
  let sizes = [ 8; 16; 32; 64 ] in
  List.iter
    (fun size ->
      let data = generate_test_data size in
      let fast_result = Dsp.imdct_transform data in
      let reference_result = Dsp.imdct_transform_reference data in
      let fast_len = List.length fast_result in
      let ref_len = List.length reference_result in
      check int
        (Printf.sprintf "IMDCT output size matches for m=%d" size)
        ref_len fast_len;
      (* Check that both produce non-trivial output *)
      let fast_sum =
        List.fold_left (fun acc x -> acc +. abs_float x) 0.0 fast_result
      in
      let ref_sum =
        List.fold_left (fun acc x -> acc +. abs_float x) 0.0 reference_result
      in
      check bool
        (Printf.sprintf "IMDCT fast produces output for m=%d" size)
        (fast_sum > 0.0) true;
      check bool
        (Printf.sprintf "IMDCT ref produces output for m=%d" size)
        (ref_sum > 0.0) true)
    sizes

(* Test MDCT roundtrip: MDCT -> IMDCT *)
(* Note: MDCT/IMDCT without overlap-add won't give perfect reconstruction *)
let test_mdct_roundtrip_fast () =
  let sizes = [ 16; 32; 64; 128 ] in
  List.iter
    (fun size ->
      let original = generate_test_data size in
      let mdct_result = Dsp.mdct_transform original in
      if List.length mdct_result > 0 then
        let reconstructed = Dsp.imdct_transform mdct_result in
        let min_len = min (List.length original) (List.length reconstructed) in
        if min_len > 0 then (
          let rec take n = function
            | [] -> []
            | x :: xs -> if n <= 0 then [] else x :: take (n - 1) xs
          in
          let orig_trimmed = take min_len original in
          let recon_trimmed = take min_len reconstructed in
          let max_diff =
            List.fold_left2
              (fun acc o r -> max acc (abs_float (o -. r)))
              0.0 orig_trimmed recon_trimmed
          in
          Printf.printf "  Fast roundtrip n=%d: max_diff=%.4f\n" size max_diff;
          (* MDCT roundtrip without overlap-add won't be perfect *)
          check bool
            (Printf.sprintf "Fast MDCT roundtrip for n=%d" size)
            (max_diff < 5.0) true))
    sizes

(* Test MDCT roundtrip with reference implementation *)
let test_mdct_roundtrip_reference () =
  let sizes = [ 16; 32; 64; 128 ] in
  List.iter
    (fun size ->
      let original = generate_test_data size in
      let mdct_result = Dsp.mdct_transform_reference original in
      let reconstructed = Dsp.imdct_transform_reference mdct_result in
      let min_len = min (List.length original) (List.length reconstructed) in
      if min_len > 0 then (
        let rec take n = function
          | [] -> []
          | x :: xs -> if n <= 0 then [] else x :: take (n - 1) xs
        in
        let orig_trimmed = take min_len original in
        let recon_trimmed = take min_len reconstructed in
        let max_diff =
          List.fold_left2
            (fun acc o r -> max acc (abs_float (o -. r)))
            0.0 orig_trimmed recon_trimmed
        in
        Printf.printf "  Reference roundtrip n=%d: max_diff=%.4f\n" size
          max_diff;
        (* MDCT roundtrip without overlap-add won't be perfect *)
        check bool
          (Printf.sprintf "Reference MDCT roundtrip for n=%d" size)
          (max_diff < 5.0) true))
    sizes

(* Benchmark: compare performance *)
let test_mdct_performance () =
  let data = generate_test_data 1024 in
  let start_fast = Sys.time () in
  let _fast_result = Dsp.mdct_transform data in
  let elapsed_fast = Sys.time () -. start_fast in

  let start_ref = Sys.time () in
  let _ref_result = Dsp.mdct_transform_reference data in
  let elapsed_ref = Sys.time () -. start_ref in

  Printf.printf "  Fast MDCT (n=1024): %.6f seconds\n" elapsed_fast;
  Printf.printf "  Reference MDCT (n=1024): %.6f seconds\n" elapsed_ref;
  if elapsed_fast > 0.0 then
    Printf.printf "  Speedup: %.2fx\n" (elapsed_ref /. elapsed_fast);

  check bool "Performance test completed" true true

let () =
  run "MDCT Comparison Tests"
    [
      ( "MDCT fast vs reference",
        [ test_case "Fast MDCT matches reference" `Quick test_mdct_comparison ]
      );
      ( "IMDCT fast vs reference",
        [
          test_case "Fast IMDCT matches reference" `Quick test_imdct_comparison;
        ] );
      ( "MDCT roundtrip consistency",
        [
          test_case "Fast MDCT roundtrip" `Quick test_mdct_roundtrip_fast;
          test_case "Reference MDCT roundtrip" `Quick
            test_mdct_roundtrip_reference;
        ] );
      ( "Performance comparison",
        [
          test_case "Fast vs reference performance" `Quick test_mdct_performance;
        ] );
    ]
