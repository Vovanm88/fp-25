(* Tests to compare fast MDCT/IMDCT with reference implementation *)
open Alcotest

module Dsp = Codec_utilities.Dsp

(* Generate test data *)
let generate_test_data size =
  List.init size (fun i -> Float.sin (2.0 *. Float.pi *. float_of_int i /. 100.0))

(* Test MDCT: fast vs reference *)
let test_mdct_comparison () =
  let sizes = [64; 128; 256; 512; 1024; 2048] in
  List.iter (fun size ->
    let data = generate_test_data size in
    let fast_result = Dsp.mdct_transform data in
    let reference_result = Dsp.mdct_transform_reference data in
    (* Compare results - should be very close (within floating point precision) *)
    List.iter2 (fun fast ref ->
      let diff = abs_float (fast -. ref) in
      check bool (Printf.sprintf "MDCT fast matches reference (size %d)" size) 
        (diff < 1e-10 || (abs_float fast < 1e-10 && abs_float ref < 1e-10)) true
    ) fast_result reference_result
  ) sizes

(* Test IMDCT: fast vs reference *)
let test_imdct_comparison () =
  let sizes = [32; 64; 128; 256; 512; 1024] in
  List.iter (fun size ->
    let data = generate_test_data size in
    let fast_result = Dsp.imdct_transform data in
    let reference_result = Dsp.imdct_transform_reference data in
    (* Compare results - should be very close *)
    List.iter2 (fun fast ref ->
      let diff = abs_float (fast -. ref) in
      check bool (Printf.sprintf "IMDCT fast matches reference (size %d)" size) 
        (diff < 1e-10 || (abs_float fast < 1e-10 && abs_float ref < 1e-10)) true
    ) fast_result reference_result
  ) sizes

(* Test MDCT roundtrip: MDCT -> IMDCT should reconstruct original *)
let test_mdct_roundtrip_fast () =
  let sizes = [64; 128; 256; 512] in
  List.iter (fun size ->
    let original = generate_test_data size in
    let mdct_result = Dsp.mdct_transform original in
    let reconstructed = Dsp.imdct_transform mdct_result in
    (* Reconstructed should match original (within precision) *)
    let min_len = min (List.length original) (List.length reconstructed) in
    let rec take n = function
      | [] -> []
      | x :: xs -> if n <= 0 then [] else x :: take (n - 1) xs
    in
    let orig_trimmed = take min_len original in
    let recon_trimmed = take min_len reconstructed in
    List.iter2 (fun orig recon ->
      let diff = abs_float (orig -. recon) in
      check bool (Printf.sprintf "MDCT roundtrip preserves signal (size %d)" size)
        (diff < 0.01) true  (* Allow some error due to MDCT/IMDCT precision *)
    ) orig_trimmed recon_trimmed
  ) sizes

(* Test MDCT roundtrip with reference implementation *)
let test_mdct_roundtrip_reference () =
  let sizes = [64; 128; 256; 512] in
  List.iter (fun size ->
    let original = generate_test_data size in
    let mdct_result = Dsp.mdct_transform_reference original in
    let reconstructed = Dsp.imdct_transform_reference mdct_result in
    (* Reconstructed should match original *)
    let min_len = min (List.length original) (List.length reconstructed) in
    let rec take n = function
      | [] -> []
      | x :: xs -> if n <= 0 then [] else x :: take (n - 1) xs
    in
    let orig_trimmed = take min_len original in
    let recon_trimmed = take min_len reconstructed in
    List.iter2 (fun orig recon ->
      let diff = abs_float (orig -. recon) in
      check bool (Printf.sprintf "MDCT reference roundtrip preserves signal (size %d)" size)
        (diff < 0.01) true
    ) orig_trimmed recon_trimmed
  ) sizes

(* Benchmark: compare performance *)
let test_mdct_performance () =
  let data = generate_test_data 1024 in
  let start_fast = Sys.time () in
  let _fast_result = Dsp.mdct_transform data in
  let elapsed_fast = Sys.time () -. start_fast in
  
  let start_ref = Sys.time () in
  let _ref_result = Dsp.mdct_transform_reference data in
  let elapsed_ref = Sys.time () -. start_ref in
  
  Printf.printf "  Fast MDCT: %.6f seconds\n" elapsed_fast;
  Printf.printf "  Reference MDCT: %.6f seconds\n" elapsed_ref;
  Printf.printf "  Speedup: %.2fx\n" (elapsed_ref /. elapsed_fast);
  
  (* Fast should be at least as fast as reference (or faster) *)
  check bool "Fast MDCT is not slower than reference" (elapsed_fast <= elapsed_ref *. 1.5) true

let () =
  run "MDCT Comparison Tests" [
    "MDCT fast vs reference", [
      test_case "Fast MDCT matches reference" `Quick test_mdct_comparison;
    ];
    "IMDCT fast vs reference", [
      test_case "Fast IMDCT matches reference" `Quick test_imdct_comparison;
    ];
    "MDCT roundtrip", [
      test_case "Fast MDCT roundtrip" `Quick test_mdct_roundtrip_fast;
      test_case "Reference MDCT roundtrip" `Quick test_mdct_roundtrip_reference;
    ];
    "Performance comparison", [
      test_case "Fast vs reference performance" `Quick test_mdct_performance;
    ];
  ]

