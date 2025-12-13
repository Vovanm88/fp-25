(* Benchmark comparing fast MDCT (O(n log n)) vs reference MDCT (O(n²)) *)

module Dsp = Codec_utilities.Dsp

(* Generate test data *)
let generate_test_data size =
  List.init size (fun i -> Float.sin (2.0 *. Float.pi *. float_of_int i /. 100.0))

(* Measure time *)
let time_function f =
  let start_time = Sys.time () in
  let result = f () in
  let end_time = Sys.time () in
  (end_time -. start_time, result)

(* Benchmark MDCT implementations *)
let bench_mdct_comparison () =
  Printf.printf "\n=== MDCT Performance Comparison ===\n\n";
  Printf.printf "Size (n) | Fast (s) | Ref (s)  | Speedup | Complexity\n";
  Printf.printf "---------|----------|----------|---------|------------\n";
  
  let sizes = [16; 64; 256; 512] in
  
  List.iter (fun size ->
    let data = generate_test_data size in
    
    (* Benchmark fast implementation *)
    let (fast_time, fast_result) = time_function (fun () ->
      Dsp.mdct_transform data
    ) in
    
    (* Benchmark reference implementation *)
    let (ref_time, ref_result) = time_function (fun () ->
      Dsp.mdct_transform_reference data
    ) in
    
    (* Calculate speedup *)
    let speedup = if fast_time > 0.0 then ref_time /. fast_time else 0.0 in
    
    (* Verify correctness *)
    let max_diff = if List.length fast_result = List.length ref_result then
      List.fold_left2 (fun acc f r -> max acc (abs_float (f -. r)))
        0.0 fast_result ref_result
    else infinity in
    
    let complexity = if size >= 64 then
      let expected_ratio = (float_of_int size *. log (float_of_int size)) /. 
                          (float_of_int size *. float_of_int size) in
      let actual_ratio = fast_time /. ref_time in
      Printf.sprintf "%.4f" (actual_ratio /. expected_ratio)
    else "-" in
    
    Printf.printf "  %5d  | %8.6f | %8.6f | %7.2fx | %s\n"
      size fast_time ref_time speedup complexity;
    
    if max_diff > 1e-6 && max_diff < infinity then
      Printf.printf "         | WARNING: max_diff = %.2e\n" max_diff
  ) sizes;
  
  Printf.printf "\n"

(* Benchmark IMDCT implementations *)
let bench_imdct_comparison () =
  Printf.printf "=== IMDCT Performance Comparison ===\n\n";
  Printf.printf "Size (m) | Fast (s) | Ref (s)  | Speedup\n";
  Printf.printf "---------|----------|----------|---------\n";
  
  let sizes = [8; 32; 128; 256] in
  
  List.iter (fun size ->
    let data = generate_test_data size in
    
    (* Benchmark fast implementation *)
    let (fast_time, _fast_result) = time_function (fun () ->
      Dsp.imdct_transform data
    ) in
    
    (* Benchmark reference implementation *)
    let (ref_time, _ref_result) = time_function (fun () ->
      Dsp.imdct_transform_reference data
    ) in
    
    (* Calculate speedup *)
    let speedup = if fast_time > 0.0 then ref_time /. fast_time else 0.0 in
    
    Printf.printf "  %5d  | %8.6f | %8.6f | %7.2fx\n"
      size fast_time ref_time speedup
  ) sizes;
  
  Printf.printf "\n"

(* Benchmark roundtrip: MDCT -> IMDCT *)
let bench_roundtrip () =
  Printf.printf "=== MDCT Roundtrip Performance ===\n\n";
  Printf.printf "Size (n) | Fast (s) | Ref (s)  | Speedup\n";
  Printf.printf "---------|----------|----------|---------\n";
  
  let sizes = [16; 64; 256; 512] in
  
  List.iter (fun size ->
    let data = generate_test_data size in
    
    (* Benchmark fast roundtrip *)
    let (fast_time, _) = time_function (fun () ->
      let mdct = Dsp.mdct_transform data in
      Dsp.imdct_transform mdct
    ) in
    
    (* Benchmark reference roundtrip *)
    let (ref_time, _) = time_function (fun () ->
      let mdct = Dsp.mdct_transform_reference data in
      Dsp.imdct_transform_reference mdct
    ) in
    
    (* Calculate speedup *)
    let speedup = if fast_time > 0.0 then ref_time /. fast_time else 0.0 in
    
    Printf.printf "  %5d  | %8.6f | %8.6f | %7.2fx\n"
      size fast_time ref_time speedup
  ) sizes;
  
  Printf.printf "\n"

(* Test with realistic audio sizes *)
let bench_realistic_sizes () =
  Printf.printf "=== Realistic Audio Sizes ===\n\n";
  Printf.printf "Duration | Samples | Fast (s) | Ref (s)  | Speedup\n";
  Printf.printf "---------|---------|----------|----------|---------\n";
  
  let sample_rate = 44100 in
  let durations = [0.01; 0.05; 0.1] in (* seconds *)
  
  List.iter (fun duration ->
    let num_samples = int_of_float (duration *. float_of_int sample_rate) in
    (* Round to nearest power of 2 for MDCT *)
    let rec nearest_pow2 n acc =
      if acc >= n then acc
      else nearest_pow2 n (acc * 2)
    in
    let size = nearest_pow2 num_samples 16 in
    let data = generate_test_data size in
    
    (* Benchmark fast *)
    let (fast_time, _) = time_function (fun () ->
      let mdct = Dsp.mdct_transform data in
      Dsp.imdct_transform mdct
    ) in
    
    (* Benchmark reference *)
    let (ref_time, _) = time_function (fun () ->
      let mdct = Dsp.mdct_transform_reference data in
      Dsp.imdct_transform_reference mdct
    ) in
    
    let speedup = if fast_time > 0.0 then ref_time /. fast_time else 0.0 in
    
    Printf.printf " %5.2fs  | %7d | %8.6f | %8.6f | %7.2fx\n"
      duration size fast_time ref_time speedup
  ) durations;
  
  Printf.printf "\n"

let () =
  bench_mdct_comparison ();
  bench_imdct_comparison ();
  bench_roundtrip ();
  bench_realistic_sizes ();
  Printf.printf "===================================\n\n"

