(* Benchmark comparing fast DFT (O(n log n)) vs reference DFT (O(n²)) *)

module Dsp = Codec_utilities.Dsp

(* Generate test data *)
let generate_test_data size =
  List.init size (fun i ->
      Float.sin (2.0 *. Float.pi *. float_of_int i /. 100.0))

(* Measure time *)
let time_function f =
  let start_time = Sys.time () in
  let result = f () in
  let end_time = Sys.time () in
  (end_time -. start_time, result)

(* Benchmark DFT implementations *)
let bench_dft_comparison () =
  Printf.printf "\n=== DFT Performance Comparison ===\n\n";
  Printf.printf "Size (n) | Fast (s) | Ref (s)  | Speedup\n";
  Printf.printf "---------|----------|----------|---------\n";

  let sizes = [ 16; 64; 256; 512 ] in

  List.iter
    (fun size ->
      let data = generate_test_data size in

      (* Benchmark fast implementation *)
      let fast_time, _fast_result =
        time_function (fun () -> Dsp.dft_transform data)
      in

      (* Benchmark reference implementation *)
      let ref_time, _ref_result =
        time_function (fun () -> Dsp.dft_transform_reference data)
      in

      (* Calculate speedup *)
      let speedup = if fast_time > 0.0 then ref_time /. fast_time else 0.0 in

      Printf.printf "  %5d  | %8.6f | %8.6f | %7.2fx\n" size fast_time ref_time
        speedup)
    sizes;

  Printf.printf "\n"

(* Benchmark IDFT implementations *)
let bench_idft_comparison () =
  Printf.printf "=== IDFT Performance Comparison ===\n\n";
  Printf.printf "Size (n) | Fast (s) | Ref (s)  | Speedup\n";
  Printf.printf "---------|----------|----------|---------\n";

  let sizes = [ 16; 64; 256; 512 ] in

  List.iter
    (fun size ->
      let data = generate_test_data size in
      let dft_data = Dsp.dft_transform_reference data in

      (* Benchmark fast implementation *)
      let fast_time, _fast_result =
        time_function (fun () -> Dsp.idft_transform dft_data)
      in

      (* Benchmark reference implementation *)
      let ref_time, _ref_result =
        time_function (fun () -> Dsp.idft_transform_reference dft_data)
      in

      (* Calculate speedup *)
      let speedup = if fast_time > 0.0 then ref_time /. fast_time else 0.0 in

      Printf.printf "  %5d  | %8.6f | %8.6f | %7.2fx\n" size fast_time ref_time
        speedup)
    sizes;

  Printf.printf "\n"

(* Benchmark roundtrip: DFT -> IDFT *)
let bench_roundtrip () =
  Printf.printf "=== DFT Roundtrip Performance ===\n\n";
  Printf.printf "Size (n) | Fast (s) | Ref (s)  | Speedup\n";
  Printf.printf "---------|----------|----------|---------\n";

  let sizes = [ 16; 64; 256; 512 ] in

  List.iter
    (fun size ->
      let data = generate_test_data size in

      (* Benchmark fast roundtrip *)
      let fast_time, _ =
        time_function (fun () ->
            let dft = Dsp.dft_transform data in
            Dsp.idft_transform dft)
      in

      (* Benchmark reference roundtrip *)
      let ref_time, _ =
        time_function (fun () ->
            let dft = Dsp.dft_transform_reference data in
            Dsp.idft_transform_reference dft)
      in

      (* Calculate speedup *)
      let speedup = if fast_time > 0.0 then ref_time /. fast_time else 0.0 in

      Printf.printf "  %5d  | %8.6f | %8.6f | %7.2fx\n" size fast_time ref_time
        speedup)
    sizes;

  Printf.printf "\n"

(* Test with filter bank sizes *)
let bench_filter_bank_sizes () =
  Printf.printf "=== Filter Bank Typical Sizes ===\n\n";
  Printf.printf "Size (n) | Fast (s) | Ref (s)  | Speedup | Use Case\n";
  Printf.printf "---------|----------|----------|---------|----------\n";

  let test_cases =
    [ (128, "Small window"); (512, "Standard window"); (1024, "Large window") ]
  in

  List.iter
    (fun (size, desc) ->
      let data = generate_test_data size in

      (* Benchmark fast *)
      let fast_time, _ =
        time_function (fun () ->
            let dft = Dsp.dft_transform data in
            Dsp.idft_transform dft)
      in

      (* Benchmark reference *)
      let ref_time, _ =
        time_function (fun () ->
            let dft = Dsp.dft_transform_reference data in
            Dsp.idft_transform_reference dft)
      in

      let speedup = if fast_time > 0.0 then ref_time /. fast_time else 0.0 in

      Printf.printf "  %5d  | %8.6f | %8.6f | %7.2fx | %s\n" size fast_time
        ref_time speedup desc)
    test_cases;

  Printf.printf "\n"

let () =
  bench_dft_comparison ();
  bench_idft_comparison ();
  bench_roundtrip ();
  bench_filter_bank_sizes ();
  Printf.printf "===================================\n\n"
