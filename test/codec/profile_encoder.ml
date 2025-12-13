(* Profile encoder to find bottlenecks *)

module Encoder = Codec.Encoder

let time_it name f =
  let start = Unix.gettimeofday () in
  let result = f () in
  let elapsed = Unix.gettimeofday () -. start in
  Printf.printf "%-35s: %8.3f sec\n" name elapsed;
  result

let () =
  (* Generate test data - 140K samples like real WAV *)
  let num_samples = 140000 in
  Printf.printf "\nGenerating %d test samples...\n" num_samples;
  flush stdout;
  let samples = List.init num_samples (fun i -> Float.sin (float_of_int i *. 0.1)) in
  let sample_rate = 44100 in
  
  Printf.printf "\n╔═══════════════════════════════════════════════════════╗\n";
  Printf.printf "║     Encoder Profiling (%d samples)            ║\n" num_samples;
  Printf.printf "╚═══════════════════════════════════════════════════════╝\n\n";
  
  let total_start = Unix.gettimeofday () in
  
  (* 1. Compress *)
  let compressed, _min_val, _max_val, _ = time_it "1. Compress track" (fun () ->
    Encoder.compress_track samples
  ) in
  
  (* 2. LR -> Mid-Side *)
  let mid, _side, _is_stereo = time_it "2. LR to Mid-Side" (fun () ->
    Encoder.lr_to_mid_side compressed compressed
  ) in
  
  (* 3. Silence replacement *)
  let processed, _mask = time_it "3. Silence replacement" (fun () ->
    Encoder.replace_silence_after_loud sample_rate mid
  ) in
  
  (* 4. Segmentation *)
  let segmented, segments_info = time_it "4. Segmentation" (fun () ->
    Encoder.segment_track sample_rate processed
  ) in
  
  (* 5. Create windows *)
  let windows = time_it "5. Create overlapping windows" (fun () ->
    Encoder.create_overlapping_windows segmented segments_info sample_rate
  ) in
  
  (* 6. MDCT Level 1 *)
  let mdct_segments = time_it "6. MDCT Level 1" (fun () ->
    Encoder.apply_mdct_level1 windows
  ) in
  
  (* 7. Split bands *)
  let band_segments = time_it "7. Split frequency bands" (fun () ->
    Encoder.split_frequency_bands sample_rate mdct_segments
  ) in
  
  (* 8. IMDCT *)
  let imdct_segments = time_it "8. IMDCT to bands" (fun () ->
    Encoder.apply_imdct_to_bands band_segments
  ) in
  
  (* 9. MDCT Level 2 *)
  let mdct2_segments = time_it "9. MDCT Level 2" (fun () ->
    Encoder.apply_mdct_level2 imdct_segments
  ) in
  
  (* 10. Quantization *)
  let _quant_segments = time_it "10. Quantization" (fun () ->
    Encoder.select_quantization_thresholds mdct2_segments
  ) in
  
  let total_time = Unix.gettimeofday () -. total_start in
  
  Printf.printf "───────────────────────────────────────────────────────\n";
  Printf.printf "%-35s: %8.3f sec\n" "TOTAL" total_time;
  Printf.printf "\n"
