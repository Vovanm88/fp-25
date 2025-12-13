(* Profile decoder to find bottlenecks *)

module Encoder = Codec.Encoder
module Decoder = Codec.Decoder

let time_it name f =
  let start = Unix.gettimeofday () in
  let result = f () in
  let elapsed = Unix.gettimeofday () -. start in
  Printf.printf "%-35s: %8.3f sec\n" name elapsed;
  result

let () =
  (* Generate test data and encode *)
  let num_samples = 140000 in
  Printf.printf "\nGenerating %d test samples...\n" num_samples;
  flush stdout;
  let samples = List.init num_samples (fun i -> Float.sin (float_of_int i *. 0.1)) in
  let sample_rate = 44100 in
  
  Printf.printf "Encoding...\n";
  flush stdout;
  let temp_file = Filename.temp_file "profile_decoder" ".audc" in
  let _audio_file = Encoder.encode_to_file samples sample_rate temp_file in
  
  Printf.printf "\n╔═══════════════════════════════════════════════════════╗\n";
  Printf.printf "║     Decoder Profiling (%d samples)            ║\n" num_samples;
  Printf.printf "╚═══════════════════════════════════════════════════════╝\n\n";
  
  let total_start = Unix.gettimeofday () in
  
  (* Read file *)
  let bytes = time_it "1. Read file (deserialize)" (fun () ->
    let ic = open_in_bin temp_file in
    let len = in_channel_length ic in
    let bytes = really_input_string ic len in
    close_in ic;
    List.init (String.length bytes) (fun i -> Char.code bytes.[i])
  ) in
  
  (* Deserialize *)
  let audio_file = time_it "2. Deserialize AudioFile" (fun () ->
    let af, _ = Codec.Serialization.deserialize_audio_file bytes in
    af
  ) in
  
  (* Read AudioFile *)
  let (segments, _sample_rate, _compression_params, _original_length) = 
    time_it "3. Read AudioFile structure" (fun () ->
      Decoder.read_audio_file audio_file
    ) in
  
  Printf.printf "\n   Segments to decode: %d\n\n" (List.length segments);
  
  (* IMDCT Level 2 *)
  let decoded_imdct2 = time_it "4. IMDCT Level 2" (fun () ->
    Decoder.apply_imdct_level2 segments
  ) in
  
  (* MDCT Level 1 to bands *)
  let decoded_mdct1 = time_it "5. MDCT Level 1 to bands" (fun () ->
    Decoder.apply_mdct_level1_to_bands decoded_imdct2
  ) in
  
  (* Merge frequency bands *)
  let merged_segments = time_it "6. Merge frequency bands" (fun () ->
    Decoder.merge_frequency_bands decoded_mdct1
  ) in
  
  (* Final IMDCT *)
  let decoded_windows = time_it "7. Final IMDCT" (fun () ->
    Decoder.apply_imdct_final merged_segments
  ) in
  
  (* Overlap-add *)
  let reconstructed_segmented = time_it "8. Overlap-add windows" (fun () ->
    Decoder.overlap_add_windows decoded_windows
  ) in
  
  (* Pass-through silence *)
  let reconstructed_processed = time_it "9. Pass-through silence" (fun () ->
    Decoder.pass_through_silence reconstructed_segmented
  ) in
  
  (* Mid-Side to LR *)
  let reconstructed_compressed, _ = time_it "10. Mid-Side to LR" (fun () ->
    Decoder.mid_side_to_lr reconstructed_processed reconstructed_processed false
  ) in
  
  (* Decompress *)
  let _final = time_it "11. Decompress track" (fun () ->
    Decoder.decompress_track reconstructed_compressed 0.0 1.0
  ) in
  
  let total_time = Unix.gettimeofday () -. total_start in
  
  Printf.printf "───────────────────────────────────────────────────────\n";
  Printf.printf "%-35s: %8.3f sec\n" "TOTAL" total_time;
  Printf.printf "\n";
  
  Sys.remove temp_file
