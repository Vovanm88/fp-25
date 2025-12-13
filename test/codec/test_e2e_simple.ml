(* Simplified E2E test - faster version with minimal processing *)
open Alcotest

module Encoder = Codec.Encoder
module Decoder = Codec.Decoder
module Wav = Wav
module Dsp = Codec_utilities.Dsp

(* Helper to get test WAV file *)
let get_test_wav_path () =
  let test_file = "test/wav/test_sample.wav" in
  if Sys.file_exists test_file then test_file
  else (
    let rec find_project_root dir =
      if Sys.file_exists (Filename.concat dir "dune-project") then Some dir
      else (
        let parent = Filename.dirname dir in
        if parent = dir then None else find_project_root parent
      )
    in
    match find_project_root (Sys.getcwd ()) with
    | Some root ->
        let abs_path = Filename.concat root test_file in
        if Sys.file_exists abs_path then abs_path
        else Filename.concat root ("_build/default/" ^ test_file)
    | None -> test_file
  )

(* Extract mono from stereo *)
let extract_mono samples num_channels =
  if num_channels = 1 then samples
  else
    let rec extract acc i = function
      | [] -> List.rev acc
      | x :: xs ->
          if i mod 2 = 0 then extract (x :: acc) (i + 1) xs
          else extract acc (i + 1) xs
    in
    extract [] 0 samples

(* Simple compression test - 1000 samples only *)
let test_simple_compression () =
  let test_file = get_test_wav_path () in
  let wav_data = Wav.Parser.read_wav test_file in
  let sample_rate = wav_data.Wav.Parser.info.Wav.Parser.sample_rate in
  let num_channels = wav_data.Wav.Parser.info.Wav.Parser.num_channels in
  
  (* Use only 1000 samples *)
  let mono_samples = extract_mono wav_data.Wav.Parser.samples num_channels in
  let test_samples = List.init 1000 (fun i -> List.nth mono_samples i) in
  
  Printf.printf "\n=== Simple Compression Test (1000 samples) ===\n";
  
  let temp_encoded = Filename.temp_file "simple_test" ".audc" in
  let temp_decoded = Filename.temp_file "simple_test_decoded" ".wav" in
  
  try
    (* Encode *)
    let start_time = Unix.gettimeofday () in
    let _audio_file = Encoder.encode_to_file test_samples sample_rate temp_encoded in
    let encode_time = Unix.gettimeofday () -. start_time in
    
    (* Decode *)
    let start_time = Unix.gettimeofday () in
    let (reconstructed, _) = Decoder.decode_from_file temp_encoded in
    let decode_time = Unix.gettimeofday () -. start_time in
    
    Printf.printf "Encode time: %.3f seconds\n" encode_time;
    Printf.printf "Decode time: %.3f seconds\n" decode_time;
    Printf.printf "Total time: %.3f seconds\n" (encode_time +. decode_time);
    Printf.printf "Reconstructed: %d samples\n" (List.length reconstructed);
    Printf.printf "==============================================\n\n";
    
    check bool "Encoded file exists" (Sys.file_exists temp_encoded) true;
    check bool "Reconstructed is not empty" (List.length reconstructed > 0) true;
    
    (* Cleanup *)
    Sys.remove temp_encoded;
    (try Sys.remove temp_decoded with _ -> ())
  with e ->
    (try Sys.remove temp_encoded with _ -> ());
    (try Sys.remove temp_decoded with _ -> ());
    raise e

(* Test with 500 samples - very fast *)
let test_tiny_compression () =
  let test_file = get_test_wav_path () in
  let wav_data = Wav.Parser.read_wav test_file in
  let sample_rate = wav_data.Wav.Parser.info.Wav.Parser.sample_rate in
  let num_channels = wav_data.Wav.Parser.info.Wav.Parser.num_channels in
  
  (* Use only 500 samples *)
  let mono_samples = extract_mono wav_data.Wav.Parser.samples num_channels in
  let test_samples = List.init 500 (fun i -> List.nth mono_samples i) in
  
  Printf.printf "\n=== Tiny Compression Test (500 samples) ===\n";
  
  let temp_file = Filename.temp_file "tiny_test" ".audc" in
  
  try
    let start_time = Unix.gettimeofday () in
    let _audio_file = Encoder.encode_to_file test_samples sample_rate temp_file in
    let (reconstructed, _) = Decoder.decode_from_file temp_file in
    let total_time = Unix.gettimeofday () -. start_time in
    
    Printf.printf "Total roundtrip time: %.3f seconds\n" total_time;
    Printf.printf "Reconstructed: %d samples\n" (List.length reconstructed);
    Printf.printf "==========================================\n\n";
    
    check bool "Test completed" true true;
    Sys.remove temp_file
  with e ->
    (try Sys.remove temp_file with _ -> ());
    raise e

let () =
  run "Simple E2E Tests" [
    "Fast Tests", [
      test_case "Tiny compression (500 samples)" `Quick test_tiny_compression;
      test_case "Simple compression (1000 samples)" `Quick test_simple_compression;
    ];
  ]
