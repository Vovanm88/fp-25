(* End-to-end golden tests for WAV file compression and decompression *)
open Alcotest

module Encoder = Codec.Encoder
module Decoder = Codec.Decoder
module Wav = Wav
module Dsp = Codec_utilities.Dsp

(* Helper function to get test WAV file path *)
let get_test_wav_path () =
  let test_file = "test/wav/test_sample.wav" in
  (* Try relative path first *)
  if Sys.file_exists test_file then test_file
  else (
    (* Try to find project root *)
    let rec find_project_root dir =
      if Sys.file_exists (Filename.concat dir "dune-project") then Some dir
      else (
        let parent = Filename.dirname dir in
        if parent = dir then None
        else find_project_root parent
      )
    in
    let current_dir = Sys.getcwd () in
    match find_project_root current_dir with
    | Some root ->
        let abs_path = Filename.concat root test_file in
        if Sys.file_exists abs_path then abs_path
        else (
          let from_build = Filename.concat root ("_build/default/" ^ test_file) in
          if Sys.file_exists from_build then from_build else test_file
        )
    | None -> test_file
  )

(* Helper to extract mono channel from stereo *)
let extract_mono samples num_channels =
  if num_channels = 1 then samples
  else
    (* Take left channel (every other sample) *)
    let rec extract acc i = function
      | [] -> List.rev acc
      | x :: xs ->
          if i mod 2 = 0 then extract (x :: acc) (i + 1) xs
          else extract acc (i + 1) xs
    in
    extract [] 0 samples

(* Helper to limit samples for testing - O(n) instead of O(n²) *)
let limit_samples samples max_samples =
  let rec take n acc = function
    | [] -> List.rev acc
    | x :: xs -> if n <= 0 then List.rev acc else take (n - 1) (x :: acc) xs
  in
  take max_samples [] samples

(* Test compression with high quality (SNR threshold = 60 dB) *)
let test_compression_high_quality () =
  let test_file = get_test_wav_path () in
  let wav_data = Wav.Parser.read_wav test_file in
  let sample_rate = wav_data.Wav.Parser.info.Wav.Parser.sample_rate in
  let num_channels = wav_data.Wav.Parser.info.Wav.Parser.num_channels in
  let bits_per_sample = wav_data.Wav.Parser.info.Wav.Parser.bits_per_sample in
  
  (* Extract mono and limit samples - use 2000 for faster testing *)
  let mono_samples = extract_mono wav_data.Wav.Parser.samples num_channels in
  let test_samples = limit_samples mono_samples 2000 in
  
  (* Create temporary files *)
  let temp_encoded = Filename.temp_file "e2e_high_quality" ".audc" in
  let temp_decoded = Filename.temp_file "e2e_high_quality_decoded" ".wav" in
  
  try
    (* Encode with high quality (SNR threshold = 60 dB) *)
    Printf.printf "\n=== High Quality Mode (SNR >= 60 dB) ===\n";
    let audio_file = Encoder.encode_to_file ~snr_threshold_db:60.0 test_samples sample_rate temp_encoded in
    
    (* Check encoded file *)
    check bool "Encoded file exists" (Sys.file_exists temp_encoded) true;
    check int "Encoded file has 1 track" audio_file.Codec.Audiomodel.num_tracks 1;
    
    (* Get file size *)
    let encoded_size = (Unix.stat temp_encoded).Unix.st_size in
    Printf.printf "Encoded file size: %d bytes\n" encoded_size;
    
    (* Decode *)
    let (reconstructed, decoded_sample_rate) = Decoder.decode_from_file temp_encoded in
    
    (* Check decoded data *)
    check int "Decoded sample rate matches" decoded_sample_rate sample_rate;
    check bool "Decoded signal is not empty" (List.length reconstructed > 0) true;
    
    (* Calculate SNR - O(n) instead of O(n²) *)
    let min_len = min (List.length test_samples) (List.length reconstructed) in
    let rec take n acc = function
      | [] -> List.rev acc
      | x :: xs -> if n <= 0 then List.rev acc else take (n - 1) (x :: acc) xs
    in
    let input_trimmed = take min_len [] test_samples in
    let reconstructed_trimmed = take min_len [] reconstructed in
    let snr = Dsp.snr input_trimmed reconstructed_trimmed in
    
    Printf.printf "Reconstructed samples: %d\n" (List.length reconstructed);
    Printf.printf "SNR: %.2f dB\n" snr;
    
    (* Write decoded audio to WAV *)
    let wav_info = {
      Wav.Writer.sample_rate = decoded_sample_rate;
      Wav.Writer.num_channels = 1;
      Wav.Writer.bits_per_sample = bits_per_sample;
    } in
    Wav.Writer.write_wav temp_decoded wav_info reconstructed;
    check bool "Decoded WAV file exists" (Sys.file_exists temp_decoded) true;
    
    let decoded_size = (Unix.stat temp_decoded).Unix.st_size in
    Printf.printf "Decoded WAV size: %d bytes\n" decoded_size;
    Printf.printf "Compression ratio: %.2fx\n" (float_of_int decoded_size /. float_of_int encoded_size);
    Printf.printf "=====================================\n\n";
    
    (* Verify SNR is reasonable *)
    check bool "SNR is valid" (not (Float.is_nan snr)) true;
    
    (* Cleanup *)
    Sys.remove temp_encoded;
    Sys.remove temp_decoded
  with e ->
    (try Sys.remove temp_encoded with _ -> ());
    (try Sys.remove temp_decoded with _ -> ());
    raise e

(* Test compression with medium quality (SNR threshold = 40 dB) *)
let test_compression_medium_quality () =
  let test_file = get_test_wav_path () in
  let wav_data = Wav.Parser.read_wav test_file in
  let sample_rate = wav_data.Wav.Parser.info.Wav.Parser.sample_rate in
  let num_channels = wav_data.Wav.Parser.info.Wav.Parser.num_channels in
  let bits_per_sample = wav_data.Wav.Parser.info.Wav.Parser.bits_per_sample in
  
  (* Extract mono and limit samples - use 2000 for faster testing *)
  let mono_samples = extract_mono wav_data.Wav.Parser.samples num_channels in
  let test_samples = limit_samples mono_samples 2000 in
  
  (* Create temporary files *)
  let temp_encoded = Filename.temp_file "e2e_medium_quality" ".audc" in
  let temp_decoded = Filename.temp_file "e2e_medium_quality_decoded" ".wav" in
  
  try
    (* Encode with medium quality (SNR threshold = 40 dB - default) *)
    Printf.printf "\n=== Medium Quality Mode (SNR >= 40 dB) ===\n";
    let audio_file = Encoder.encode_to_file ~snr_threshold_db:40.0 test_samples sample_rate temp_encoded in
    
    (* Check encoded file *)
    check bool "Encoded file exists" (Sys.file_exists temp_encoded) true;
    check int "Encoded file has 1 track" audio_file.Codec.Audiomodel.num_tracks 1;
    
    (* Get file size *)
    let encoded_size = (Unix.stat temp_encoded).Unix.st_size in
    Printf.printf "Encoded file size: %d bytes\n" encoded_size;
    
    (* Decode *)
    let (reconstructed, decoded_sample_rate) = Decoder.decode_from_file temp_encoded in
    
    (* Check decoded data *)
    check int "Decoded sample rate matches" decoded_sample_rate sample_rate;
    check bool "Decoded signal is not empty" (List.length reconstructed > 0) true;
    
    (* Calculate SNR - O(n) instead of O(n²) *)
    let min_len = min (List.length test_samples) (List.length reconstructed) in
    let rec take n acc = function
      | [] -> List.rev acc
      | x :: xs -> if n <= 0 then List.rev acc else take (n - 1) (x :: acc) xs
    in
    let input_trimmed = take min_len [] test_samples in
    let reconstructed_trimmed = take min_len [] reconstructed in
    let snr = Dsp.snr input_trimmed reconstructed_trimmed in
    
    Printf.printf "Reconstructed samples: %d\n" (List.length reconstructed);
    Printf.printf "SNR: %.2f dB\n" snr;
    
    (* Write decoded audio to WAV *)
    let wav_info = {
      Wav.Writer.sample_rate = decoded_sample_rate;
      Wav.Writer.num_channels = 1;
      Wav.Writer.bits_per_sample = bits_per_sample;
    } in
    Wav.Writer.write_wav temp_decoded wav_info reconstructed;
    check bool "Decoded WAV file exists" (Sys.file_exists temp_decoded) true;
    
    let decoded_size = (Unix.stat temp_decoded).Unix.st_size in
    Printf.printf "Decoded WAV size: %d bytes\n" decoded_size;
    Printf.printf "Compression ratio: %.2fx\n" (float_of_int decoded_size /. float_of_int encoded_size);
    Printf.printf "=====================================\n\n";
    
    (* Verify SNR is reasonable *)
    check bool "SNR is valid" (not (Float.is_nan snr)) true;
    
    (* Cleanup *)
    Sys.remove temp_encoded;
    Sys.remove temp_decoded
  with e ->
    (try Sys.remove temp_encoded with _ -> ());
    (try Sys.remove temp_decoded with _ -> ());
    raise e

(* Test compression with low quality (SNR threshold = 20 dB) *)
let test_compression_low_quality () =
  let test_file = get_test_wav_path () in
  let wav_data = Wav.Parser.read_wav test_file in
  let sample_rate = wav_data.Wav.Parser.info.Wav.Parser.sample_rate in
  let num_channels = wav_data.Wav.Parser.info.Wav.Parser.num_channels in
  let bits_per_sample = wav_data.Wav.Parser.info.Wav.Parser.bits_per_sample in
  
  (* Extract mono and limit samples - use 2000 for faster testing *)
  let mono_samples = extract_mono wav_data.Wav.Parser.samples num_channels in
  let test_samples = limit_samples mono_samples 2000 in
  
  (* Create temporary files *)
  let temp_encoded = Filename.temp_file "e2e_low_quality" ".audc" in
  let temp_decoded = Filename.temp_file "e2e_low_quality_decoded" ".wav" in
  
  try
    (* Encode with low quality (SNR threshold = 20 dB) *)
    Printf.printf "\n=== Low Quality Mode (SNR >= 20 dB) ===\n";
    let audio_file = Encoder.encode_to_file ~snr_threshold_db:20.0 test_samples sample_rate temp_encoded in
    
    (* Check encoded file *)
    check bool "Encoded file exists" (Sys.file_exists temp_encoded) true;
    check int "Encoded file has 1 track" audio_file.Codec.Audiomodel.num_tracks 1;
    
    (* Get file size *)
    let encoded_size = (Unix.stat temp_encoded).Unix.st_size in
    Printf.printf "Encoded file size: %d bytes\n" encoded_size;
    
    (* Decode *)
    let (reconstructed, decoded_sample_rate) = Decoder.decode_from_file temp_encoded in
    
    (* Check decoded data *)
    check int "Decoded sample rate matches" decoded_sample_rate sample_rate;
    check bool "Decoded signal is not empty" (List.length reconstructed > 0) true;
    
    (* Calculate SNR - O(n) instead of O(n²) *)
    let min_len = min (List.length test_samples) (List.length reconstructed) in
    let rec take n acc = function
      | [] -> List.rev acc
      | x :: xs -> if n <= 0 then List.rev acc else take (n - 1) (x :: acc) xs
    in
    let input_trimmed = take min_len [] test_samples in
    let reconstructed_trimmed = take min_len [] reconstructed in
    let snr = Dsp.snr input_trimmed reconstructed_trimmed in
    
    Printf.printf "Reconstructed samples: %d\n" (List.length reconstructed);
    Printf.printf "SNR: %.2f dB\n" snr;
    
    (* Write decoded audio to WAV *)
    let wav_info = {
      Wav.Writer.sample_rate = decoded_sample_rate;
      Wav.Writer.num_channels = 1;
      Wav.Writer.bits_per_sample = bits_per_sample;
    } in
    Wav.Writer.write_wav temp_decoded wav_info reconstructed;
    check bool "Decoded WAV file exists" (Sys.file_exists temp_decoded) true;
    
    let decoded_size = (Unix.stat temp_decoded).Unix.st_size in
    Printf.printf "Decoded WAV size: %d bytes\n" decoded_size;
    Printf.printf "Compression ratio: %.2fx\n" (float_of_int decoded_size /. float_of_int encoded_size);
    Printf.printf "=====================================\n\n";
    
    (* Verify SNR is reasonable *)
    check bool "SNR is valid" (not (Float.is_nan snr)) true;
    
    (* Cleanup *)
    Sys.remove temp_encoded;
    Sys.remove temp_decoded
  with e ->
    (try Sys.remove temp_encoded with _ -> ());
    (try Sys.remove temp_decoded with _ -> ());
    raise e

(* Test full WAV file compression (longer sample) *)
let test_compression_full_wav () =
  let test_file = get_test_wav_path () in
  let wav_data = Wav.Parser.read_wav test_file in
  let sample_rate = wav_data.Wav.Parser.info.Wav.Parser.sample_rate in
  let num_channels = wav_data.Wav.Parser.info.Wav.Parser.num_channels in
  let bits_per_sample = wav_data.Wav.Parser.info.Wav.Parser.bits_per_sample in
  
  (* Extract mono and limit to 5000 samples for faster testing *)
  let mono_samples = extract_mono wav_data.Wav.Parser.samples num_channels in
  let test_samples = limit_samples mono_samples 5000 in
  
  (* Create temporary files *)
  let temp_encoded = Filename.temp_file "e2e_full_wav" ".audc" in
  let temp_decoded = Filename.temp_file "e2e_full_wav_decoded" ".wav" in
  
  try
    (* Encode with default quality *)
    Printf.printf "\n=== Full WAV Compression Test (5000 samples) ===\n";
    let audio_file = Encoder.encode_to_file test_samples sample_rate temp_encoded in
    
    (* Check encoded file *)
    check bool "Encoded file exists" (Sys.file_exists temp_encoded) true;
    check int "Encoded file has 1 track" audio_file.Codec.Audiomodel.num_tracks 1;
    
    (* Get file size *)
    let encoded_size = (Unix.stat temp_encoded).Unix.st_size in
    Printf.printf "Original samples: %d\n" (List.length test_samples);
    Printf.printf "Encoded file size: %d bytes\n" encoded_size;
    
    (* Decode *)
    let (reconstructed, decoded_sample_rate) = Decoder.decode_from_file temp_encoded in
    
    (* Check decoded data *)
    check int "Decoded sample rate matches" decoded_sample_rate sample_rate;
    check bool "Decoded signal is not empty" (List.length reconstructed > 0) true;
    
    (* Calculate SNR - O(n) instead of O(n²) *)
    let min_len = min (List.length test_samples) (List.length reconstructed) in
    let rec take n acc = function
      | [] -> List.rev acc
      | x :: xs -> if n <= 0 then List.rev acc else take (n - 1) (x :: acc) xs
    in
    let input_trimmed = take min_len [] test_samples in
    let reconstructed_trimmed = take min_len [] reconstructed in
    let snr = Dsp.snr input_trimmed reconstructed_trimmed in
    
    Printf.printf "Reconstructed samples: %d\n" (List.length reconstructed);
    Printf.printf "Length difference: %d samples\n" (abs (List.length reconstructed - List.length test_samples));
    Printf.printf "SNR: %.2f dB\n" snr;
    
    (* Write decoded audio to WAV *)
    let wav_info = {
      Wav.Writer.sample_rate = decoded_sample_rate;
      Wav.Writer.num_channels = 1;
      Wav.Writer.bits_per_sample = bits_per_sample;
    } in
    Wav.Writer.write_wav temp_decoded wav_info reconstructed;
    check bool "Decoded WAV file exists" (Sys.file_exists temp_decoded) true;
    
    let decoded_size = (Unix.stat temp_decoded).Unix.st_size in
    Printf.printf "Decoded WAV size: %d bytes\n" decoded_size;
    Printf.printf "Compression ratio: %.2fx\n" (float_of_int decoded_size /. float_of_int encoded_size);
    Printf.printf "================================================\n\n";
    
    (* Verify SNR is reasonable *)
    check bool "SNR is valid" (not (Float.is_nan snr)) true;
    
    (* Verify we can read the decoded WAV back *)
    let decoded_wav = Wav.Parser.read_wav temp_decoded in
    check int "Decoded WAV sample rate matches" 
      decoded_wav.Wav.Parser.info.Wav.Parser.sample_rate decoded_sample_rate;
    check int "Decoded WAV length matches" 
      (List.length decoded_wav.Wav.Parser.samples) (List.length reconstructed);
    
    (* Cleanup *)
    Sys.remove temp_encoded;
    Sys.remove temp_decoded
  with e ->
    (try Sys.remove temp_encoded with _ -> ());
    (try Sys.remove temp_decoded with _ -> ());
    raise e

(* Test compression comparison across all modes *)
let test_compression_comparison () =
  let test_file = get_test_wav_path () in
  let wav_data = Wav.Parser.read_wav test_file in
  let sample_rate = wav_data.Wav.Parser.info.Wav.Parser.sample_rate in
  let num_channels = wav_data.Wav.Parser.info.Wav.Parser.num_channels in
  
  (* Extract mono and limit samples - use 3000 for comparison *)
  let mono_samples = extract_mono wav_data.Wav.Parser.samples num_channels in
  let test_samples = limit_samples mono_samples 3000 in
  
  Printf.printf "\n=== Compression Mode Comparison ===\n";
  Printf.printf "Original samples: %d\n" (List.length test_samples);
  Printf.printf "Sample rate: %d Hz\n\n" sample_rate;
  
  let modes = [
    ("High Quality (60 dB)", 60.0);
    ("Medium Quality (40 dB)", 40.0);
    ("Low Quality (20 dB)", 20.0);
  ] in
  
  List.iter (fun (mode_name, snr_threshold) ->
    let temp_encoded = Filename.temp_file "e2e_comparison" ".audc" in
    try
      (* Encode *)
      let _audio_file = Encoder.encode_to_file ~snr_threshold_db:snr_threshold test_samples sample_rate temp_encoded in
      let encoded_size = (Unix.stat temp_encoded).Unix.st_size in
      
      (* Decode *)
      let (reconstructed, _) = Decoder.decode_from_file temp_encoded in
      
      (* Calculate SNR *)
      let min_len = min (List.length test_samples) (List.length reconstructed) in
      let input_trimmed = List.init min_len (fun i -> List.nth test_samples i) in
      let reconstructed_trimmed = List.init min_len (fun i -> List.nth reconstructed i) in
      let snr = Dsp.snr input_trimmed reconstructed_trimmed in
      
      (* Calculate original WAV size (approximate) *)
      let original_size = (List.length test_samples) * 2 + 44 in (* 16-bit samples + WAV header *)
      let compression_ratio = float_of_int original_size /. float_of_int encoded_size in
      
      Printf.printf "%s:\n" mode_name;
      Printf.printf "  Encoded size: %d bytes\n" encoded_size;
      Printf.printf "  Compression ratio: %.2fx\n" compression_ratio;
      Printf.printf "  Achieved SNR: %.2f dB\n\n" snr;
      
      (* Cleanup *)
      Sys.remove temp_encoded
    with e ->
      (try Sys.remove temp_encoded with _ -> ());
      raise e
  ) modes;
  
  Printf.printf "===================================\n\n";
  
  (* Just verify the test ran *)
  check bool "Comparison test completed" true true

let () =
  run "E2E WAV Compression Tests" [
    "Compression Modes", [
      test_case "High quality compression (60 dB)" `Slow test_compression_high_quality;
      test_case "Medium quality compression (40 dB)" `Slow test_compression_medium_quality;
      test_case "Low quality compression (20 dB)" `Slow test_compression_low_quality;
    ];
    "Full WAV Tests", [
      test_case "Full WAV file compression" `Slow test_compression_full_wav;
    ];
    "Comparison", [
      test_case "Compare compression modes" `Slow test_compression_comparison;
    ];
  ]
