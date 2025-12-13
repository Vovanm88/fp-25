(* Golden test for WAV compression - saves output files for manual inspection *)
open Alcotest

module Encoder = Codec.Encoder
module Decoder = Codec.Decoder
module Wav = Wav
module Dsp = Codec_utilities.Dsp

(* Helper function to get test WAV file path *)
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
    let rec extract acc i = function
      | [] -> List.rev acc
      | x :: xs ->
          if i mod 2 = 0 then extract (x :: acc) (i + 1) xs
          else extract acc (i + 1) xs
    in
    extract [] 0 samples

(* Helper to limit samples *)
let limit_samples samples max_samples =
  if List.length samples > max_samples then
    List.init max_samples (fun i -> List.nth samples i)
  else
    samples

(* Create output directory if it doesn't exist *)
let ensure_output_dir () =
  let output_dir = "test_output" in
  (try
    if not (Sys.file_exists output_dir) then
      Unix.mkdir output_dir 0o755
  with _ -> ());
  output_dir

(* Golden test: compress and decompress WAV in different modes *)
let test_golden_compression () =
  let test_file = get_test_wav_path () in
  let wav_data = Wav.Parser.read_wav test_file in
  let sample_rate = wav_data.Wav.Parser.info.Wav.Parser.sample_rate in
  let num_channels = wav_data.Wav.Parser.info.Wav.Parser.num_channels in
  let bits_per_sample = wav_data.Wav.Parser.info.Wav.Parser.bits_per_sample in
  
  (* Extract mono and limit to 5000 samples for faster testing *)
  let mono_samples = extract_mono wav_data.Wav.Parser.samples num_channels in
  let test_samples = limit_samples mono_samples 5000 in
  
  let output_dir = ensure_output_dir () in
  
  Printf.printf "\n=== Golden Test: WAV Compression in Different Modes ===\n";
  Printf.printf "Input: %s\n" test_file;
  Printf.printf "Samples: %d (limited from %d)\n" (List.length test_samples) (List.length mono_samples);
  Printf.printf "Sample rate: %d Hz\n" sample_rate;
  Printf.printf "Output directory: %s/\n\n" output_dir;
  
  let modes = [
    ("high_quality", "High Quality", 60.0);
    ("medium_quality", "Medium Quality", 40.0);
    ("low_quality", "Low Quality", 20.0);
  ] in
  
  List.iter (fun (mode_id, mode_name, snr_threshold) ->
    Printf.printf "Processing %s (SNR >= %.0f dB)...\n" mode_name snr_threshold;
    
    let encoded_file = Filename.concat output_dir (Printf.sprintf "test_sample_%s.audc" mode_id) in
    let decoded_file = Filename.concat output_dir (Printf.sprintf "test_sample_%s_decoded.wav" mode_id) in
    
    (* Encode *)
    let audio_file = Encoder.encode_to_file ~snr_threshold_db:snr_threshold test_samples sample_rate encoded_file in
    let encoded_size = (Unix.stat encoded_file).Unix.st_size in
    
    (* Decode *)
    let (reconstructed, decoded_sample_rate) = Decoder.decode_from_file encoded_file in
    
    (* Calculate SNR *)
    let min_len = min (List.length test_samples) (List.length reconstructed) in
    let input_trimmed = List.init min_len (fun i -> List.nth test_samples i) in
    let reconstructed_trimmed = List.init min_len (fun i -> List.nth reconstructed i) in
    let snr = Dsp.snr input_trimmed reconstructed_trimmed in
    
    (* Write decoded WAV *)
    let wav_info = {
      Wav.Writer.sample_rate = decoded_sample_rate;
      Wav.Writer.num_channels = 1;
      Wav.Writer.bits_per_sample = bits_per_sample;
    } in
    Wav.Writer.write_wav decoded_file wav_info reconstructed;
    let decoded_size = (Unix.stat decoded_file).Unix.st_size in
    
    Printf.printf "  Encoded: %s (%d bytes)\n" encoded_file encoded_size;
    Printf.printf "  Decoded: %s (%d bytes)\n" decoded_file decoded_size;
    Printf.printf "  Compression ratio: %.2fx\n" (float_of_int decoded_size /. float_of_int encoded_size);
    Printf.printf "  SNR: %.2f dB\n" snr;
    Printf.printf "  Reconstructed samples: %d (diff: %d)\n\n" 
      (List.length reconstructed) 
      (abs (List.length reconstructed - List.length test_samples));
    
    (* Verify files exist *)
    check bool (Printf.sprintf "%s: encoded file exists" mode_name) (Sys.file_exists encoded_file) true;
    check bool (Printf.sprintf "%s: decoded file exists" mode_name) (Sys.file_exists decoded_file) true;
    check int (Printf.sprintf "%s: audio file has 1 track" mode_name) audio_file.Codec.Audiomodel.num_tracks 1;
    check int (Printf.sprintf "%s: decoded sample rate matches" mode_name) decoded_sample_rate sample_rate;
  ) modes;
  
  Printf.printf "=======================================================\n";
  Printf.printf "Golden test completed. Output files saved to %s/\n" output_dir;
  Printf.printf "You can manually inspect and compare:\n";
  Printf.printf "  - Original: %s\n" test_file;
  Printf.printf "  - High quality: %s/test_sample_high_quality_decoded.wav\n" output_dir;
  Printf.printf "  - Medium quality: %s/test_sample_medium_quality_decoded.wav\n" output_dir;
  Printf.printf "  - Low quality: %s/test_sample_low_quality_decoded.wav\n" output_dir;
  Printf.printf "=======================================================\n\n"

(* Golden test: full WAV file compression *)
let test_golden_full_wav () =
  let test_file = get_test_wav_path () in
  let wav_data = Wav.Parser.read_wav test_file in
  let sample_rate = wav_data.Wav.Parser.info.Wav.Parser.sample_rate in
  let num_channels = wav_data.Wav.Parser.info.Wav.Parser.num_channels in
  let bits_per_sample = wav_data.Wav.Parser.info.Wav.Parser.bits_per_sample in
  
  (* Extract mono - limit to 15000 samples for reasonable test time *)
  let mono_samples = extract_mono wav_data.Wav.Parser.samples num_channels in
  let mono_samples = limit_samples mono_samples 15000 in
  
  let output_dir = ensure_output_dir () in
  
  Printf.printf "\n=== Golden Test: Full WAV File Compression ===\n";
  Printf.printf "Input: %s\n" test_file;
  Printf.printf "Samples: %d\n" (List.length mono_samples);
  Printf.printf "Sample rate: %d Hz\n" sample_rate;
  Printf.printf "Output directory: %s/\n\n" output_dir;
  
  let encoded_file = Filename.concat output_dir "test_sample_full.audc" in
  let decoded_file = Filename.concat output_dir "test_sample_full_decoded.wav" in
  
  (* Encode with default quality *)
  Printf.printf "Encoding full WAV file...\n";
  let audio_file = Encoder.encode_to_file mono_samples sample_rate encoded_file in
  let encoded_size = (Unix.stat encoded_file).Unix.st_size in
  
  (* Decode *)
  Printf.printf "Decoding...\n";
  let (reconstructed, decoded_sample_rate) = Decoder.decode_from_file encoded_file in
  
  (* Calculate SNR *)
  let min_len = min (List.length mono_samples) (List.length reconstructed) in
  let input_trimmed = List.init min_len (fun i -> List.nth mono_samples i) in
  let reconstructed_trimmed = List.init min_len (fun i -> List.nth reconstructed i) in
  let snr = Dsp.snr input_trimmed reconstructed_trimmed in
  
  (* Write decoded WAV *)
  let wav_info = {
    Wav.Writer.sample_rate = decoded_sample_rate;
    Wav.Writer.num_channels = 1;
    Wav.Writer.bits_per_sample = bits_per_sample;
  } in
  Wav.Writer.write_wav decoded_file wav_info reconstructed;
  let decoded_size = (Unix.stat decoded_file).Unix.st_size in
  
  Printf.printf "\nResults:\n";
  Printf.printf "  Original samples: %d\n" (List.length mono_samples);
  Printf.printf "  Reconstructed samples: %d\n" (List.length reconstructed);
  Printf.printf "  Length difference: %d samples\n" (abs (List.length reconstructed - List.length mono_samples));
  Printf.printf "  Encoded size: %d bytes\n" encoded_size;
  Printf.printf "  Decoded WAV size: %d bytes\n" decoded_size;
  Printf.printf "  Compression ratio: %.2fx\n" (float_of_int decoded_size /. float_of_int encoded_size);
  Printf.printf "  SNR: %.2f dB\n" snr;
  Printf.printf "\nOutput files:\n";
  Printf.printf "  Encoded: %s\n" encoded_file;
  Printf.printf "  Decoded: %s\n" decoded_file;
  Printf.printf "==============================================\n\n";
  
  (* Verify files exist *)
  check bool "Full WAV: encoded file exists" (Sys.file_exists encoded_file) true;
  check bool "Full WAV: decoded file exists" (Sys.file_exists decoded_file) true;
  check int "Full WAV: audio file has 1 track" audio_file.Codec.Audiomodel.num_tracks 1;
  check int "Full WAV: decoded sample rate matches" decoded_sample_rate sample_rate

let () =
  run "Golden WAV Compression Tests" [
    "Golden Tests", [
      test_case "Compress and decompress in different modes" `Slow test_golden_compression;
      test_case "Full WAV file compression" `Slow test_golden_full_wav;
    ];
  ]
