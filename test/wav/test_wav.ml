(* Unit tests for WAV parser and writer *)
open Alcotest

module Parser = Wav.Parser
module Writer = Wav.Writer

let float_approx = float 0.0001

(* Get path to test sample file, handling both source and build directories *)
let get_test_sample_path () =
  let test_file = "test/wav/test_sample.wav" in
  (* Try relative path first *)
  if Sys.file_exists test_file then test_file
  else (
    (* Try to find project root by looking for dune-project file *)
    let rec find_project_root dir =
      if Sys.file_exists (Filename.concat dir "dune-project") then Some dir
      else (
        let parent = Filename.dirname dir in
        if parent = dir then None (* reached root *)
        else find_project_root parent
      )
    in
    let current_dir = Sys.getcwd () in
    match find_project_root current_dir with
    | Some root ->
        let abs_path = Filename.concat root test_file in
        if Sys.file_exists abs_path then abs_path
        else (
          (* Try from _build directory *)
          let from_build = Filename.concat root ("_build/default/" ^ test_file) in
          if Sys.file_exists from_build then from_build else test_file
        )
    | None ->
        (* Fallback: try common locations *)
        let from_build = "../../" ^ test_file in
        if Sys.file_exists from_build then from_build else test_file
  )

let test_read_wav_info () =
  let test_file = get_test_sample_path () in
  let info = Parser.read_wav_info test_file in
  check int "Sample rate is 44100" info.Parser.sample_rate 44100;
  check int "Number of channels is 2" info.Parser.num_channels 2;
  check int "Bits per sample is 16" info.Parser.bits_per_sample 16;
  check bool "Number of samples > 0" (info.Parser.num_samples > 0) true

let test_read_wav () =
  let test_file = get_test_sample_path () in
  let wav_data = Parser.read_wav test_file in
  let info = wav_data.Parser.info in
  check int "Sample rate is 44100" info.Parser.sample_rate 44100;
  check int "Number of channels is 2" info.Parser.num_channels 2;
  check int "Bits per sample is 16" info.Parser.bits_per_sample 16;
  check bool "Samples list is not empty" (List.length wav_data.Parser.samples > 0) true;
  (* Check that samples are in valid range [-1.0, 1.0] *)
  List.iter
    (fun sample ->
      check bool "Sample in valid range" (sample >= -1.0 && sample <= 1.0) true)
    wav_data.Parser.samples;
  (* Expected number of samples = num_samples * num_channels *)
  let expected_samples = info.Parser.num_samples * info.Parser.num_channels in
  check int "Correct number of samples" (List.length wav_data.Parser.samples) expected_samples

let test_write_read_roundtrip_16bit () =
  let test_samples = [0.0; 0.5; -0.5; 1.0; -1.0; 0.25; -0.25; 0.75] in
  let info = { Writer.sample_rate = 44100; num_channels = 1; bits_per_sample = 16 } in
  let test_file = Filename.temp_file "test_wav" ".wav" in
  try
    Writer.write_wav test_file info test_samples;
    let wav_data = Parser.read_wav test_file in
    check int "Roundtrip preserves sample rate" wav_data.Parser.info.Parser.sample_rate 44100;
    check int "Roundtrip preserves channels" wav_data.Parser.info.Parser.num_channels 1;
    check int "Roundtrip preserves bits per sample" wav_data.Parser.info.Parser.bits_per_sample 16;
    check int "Roundtrip preserves number of samples" (List.length wav_data.Parser.samples) (List.length test_samples);
    (* Check samples are approximately equal (with quantization error) *)
    List.iter2
      (fun original recovered ->
        check float_approx "Roundtrip preserves sample value" original recovered)
      test_samples wav_data.Parser.samples;
    Sys.remove test_file
  with
  | e ->
      (try Sys.remove test_file with _ -> ());
      raise e

let test_write_read_roundtrip_32bit () =
  let test_samples = [0.0; 0.5; -0.5; 1.0; -1.0; 0.25; -0.25; 0.75] in
  let info = { Writer.sample_rate = 22050; num_channels = 1; bits_per_sample = 32 } in
  let test_file = Filename.temp_file "test_wav" ".wav" in
  try
    Writer.write_wav test_file info test_samples;
    let wav_data = Parser.read_wav test_file in
    check int "Roundtrip preserves sample rate" wav_data.Parser.info.Parser.sample_rate 22050;
    check int "Roundtrip preserves channels" wav_data.Parser.info.Parser.num_channels 1;
    check int "Roundtrip preserves bits per sample" wav_data.Parser.info.Parser.bits_per_sample 32;
    check int "Roundtrip preserves number of samples" (List.length wav_data.Parser.samples) (List.length test_samples);
    (* 32-bit should have better precision *)
    List.iter2
      (fun original recovered ->
        check float_approx "Roundtrip preserves sample value" original recovered)
      test_samples wav_data.Parser.samples;
    Sys.remove test_file
  with
  | e ->
      (try Sys.remove test_file with _ -> ());
      raise e

let test_write_read_roundtrip_stereo () =
  let test_samples = [0.0; 0.0; 0.5; -0.5; -0.5; 0.5; 1.0; -1.0] in
  let info = { Writer.sample_rate = 48000; num_channels = 2; bits_per_sample = 16 } in
  let test_file = Filename.temp_file "test_wav" ".wav" in
  try
    Writer.write_wav test_file info test_samples;
    let wav_data = Parser.read_wav test_file in
    check int "Roundtrip preserves sample rate" wav_data.Parser.info.Parser.sample_rate 48000;
    check int "Roundtrip preserves channels" wav_data.Parser.info.Parser.num_channels 2;
    check int "Roundtrip preserves bits per sample" wav_data.Parser.info.Parser.bits_per_sample 16;
    check int "Roundtrip preserves number of samples" (List.length wav_data.Parser.samples) (List.length test_samples);
    List.iter2
      (fun original recovered ->
        check float_approx "Roundtrip preserves sample value" original recovered)
      test_samples wav_data.Parser.samples;
    Sys.remove test_file
  with
  | e ->
      (try Sys.remove test_file with _ -> ());
      raise e

let test_write_read_roundtrip_32bit_precision () =
  (* Test 32-bit PCM has better precision than 16-bit *)
  let test_samples = [0.0; 0.5; -0.5; 1.0; -1.0; 0.123456; -0.789012] in
  let info = { Writer.sample_rate = 44100; num_channels = 1; bits_per_sample = 32 } in
  let test_file = Filename.temp_file "test_wav" ".wav" in
  try
    Writer.write_wav test_file info test_samples;
    let wav_data = Parser.read_wav test_file in
    check int "Roundtrip preserves sample rate" wav_data.Parser.info.Parser.sample_rate 44100;
    check int "Roundtrip preserves channels" wav_data.Parser.info.Parser.num_channels 1;
    check int "Roundtrip preserves bits per sample" wav_data.Parser.info.Parser.bits_per_sample 32;
    check int "Roundtrip preserves number of samples" (List.length wav_data.Parser.samples) (List.length test_samples);
    (* 32-bit PCM should have good precision *)
    List.iter2
      (fun original recovered ->
        check float_approx "Roundtrip preserves sample value" original recovered)
      test_samples wav_data.Parser.samples;
    Sys.remove test_file
  with
  | e ->
      (try Sys.remove test_file with _ -> ());
      raise e

let test_clamp_samples () =
  (* Test that samples outside [-1.0, 1.0] are clamped *)
  let test_samples = [-2.0; -1.5; -1.0; 0.0; 1.0; 1.5; 2.0] in
  let info = { Writer.sample_rate = 44100; num_channels = 1; bits_per_sample = 16 } in
  let test_file = Filename.temp_file "test_wav" ".wav" in
  try
    Writer.write_wav test_file info test_samples;
    let wav_data = Parser.read_wav test_file in
    (* All samples should be in valid range after reading *)
    List.iter
      (fun sample ->
        check bool "Clamped sample in valid range" (sample >= -1.0 && sample <= 1.0) true)
      wav_data.Parser.samples;
    Sys.remove test_file
  with
  | e ->
      (try Sys.remove test_file with _ -> ());
      raise e

let test_read_write_real_file () =
  (* Read the test sample, write it, read it back, compare *)
  let test_file = get_test_sample_path () in
  let original = Parser.read_wav test_file in
  let test_file = Filename.temp_file "test_wav" ".wav" in
  try
    let write_info =
      {
        Writer.sample_rate = original.Parser.info.Parser.sample_rate;
        num_channels = original.Parser.info.Parser.num_channels;
        bits_per_sample = original.Parser.info.Parser.bits_per_sample;
      }
    in
    Writer.write_wav test_file write_info original.Parser.samples;
    let recovered = Parser.read_wav test_file in
    check int "Real file roundtrip preserves sample rate" recovered.Parser.info.Parser.sample_rate
      original.Parser.info.Parser.sample_rate;
    check int "Real file roundtrip preserves channels" recovered.Parser.info.Parser.num_channels
      original.Parser.info.Parser.num_channels;
    check int "Real file roundtrip preserves bits per sample" recovered.Parser.info.Parser.bits_per_sample
      original.Parser.info.Parser.bits_per_sample;
    check int "Real file roundtrip preserves number of samples" (List.length recovered.Parser.samples)
      (List.length original.Parser.samples);
    (* Compare samples (with quantization tolerance) *)
    List.iter2
      (fun original_sample recovered_sample ->
        check float_approx "Real file roundtrip preserves sample" original_sample recovered_sample)
      original.Parser.samples recovered.Parser.samples;
    Sys.remove test_file
  with
  | e ->
      (try Sys.remove test_file with _ -> ());
      raise e

let test_empty_file_error () =
  let empty_file = Filename.temp_file "empty" ".wav" in
  try
    let _ = Parser.read_wav empty_file in
    check bool "Should raise exception for empty file" false true
  with
  | Parser.Invalid_wav_file _ -> (
      try Sys.remove empty_file with _ -> ();
      check bool "Correctly raises Invalid_wav_file" true true)
  | e ->
      (try Sys.remove empty_file with _ -> ());
      raise e

let () =
  run "WAV Parser and Writer"
    [
      "WAV Parser", [test_case "Read WAV info" `Quick test_read_wav_info; test_case "Read WAV file" `Quick test_read_wav];
      "WAV Writer - Roundtrip 16-bit",
      [
        test_case "Write/read roundtrip 16-bit mono" `Quick test_write_read_roundtrip_16bit;
        test_case "Write/read roundtrip 16-bit stereo" `Quick test_write_read_roundtrip_stereo;
      ];
      "WAV Writer - Roundtrip 32-bit",
      [
        test_case "Write/read roundtrip 32-bit PCM" `Quick test_write_read_roundtrip_32bit;
        test_case "Write/read roundtrip 32-bit precision" `Quick test_write_read_roundtrip_32bit_precision;
      ];
      "WAV Writer - Edge cases",
      [
        test_case "Clamp out-of-range samples" `Quick test_clamp_samples;
        test_case "Read/write real file" `Quick test_read_write_real_file;
        test_case "Error on empty file" `Quick test_empty_file_error;
      ];
    ]

