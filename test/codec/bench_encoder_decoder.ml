(* Benchmark tests for encoder/decoder functions *)

module Encoder = Codec.Encoder
module Decoder = Codec.Decoder
module Audiomodel = Codec.Audiomodel
module Dsp = Codec_utilities.Dsp
module Wav = Wav

(* Helper to measure time *)
let time_function name f =
  let start_time = Sys.time () in
  let result = f () in
  let end_time = Sys.time () in
  let elapsed = end_time -. start_time in
  Printf.printf "  %s: %.4f seconds\n" name elapsed;
  result

(* Generate test data *)
let generate_test_data size =
  List.init size (fun i ->
      Float.sin (2.0 *. Float.pi *. float_of_int i /. 100.0))

(* Benchmark compression *)
let bench_compress_track () =
  let data = generate_test_data 10000 in
  time_function "compress_track (10k samples)" (fun () ->
      let _compressed, _min_val, _max_val, _snr = Encoder.compress_track data in
      ())

(* Benchmark LR to Mid-Side *)
let bench_lr_to_mid_side () =
  let data = generate_test_data 10000 in
  time_function "lr_to_mid_side (10k samples)" (fun () ->
      let _mid, _side, _is_stereo = Encoder.lr_to_mid_side data data in
      ())

(* Benchmark silence replacement *)
let bench_replace_silence () =
  let data = generate_test_data 10000 in
  let sample_rate = 44100 in
  time_function "replace_silence_after_loud (10k samples)" (fun () ->
      let _processed, _mask =
        Encoder.replace_silence_after_loud sample_rate data
      in
      ())

(* Benchmark segmentation *)
let bench_segment_track () =
  let data = generate_test_data 10000 in
  let sample_rate = 44100 in
  time_function "segment_track (10k samples)" (fun () ->
      let _segmented, _segments_info = Encoder.segment_track sample_rate data in
      ())

(* Benchmark windowing *)
let bench_create_windows () =
  let data = generate_test_data 10000 in
  let sample_rate = 44100 in
  let _segmented, segments_info = Encoder.segment_track sample_rate data in
  time_function "create_overlapping_windows (10k samples)" (fun () ->
      let _windows =
        Encoder.create_overlapping_windows _segmented segments_info sample_rate
      in
      ())

(* Benchmark MDCT Level 1 *)
let bench_mdct_level1 () =
  let data = generate_test_data 10000 in
  let sample_rate = 44100 in
  let segmented, segments_info = Encoder.segment_track sample_rate data in
  let windows =
    Encoder.create_overlapping_windows segmented segments_info sample_rate
  in
  time_function "apply_mdct_level1" (fun () ->
      let _mdct_segments = Encoder.apply_mdct_level1 windows in
      ())

(* Benchmark frequency bands split *)
let bench_split_bands () =
  let data = generate_test_data 10000 in
  let sample_rate = 44100 in
  let segmented, segments_info = Encoder.segment_track sample_rate data in
  let windows =
    Encoder.create_overlapping_windows segmented segments_info sample_rate
  in
  let mdct_segments = Encoder.apply_mdct_level1 windows in
  time_function "split_frequency_bands" (fun () ->
      let _band_segments =
        Encoder.split_frequency_bands sample_rate mdct_segments
      in
      ())

(* Benchmark IMDCT to bands *)
let bench_imdct_to_bands () =
  let data = generate_test_data 10000 in
  let sample_rate = 44100 in
  let segmented, segments_info = Encoder.segment_track sample_rate data in
  let windows =
    Encoder.create_overlapping_windows segmented segments_info sample_rate
  in
  let mdct_segments = Encoder.apply_mdct_level1 windows in
  let band_segments = Encoder.split_frequency_bands sample_rate mdct_segments in
  time_function "apply_imdct_to_bands" (fun () ->
      let _imdct_segments = Encoder.apply_imdct_to_bands band_segments in
      ())

(* Benchmark MDCT Level 2 *)
let bench_mdct_level2 () =
  let data = generate_test_data 10000 in
  let sample_rate = 44100 in
  let segmented, segments_info = Encoder.segment_track sample_rate data in
  let windows =
    Encoder.create_overlapping_windows segmented segments_info sample_rate
  in
  let mdct_segments = Encoder.apply_mdct_level1 windows in
  let band_segments = Encoder.split_frequency_bands sample_rate mdct_segments in
  let imdct_segments = Encoder.apply_imdct_to_bands band_segments in
  time_function "apply_mdct_level2" (fun () ->
      let _mdct2_segments = Encoder.apply_mdct_level2 imdct_segments in
      ())

(* Benchmark quantization thresholds *)
let bench_quantization_thresholds () =
  let data = generate_test_data 10000 in
  let sample_rate = 44100 in
  let segmented, segments_info = Encoder.segment_track sample_rate data in
  let windows =
    Encoder.create_overlapping_windows segmented segments_info sample_rate
  in
  let mdct_segments = Encoder.apply_mdct_level1 windows in
  let band_segments = Encoder.split_frequency_bands sample_rate mdct_segments in
  let imdct_segments = Encoder.apply_imdct_to_bands band_segments in
  let mdct2_segments = Encoder.apply_mdct_level2 imdct_segments in
  time_function "select_quantization_thresholds" (fun () ->
      let _quant_segments =
        Encoder.select_quantization_thresholds mdct2_segments
      in
      ())

(* Benchmark full encode pipeline with detailed breakdown *)
let bench_full_encode_detailed () =
  let data = generate_test_data 10000 in
  let sample_rate = 44100 in
  Printf.printf "  Full encode pipeline (10k samples) - detailed:\n";
  let compressed, _min_val, _max_val, _snr =
    time_function "    - compress_track" (fun () -> Encoder.compress_track data)
  in
  let mid, _side, _is_stereo =
    time_function "    - lr_to_mid_side" (fun () ->
        Encoder.lr_to_mid_side compressed compressed)
  in
  let processed, _mask =
    time_function "    - replace_silence_after_loud" (fun () ->
        Encoder.replace_silence_after_loud sample_rate mid)
  in
  let segmented, segments_info =
    time_function "    - segment_track" (fun () ->
        Encoder.segment_track sample_rate processed)
  in
  let windows =
    time_function "    - create_overlapping_windows" (fun () ->
        Encoder.create_overlapping_windows segmented segments_info sample_rate)
  in
  let mdct_segments =
    time_function "    - apply_mdct_level1" (fun () ->
        Encoder.apply_mdct_level1 windows)
  in
  let band_segments =
    time_function "    - split_frequency_bands" (fun () ->
        Encoder.split_frequency_bands sample_rate mdct_segments)
  in
  let imdct_segments =
    time_function "    - apply_imdct_to_bands" (fun () ->
        Encoder.apply_imdct_to_bands band_segments)
  in
  let mdct2_segments =
    time_function "    - apply_mdct_level2" (fun () ->
        Encoder.apply_mdct_level2 imdct_segments)
  in
  let _quant_segments =
    time_function "    - select_quantization_thresholds" (fun () ->
        Encoder.select_quantization_thresholds mdct2_segments)
  in
  ()

(* Benchmark full encode pipeline *)
let bench_full_encode () =
  let data = generate_test_data 10000 in
  let sample_rate = 44100 in
  time_function "Full encode pipeline (10k samples)" (fun () ->
      let _compressed, _min_val, _max_val, _snr = Encoder.compress_track data in
      let _mid, _side, _is_stereo =
        Encoder.lr_to_mid_side _compressed _compressed
      in
      let _processed, _mask =
        Encoder.replace_silence_after_loud sample_rate _mid
      in
      let _segmented, segments_info =
        Encoder.segment_track sample_rate _processed
      in
      let windows =
        Encoder.create_overlapping_windows _segmented segments_info sample_rate
      in
      let mdct_segments = Encoder.apply_mdct_level1 windows in
      let band_segments =
        Encoder.split_frequency_bands sample_rate mdct_segments
      in
      let imdct_segments = Encoder.apply_imdct_to_bands band_segments in
      let mdct2_segments = Encoder.apply_mdct_level2 imdct_segments in
      let _quant_segments =
        Encoder.select_quantization_thresholds mdct2_segments
      in
      ())

(* Benchmark WAV read *)
let bench_wav_read () =
  let test_file = "test/wav/test_sample.wav" in
  if Sys.file_exists test_file then
    time_function "WAV read" (fun () ->
        let _wav_data = Wav.Parser.read_wav test_file in
        ())
  else Printf.printf "  WAV read: skipped (file not found)\n"

(* Benchmark WAV write *)
let bench_wav_write () =
  let test_file = "test/wav/test_sample.wav" in
  if Sys.file_exists test_file then (
    let wav_data = Wav.Parser.read_wav test_file in
    let samples = wav_data.Wav.Parser.samples in
    let info = wav_data.Wav.Parser.info in
    let temp_file = Filename.temp_file "bench_wav" ".wav" in
    time_function "WAV write" (fun () ->
        Wav.Writer.write_wav temp_file
          {
            Wav.Writer.sample_rate = info.Wav.Parser.sample_rate;
            Wav.Writer.num_channels = info.Wav.Parser.num_channels;
            Wav.Writer.bits_per_sample = info.Wav.Parser.bits_per_sample;
          }
          samples);
    Sys.remove temp_file)
  else Printf.printf "  WAV write: skipped (file not found)\n"

(* Benchmark find_optimal_quantization *)
let bench_find_optimal_quantization () =
  let data = generate_test_data 1000 in
  time_function "find_optimal_quantization (1k samples)" (fun () ->
      let _n, _min_val, _max_val = Audiomodel.find_optimal_quantization data in
      ())

(* Benchmark SNR calculation *)
let bench_snr () =
  let data1 = generate_test_data 10000 in
  let data2 = List.map (fun x -> x *. 0.99) data1 in
  time_function "SNR calculation (10k samples)" (fun () ->
      let _snr = Dsp.snr data1 data2 in
      ())

(* Benchmark decoder operations *)
let bench_decoder_operations () =
  (* First encode to get segments *)
  let data = generate_test_data 10000 in
  let sample_rate = 44100 in
  let compressed, _min_val, _max_val, _snr = Encoder.compress_track data in
  let mid, _side, _is_stereo = Encoder.lr_to_mid_side compressed compressed in
  let processed, _mask = Encoder.replace_silence_after_loud sample_rate mid in
  let segmented, segments_info = Encoder.segment_track sample_rate processed in
  let windows =
    Encoder.create_overlapping_windows segmented segments_info sample_rate
  in
  let mdct_segments = Encoder.apply_mdct_level1 windows in
  let band_segments = Encoder.split_frequency_bands sample_rate mdct_segments in
  let imdct_segments = Encoder.apply_imdct_to_bands band_segments in
  let mdct2_segments = Encoder.apply_mdct_level2 imdct_segments in
  let quant_segments = Encoder.select_quantization_thresholds mdct2_segments in

  Printf.printf "\nDecoder operations:\n";
  let imdct2_segments =
    time_function "  apply_imdct_level2" (fun () ->
        Decoder.apply_imdct_level2 quant_segments)
  in
  let mdct1_segments =
    time_function "  apply_mdct_level1_to_bands" (fun () ->
        Decoder.apply_mdct_level1_to_bands imdct2_segments)
  in
  let merged_segments =
    time_function "  merge_frequency_bands" (fun () ->
        Decoder.merge_frequency_bands mdct1_segments)
  in
  let window_segments =
    time_function "  apply_imdct_final" (fun () ->
        Decoder.apply_imdct_final merged_segments)
  in
  let _reconstructed =
    time_function "  overlap_add_windows" (fun () ->
        Decoder.overlap_add_windows window_segments)
  in
  ()

(* Helper to measure time and return elapsed time *)
let measure_time name f =
  let start_time = Sys.time () in
  let _result = f () in
  let end_time = Sys.time () in
  let elapsed = end_time -. start_time in
  Printf.printf "  %s: %.4f seconds\n" name elapsed;
  elapsed

(* Benchmark with different data sizes *)
let bench_different_sizes () =
  Printf.printf "\nPerformance by data size:\n";
  let sizes = [ 1000; 5000; 10000; 20000 ] in
  List.iter
    (fun size ->
      let data = generate_test_data size in
      let sample_rate = 44100 in
      let elapsed =
        measure_time (Printf.sprintf "  Full encode (%d samples)" size)
          (fun () ->
            let _compressed, _min_val, _max_val, _snr =
              Encoder.compress_track data
            in
            let _mid, _side, _is_stereo =
              Encoder.lr_to_mid_side _compressed _compressed
            in
            let _processed, _mask =
              Encoder.replace_silence_after_loud sample_rate _mid
            in
            let _segmented, segments_info =
              Encoder.segment_track sample_rate _processed
            in
            let windows =
              Encoder.create_overlapping_windows _segmented segments_info
                sample_rate
            in
            let mdct_segments = Encoder.apply_mdct_level1 windows in
            let band_segments =
              Encoder.split_frequency_bands sample_rate mdct_segments
            in
            let imdct_segments = Encoder.apply_imdct_to_bands band_segments in
            let mdct2_segments = Encoder.apply_mdct_level2 imdct_segments in
            let _quant_segments =
              Encoder.select_quantization_thresholds mdct2_segments
            in
            ())
      in
      let samples_per_sec = float_of_int size /. elapsed in
      Printf.printf "    -> %.0f samples/second\n" samples_per_sec)
    sizes

let () =
  Printf.printf "\n=== Encoder/Decoder Benchmarks ===\n\n";
  Printf.printf "Individual functions:\n";
  bench_compress_track ();
  bench_lr_to_mid_side ();
  bench_replace_silence ();
  bench_segment_track ();
  bench_create_windows ();
  bench_mdct_level1 ();
  bench_split_bands ();
  bench_imdct_to_bands ();
  bench_mdct_level2 ();
  bench_quantization_thresholds ();
  bench_find_optimal_quantization ();
  bench_snr ();
  Printf.printf "\nFull pipeline:\n";
  bench_full_encode ();
  Printf.printf "\nFull pipeline (detailed breakdown):\n";
  bench_full_encode_detailed ();
  bench_decoder_operations ();
  bench_different_sizes ();
  Printf.printf "\nWAV I/O:\n";
  bench_wav_read ();
  bench_wav_write ();
  Printf.printf "\n===================================\n\n"
