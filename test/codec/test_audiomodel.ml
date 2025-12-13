(* Unit tests for AudioModel module *)
open Alcotest

module AudioModel = Codec.Audiomodel

let float_approx = float 0.0001

let test_window_type_creation () =
  let _ = AudioModel.Short in
  let _ = AudioModel.Long in
  let _ = AudioModel.VeryLong in
  let _ = AudioModel.EndFocused in
  let _ = AudioModel.StartFocused in
  check bool "Window types can be created" true true

let test_segment_creation () =
  let segment = {
    AudioModel.window_type = AudioModel.Short;
    window_data = None;
    start_sample = None;
    end_sample = None;
    attack_ms = None;
    release_ms = None;
    original_length = None;
    num_bands = 2;
    frequency_bands = [(0.0, 0.5); (0.5, 1.0)];
    band_ranges = [(-1.0, 1.0); (-0.5, 0.5)];
    quantization_levels = [4; 8];
    quantized_data = Some [[0; 1; 2; 3]; [0; 1; 2; 3; 4; 5; 6; 7]];
    raw_data = None;
  } in
  check int "Segment num_bands" segment.AudioModel.num_bands 2;
  check int "Segment frequency_bands length" 
    (List.length segment.AudioModel.frequency_bands) 2;
  check int "Segment quantization_levels length"
    (List.length segment.AudioModel.quantization_levels) 2

let test_segment_with_raw_data () =
  let segment = {
    AudioModel.window_type = AudioModel.Long;
    window_data = None;
    start_sample = None;
    end_sample = None;
    attack_ms = None;
    release_ms = None;
    original_length = None;
    num_bands = 1;
    frequency_bands = [(0.0, 1.0)];
    band_ranges = [(-1.0, 1.0)];
    quantization_levels = [16];
    quantized_data = None;
    raw_data = Some [[0.1; 0.2; 0.3; 0.4]];
  } in
  check bool "Segment has raw_data" 
    (match segment.AudioModel.raw_data with Some _ -> true | None -> false) true;
  check bool "Segment has no quantized_data"
    (match segment.AudioModel.quantized_data with Some _ -> false | None -> true) true

let test_track_creation () =
  let segment1 = {
    AudioModel.window_type = AudioModel.Short;
    window_data = None;
    start_sample = None;
    end_sample = None;
    attack_ms = None;
    release_ms = None;
    original_length = None;
    num_bands = 1;
    frequency_bands = [(0.0, 1.0)];
    band_ranges = [(-1.0, 1.0)];
    quantization_levels = [4];
    quantized_data = Some [[0; 1; 2; 3]];
    raw_data = None;
  } in
  let track = {
    AudioModel.num_segments = 1;
    segments = [segment1];
    length_samples = 1000;
    sample_rate = 44100;
    compression_params = Some (-1.0, 1.0);
  } in
  check int "Track num_segments" track.AudioModel.num_segments 1;
  check int "Track length_samples" track.AudioModel.length_samples 1000;
  check int "Track sample_rate" track.AudioModel.sample_rate 44100;
  check bool "Track has compression_params"
    (match track.AudioModel.compression_params with Some _ -> true | None -> false) true

let test_audio_file_creation () =
  let segment = {
    AudioModel.window_type = AudioModel.Long;
    window_data = None;
    start_sample = None;
    end_sample = None;
    attack_ms = None;
    release_ms = None;
    original_length = None;
    num_bands = 1;
    frequency_bands = [(0.0, 1.0)];
    band_ranges = [(-1.0, 1.0)];
    quantization_levels = [8];
    quantized_data = Some [[0; 1; 2; 3; 4; 5; 6; 7]];
    raw_data = None;
  } in
  let track = {
    AudioModel.num_segments = 1;
    segments = [segment];
    length_samples = 2000;
    sample_rate = 44100;
    compression_params = None;
  } in
  let audio_file = {
    AudioModel.num_tracks = 1;
    tracks = [track];
    bits_per_sample = 16;
    sample_rate = 44100;
  } in
  check int "AudioFile num_tracks" audio_file.AudioModel.num_tracks 1;
  check int "AudioFile bits_per_sample" audio_file.AudioModel.bits_per_sample 16;
  check int "AudioFile sample_rate" audio_file.AudioModel.sample_rate 44100

let test_empty_track () =
  let track = {
    AudioModel.num_segments = 0;
    segments = [];
    length_samples = 0;
    sample_rate = 44100;
    compression_params = None;
  } in
  check int "Empty track num_segments" track.AudioModel.num_segments 0;
  check int "Empty track segments length" (List.length track.AudioModel.segments) 0

let test_standard_frequencies () =
  let freqs = AudioModel.standard_10band_frequencies_hz in
  check int "Standard frequencies count" (List.length freqs) 11;
  (* Check first and last frequencies *)
  check (float 0.1) "First frequency is 20Hz" (List.hd freqs) 20.0;
  check (float 0.1) "Last frequency is 20kHz" 
    (List.nth freqs (List.length freqs - 1)) 20000.0

let test_polybank_filter_basic () =
  (* Reduced size from 1000 to 128 for test speed (DFT is O(n²)) *)
  let signal = List.init 128 (fun i -> Float.sin (2.0 *. Float.pi *. float_of_int i /. 16.0)) in
  let sample_rate = 44100 in
  let bands = AudioModel.polybank_filter signal AudioModel.standard_10band_frequencies_hz sample_rate in
  check int "Polybank returns 10 bands" (List.length bands) 10;
  (* Each band should have same length as input *)
  List.iter
    (fun band ->
      check int "Band has correct length" (List.length band) (List.length signal))
    bands

let test_polybank_filter_custom () =
  (* Reduced size from 512 to 64 for test speed (DFT is O(n²)) *)
  let signal = List.init 64 (fun i -> Float.sin (2.0 *. Float.pi *. float_of_int i /. 8.0)) in
  let sample_rate = 44100 in
  let custom_freqs = [100.0; 1000.0; 5000.0; 10000.0] in
  let bands = AudioModel.polybank_filter signal custom_freqs sample_rate in
  (* Should have 3 bands (4 boundaries = 3 bands) *)
  check int "Custom polybank returns correct number of bands" (List.length bands) 3

let test_polybank_filter_empty () =
  let signal = [] in
  let sample_rate = 44100 in
  let bands = AudioModel.polybank_filter signal AudioModel.standard_10band_frequencies_hz sample_rate in
  check (list (list float_approx)) "Polybank on empty signal" bands []

let test_polybank_filter_single_band () =
  let signal = [1.0; 2.0; 3.0; 4.0; 5.0] in
  let sample_rate = 44100 in
  let freqs = [0.0; 22050.0] in  (* Single band covering all frequencies *)
  let bands = AudioModel.polybank_filter signal freqs sample_rate in
  check int "Single band polybank" (List.length bands) 1;
  check int "Single band has correct length" (List.length (List.hd bands)) (List.length signal)

let test_determine_window_short () =
  (* Short: attack < 5ms OR release < 5ms *)
  check bool "Short window: fast attack" 
    (AudioModel.determine_window_type 3.0 10.0 = AudioModel.Short) true;
  check bool "Short window: fast release"
    (AudioModel.determine_window_type 10.0 3.0 = AudioModel.Short) true;
  check bool "Short window: both fast"
    (AudioModel.determine_window_type 2.0 2.0 = AudioModel.Short) true

let test_determine_window_end_focused () =
  (* EndFocused: attack < 5ms AND release > 20ms *)
  check bool "EndFocused window"
    (AudioModel.determine_window_type 3.0 25.0 = AudioModel.EndFocused) true

let test_determine_window_start_focused () =
  (* StartFocused: attack > 20ms AND release < 5ms *)
  check bool "StartFocused window"
    (AudioModel.determine_window_type 25.0 3.0 = AudioModel.StartFocused) true

let test_determine_window_very_long () =
  (* VeryLong: attack > 50ms AND release > 50ms *)
  check bool "VeryLong window"
    (AudioModel.determine_window_type 60.0 60.0 = AudioModel.VeryLong) true;
  check bool "VeryLong window: both very long"
    (AudioModel.determine_window_type 100.0 100.0 = AudioModel.VeryLong) true

let test_determine_window_long () =
  (* Long: default (all other cases) *)
  check bool "Long window: medium attack and release"
    (AudioModel.determine_window_type 15.0 15.0 = AudioModel.Long) true;
  check bool "Long window: medium attack, long release"
    (AudioModel.determine_window_type 15.0 30.0 = AudioModel.Long) true;
  check bool "Long window: long attack, medium release"
    (AudioModel.determine_window_type 30.0 15.0 = AudioModel.Long) true

let test_determine_window_edge_cases () =
  (* Edge cases at boundaries *)
  check bool "Boundary: attack = 5ms, release = 5ms -> Short"
    (AudioModel.determine_window_type 4.9 4.9 = AudioModel.Short) true;
  check bool "Boundary: attack = 5ms, release = 20ms -> Long"
    (AudioModel.determine_window_type 5.1 20.1 = AudioModel.Long) true;
  check bool "Boundary: attack = 50ms, release = 50ms -> Long"
    (AudioModel.determine_window_type 49.9 49.9 = AudioModel.Long) true;
  check bool "Boundary: attack = 50ms, release = 50ms -> VeryLong"
    (AudioModel.determine_window_type 50.1 50.1 = AudioModel.VeryLong) true

let test_find_optimal_quantization_basic () =
  let signal = List.init 100 (fun i -> Float.sin (2.0 *. Float.pi *. float_of_int i /. 10.0)) in
  let (n, min_val, max_val) = AudioModel.find_optimal_quantization signal in
  check bool "Optimal n is positive" (n > 0) true;
  check bool "Optimal n is reasonable" (n <= 256) true;
  check bool "Min val is less than max val" (min_val < max_val) true

let test_find_optimal_quantization_snr () =
  let signal = List.init 200 (fun i -> Float.sin (2.0 *. Float.pi *. float_of_int i /. 20.0)) in
  let (n, min_val, max_val) = AudioModel.find_optimal_quantization ~snr_threshold:30.0 signal in
  (* Verify SNR is maintained *)
  let quantized, _, _ = Codec_utilities.Quantization.quantize_with_range signal n in
  let dequantized = Codec_utilities.Quantization.dequantize_with_range quantized n min_val max_val in
  let snr = Codec_utilities.Dsp.snr signal dequantized in
  check bool "SNR meets threshold" (snr >= 30.0) true

let test_find_optimal_quantization_constant () =
  let signal = List.init 100 (fun _ -> 5.0) in
  let (n, min_val, max_val) = AudioModel.find_optimal_quantization signal in
  check int "Constant signal uses minimal n" n 2;
  check (float 0.1) "Constant signal min equals max" min_val max_val

let test_find_optimal_quantization_empty () =
  let signal = [] in
  let (n, min_val, max_val) = AudioModel.find_optimal_quantization signal in
  check int "Empty signal returns default n" n 2;
  check (float 0.1) "Empty signal min and max are 0" min_val 0.0;
  check (float 0.1) "Empty signal max is 0" max_val 0.0

let test_find_optimal_quantization_high_threshold () =
  let signal = List.init 100 (fun i -> Float.sin (2.0 *. Float.pi *. float_of_int i /. 10.0)) in
  let (n, _, _) = AudioModel.find_optimal_quantization ~snr_threshold:60.0 signal in
  (* High threshold should require more quantization levels *)
  check bool "High threshold requires reasonable n" (n >= 2 && n <= 256) true

let test_compress_track_basic () =
  let signal = [0.0; 0.5; 1.0; 1.5; 2.0] in
  let (compressed, min_val, max_val) = AudioModel.compress_track signal in
  check int "Compressed length matches" (List.length compressed) (List.length signal);
  check (float 0.1) "Original min preserved" min_val 0.0;
  check (float 0.1) "Original max preserved" max_val 2.0;
  (* Check that compressed values are in [-1.0, 1.0] range *)
  List.iter
    (fun x -> check bool "Compressed value in range" (x >= -1.0 && x <= 1.0) true)
    compressed;
  (* Check first and last values *)
  check (float 0.1) "First value compressed to -1.0" (List.hd compressed) (-1.0);
  check (float 0.1) "Last value compressed to 1.0" 
    (List.nth compressed (List.length compressed - 1)) 1.0

let test_decompress_track_basic () =
  let compressed = [-1.0; -0.5; 0.0; 0.5; 1.0] in
  let decompressed = AudioModel.decompress_track compressed 0.0 2.0 in
  check int "Decompressed length matches" (List.length decompressed) (List.length compressed);
  check (float 0.1) "First value decompressed" (List.hd decompressed) 0.0;
  check (float 0.1) "Last value decompressed"
    (List.nth decompressed (List.length decompressed - 1)) 2.0

let test_compress_decompress_roundtrip () =
  let original = List.init 100 (fun i -> Float.sin (2.0 *. Float.pi *. float_of_int i /. 10.0)) in
  let (compressed, min_val, max_val) = AudioModel.compress_track original in
  let decompressed = AudioModel.decompress_track compressed min_val max_val in
  check int "Roundtrip preserves length" (List.length decompressed) (List.length original);
  (* Values should be approximately recovered *)
  List.iter2
    (fun orig dec ->
      check float_approx "Roundtrip recovers original" orig dec)
    original decompressed

let test_compress_track_constant () =
  let signal = List.init 10 (fun _ -> 5.0) in
  let (compressed, min_val, max_val) = AudioModel.compress_track signal in
  check (float 0.1) "Constant signal min equals max" min_val max_val;
  (* All compressed values should be 0.0 for constant signal *)
  List.iter
    (fun x -> check (float 0.1) "Constant compressed to 0.0" x 0.0)
    compressed

let test_compress_track_empty () =
  let signal = [] in
  let (compressed, min_val, max_val) = AudioModel.compress_track signal in
  check (list float_approx) "Empty signal compressed" compressed [];
  check (float 0.1) "Empty signal min" min_val 0.0;
  check (float 0.1) "Empty signal max" max_val 0.0

let test_decompress_track_empty () =
  let compressed = [] in
  let decompressed = AudioModel.decompress_track compressed 0.0 1.0 in
  check (list float_approx) "Empty signal decompressed" decompressed []

let test_lr_to_mid_side_basic () =
  let left = [1.0; 2.0; 3.0; 4.0] in
  let right = [0.0; 1.0; 2.0; 3.0] in
  let (mid, side) = AudioModel.lr_to_mid_side left right in
  check int "Mid channel length" (List.length mid) (List.length left);
  check int "Side channel length" (List.length side) (List.length left);
  (* mid[0] = (1.0 + 0.0) / 2 = 0.5 *)
  check float_approx "Mid channel first value" (List.hd mid) 0.5;
  (* side[0] = 1.0 - 0.5 = 0.5 *)
  check float_approx "Side channel first value" (List.hd side) 0.5

let test_mid_side_to_lr_basic () =
  let mid = [0.5; 1.5; 2.5; 3.5] in
  let side = [0.5; 0.5; 0.5; 0.5] in
  let (left, right) = AudioModel.mid_side_to_lr mid side in
  check int "Left channel length" (List.length left) (List.length mid);
  check int "Right channel length" (List.length right) (List.length mid);
  (* left[0] = 0.5 + 0.5 = 1.0 *)
  check float_approx "Left channel first value" (List.hd left) 1.0;
  (* right[0] = 0.5 - 0.5 = 0.0 *)
  check float_approx "Right channel first value" (List.hd right) 0.0

let test_lr_mid_side_roundtrip () =
  let left = List.init 100 (fun i -> Float.sin (2.0 *. Float.pi *. float_of_int i /. 10.0)) in
  let right = List.init 100 (fun i -> Float.cos (2.0 *. Float.pi *. float_of_int i /. 10.0)) in
  let (mid, side) = AudioModel.lr_to_mid_side left right in
  let (left_recovered, right_recovered) = AudioModel.mid_side_to_lr mid side in
  check int "Roundtrip preserves left length" (List.length left_recovered) (List.length left);
  check int "Roundtrip preserves right length" (List.length right_recovered) (List.length right);
  (* Values should be exactly recovered (no quantization error) *)
  List.iter2
    (fun orig recov -> check float_approx "Roundtrip recovers left" orig recov)
    left left_recovered;
  List.iter2
    (fun orig recov -> check float_approx "Roundtrip recovers right" orig recov)
    right right_recovered

let test_lr_to_mid_side_mono () =
  (* Mono signal: left = right *)
  let signal = [1.0; 2.0; 3.0; 4.0] in
  let (mid, side) = AudioModel.lr_to_mid_side signal signal in
  (* Mid should equal original, side should be all zeros *)
  List.iter2
    (fun orig m -> check float_approx "Mono mid equals original" orig m)
    signal mid;
  List.iter
    (fun s -> check float_approx "Mono side is zero" s 0.0)
    side

let test_lr_to_mid_side_empty () =
  let left = [] in
  let right = [] in
  let (mid, side) = AudioModel.lr_to_mid_side left right in
  check (list float_approx) "Empty mid channel" mid [];
  check (list float_approx) "Empty side channel" side []

let test_mid_side_to_lr_empty () =
  let mid = [] in
  let side = [] in
  let (left, right) = AudioModel.mid_side_to_lr mid side in
  check (list float_approx) "Empty left channel" left [];
  check (list float_approx) "Empty right channel" right []

let () =
  run "AudioModel Tests" [
    "Data structure creation", [
      test_case "Window type creation" `Quick test_window_type_creation;
      test_case "Segment creation" `Quick test_segment_creation;
      test_case "Segment with raw data" `Quick test_segment_with_raw_data;
      test_case "Track creation" `Quick test_track_creation;
      test_case "Audio file creation" `Quick test_audio_file_creation;
      test_case "Empty track" `Quick test_empty_track;
    ];
    "Polybank filter", [
      test_case "Standard frequencies" `Quick test_standard_frequencies;
      test_case "Polybank filter basic" `Quick test_polybank_filter_basic;
      test_case "Polybank filter custom" `Quick test_polybank_filter_custom;
      test_case "Polybank filter empty" `Quick test_polybank_filter_empty;
      test_case "Polybank filter single band" `Quick test_polybank_filter_single_band;
    ];
    "Window type FSM", [
      test_case "Determine window: Short" `Quick test_determine_window_short;
      test_case "Determine window: EndFocused" `Quick test_determine_window_end_focused;
      test_case "Determine window: StartFocused" `Quick test_determine_window_start_focused;
      test_case "Determine window: VeryLong" `Quick test_determine_window_very_long;
      test_case "Determine window: Long" `Quick test_determine_window_long;
      test_case "Determine window: edge cases" `Quick test_determine_window_edge_cases;
    ];
    "Adaptive quantization", [
      test_case "Find optimal quantization: basic" `Quick test_find_optimal_quantization_basic;
      test_case "Find optimal quantization: SNR" `Quick test_find_optimal_quantization_snr;
      test_case "Find optimal quantization: constant" `Quick test_find_optimal_quantization_constant;
      test_case "Find optimal quantization: empty" `Quick test_find_optimal_quantization_empty;
      test_case "Find optimal quantization: high threshold" `Quick test_find_optimal_quantization_high_threshold;
    ];
    "Track compression", [
      test_case "Compress track: basic" `Quick test_compress_track_basic;
      test_case "Decompress track: basic" `Quick test_decompress_track_basic;
      test_case "Compress-decompress roundtrip" `Quick test_compress_decompress_roundtrip;
      test_case "Compress track: constant" `Quick test_compress_track_constant;
      test_case "Compress track: empty" `Quick test_compress_track_empty;
      test_case "Decompress track: empty" `Quick test_decompress_track_empty;
    ];
    "Channel transformations", [
      test_case "LR to mid-side: basic" `Quick test_lr_to_mid_side_basic;
      test_case "Mid-side to LR: basic" `Quick test_mid_side_to_lr_basic;
      test_case "LR-mid-side roundtrip" `Quick test_lr_mid_side_roundtrip;
      test_case "LR to mid-side: mono" `Quick test_lr_to_mid_side_mono;
      test_case "LR to mid-side: empty" `Quick test_lr_to_mid_side_empty;
      test_case "Mid-side to LR: empty" `Quick test_mid_side_to_lr_empty;
    ];
  ]

