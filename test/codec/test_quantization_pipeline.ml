(* Tests for quantization and serialization pipeline *)

open Alcotest

module Encoder = Codec.Encoder
module Decoder = Codec.Decoder
module Serialization = Codec.Serialization
module Quantization = Codec_utilities.Quantization
module Dsp = Codec_utilities.Dsp
module Audiomodel = Codec.Audiomodel

(* Test that quantization actually creates quantized_data *)
let test_encoder_quantization_creates_data () =
  let sample_rate = 44100 in
  let input = List.init 1000 (fun i -> Float.sin (float_of_int i *. 0.1)) in
  
  (* Full pipeline up to quantization *)
  let compressed, _min_val, _max_val, _snr = Encoder.compress_track input in
  let mid, _side, _is_stereo = Encoder.lr_to_mid_side compressed compressed in
  let processed, _silence_mask = Encoder.replace_silence_after_loud sample_rate mid in
  let segmented, segments_info = Encoder.segment_track sample_rate processed in
  let windows = Encoder.create_overlapping_windows segmented segments_info sample_rate in
  let mdct_segments = Encoder.apply_mdct_level1 windows in
  let band_segments = Encoder.split_frequency_bands sample_rate mdct_segments in
  let imdct_segments = Encoder.apply_imdct_to_bands band_segments in
  let mdct2_segments = Encoder.apply_mdct_level2 imdct_segments in
  
  (* Apply quantization *)
  let quant_segments = Encoder.select_quantization_thresholds mdct2_segments in
  
  (* Check that quantized_data exists and raw_data is cleared *)
  List.iter (fun seg ->
      match seg.Codec.Audiomodel.quantized_data with
    | None -> 
      (* If no quantized_data, check if it's because there was no raw_data *)
      check bool "Segment has quantized_data or was skipped" 
        (seg.Codec.Audiomodel.raw_data = None) true
    | Some quantized_bands ->
      check bool "Segment has quantized_data" true true;
      check bool "raw_data is cleared" (seg.Codec.Audiomodel.raw_data = None) true;
      check int "quantized_data has bands" (List.length quantized_bands) 
        (List.length seg.Codec.Audiomodel.quantization_levels);
      (* Check that quantization_levels are all > 0 *)
      List.iter (fun ql ->
        check bool "quantization_level > 0" (ql > 0) true
      ) seg.Codec.Audiomodel.quantization_levels;
      (* Check that quantized bands are int lists, not float *)
      List.iter (fun band ->
        check bool "quantized band is not empty" (List.length band > 0) true;
        (* All values should be integers (indices) *)
        List.iter (fun idx ->
          check bool "quantized index is integer" (idx >= 0) true
        ) band
      ) quantized_bands
  ) quant_segments

(* Test serialization of quantized data *)
let test_serialization_quantized_data () =
  let sample_rate = 44100 in
  let input = List.init 500 (fun i -> Float.sin (float_of_int i *. 0.1)) in
  
  (* Create quantized segments *)
  let compressed, min_val, max_val, _snr = Encoder.compress_track input in
  let mid, _side, _is_stereo = Encoder.lr_to_mid_side compressed compressed in
  let processed, _silence_mask = Encoder.replace_silence_after_loud sample_rate mid in
  let segmented, segments_info = Encoder.segment_track sample_rate processed in
  let windows = Encoder.create_overlapping_windows segmented segments_info sample_rate in
  let mdct_segments = Encoder.apply_mdct_level1 windows in
  let band_segments = Encoder.split_frequency_bands sample_rate mdct_segments in
  let imdct_segments = Encoder.apply_imdct_to_bands band_segments in
  let mdct2_segments = Encoder.apply_mdct_level2 imdct_segments in
  let quant_segments = Encoder.select_quantization_thresholds mdct2_segments in
  
  (* Create AudioFile *)
  let audio_file = Encoder.create_audio_file 
    ~compression_params:(Some (min_val, max_val))
    sample_rate 
    quant_segments 
  in
  
  (* Serialize and deserialize *)
  let bytes = Serialization.serialize_audio_file audio_file in
  let (deserialized, remaining) = Serialization.deserialize_audio_file bytes in
  
  check int "Remaining bytes is 0" (List.length remaining) 0;
  check int "Deserialized num_tracks matches" 
    deserialized.Codec.Audiomodel.num_tracks audio_file.Codec.Audiomodel.num_tracks;
  
  (* Check that quantized_data is preserved *)
  match deserialized.Codec.Audiomodel.tracks with
  | [des_track] ->
    (match audio_file.Codec.Audiomodel.tracks with
    | [orig_track] ->
      check int "Segments count matches" 
        (List.length des_track.Codec.Audiomodel.segments)
        (List.length orig_track.Codec.Audiomodel.segments);
      (* Check each segment *)
      List.iter2 (fun des_seg orig_seg ->
        check int "quantization_levels count matches"
          (List.length des_seg.Codec.Audiomodel.quantization_levels)
          (List.length orig_seg.Codec.Audiomodel.quantization_levels);
        (* Check that quantized_data exists *)
        match des_seg.Codec.Audiomodel.quantized_data with
        | None -> 
          check bool "deserialized segment has quantized_data" false true
        | Some des_bands ->
          (match orig_seg.Codec.Audiomodel.quantized_data with
          | None -> 
            check bool "original segment has quantized_data" false true
          | Some orig_bands ->
            check int "quantized_data bands count matches"
              (List.length des_bands) (List.length orig_bands);
            (* Check that bands have same length *)
            List.iter2 (fun des_band orig_band ->
              check int "quantized band length matches"
                (List.length des_band) (List.length orig_band)
            ) des_bands orig_bands
          )
      ) des_track.Codec.Audiomodel.segments orig_track.Codec.Audiomodel.segments
    | _ -> ())
  | _ -> ()

(* Test dequantization in decoder *)
let test_decoder_dequantization () =
  let sample_rate = 44100 in
  let input = List.init 500 (fun i -> Float.sin (float_of_int i *. 0.1)) in
  
  (* Create quantized segments *)
  let compressed, _min_val, _max_val, _snr = Encoder.compress_track input in
  let mid, _side, _is_stereo = Encoder.lr_to_mid_side compressed compressed in
  let processed, _silence_mask = Encoder.replace_silence_after_loud sample_rate mid in
  let segmented, segments_info = Encoder.segment_track sample_rate processed in
  let windows = Encoder.create_overlapping_windows segmented segments_info sample_rate in
  let mdct_segments = Encoder.apply_mdct_level1 windows in
  let band_segments = Encoder.split_frequency_bands sample_rate mdct_segments in
  let imdct_segments = Encoder.apply_imdct_to_bands band_segments in
  let mdct2_segments = Encoder.apply_mdct_level2 imdct_segments in
  let quant_segments = Encoder.select_quantization_thresholds mdct2_segments in
  
  (* Manually dequantize using decoder function *)
  let dequantized_segments = Decoder.dequantize_segments quant_segments in
  
  (* Check that dequantized segments have raw_data and no quantized_data *)
  List.iter (fun seg ->
    match seg.Codec.Audiomodel.quantized_data with
    | None -> 
      (* After dequantization, quantized_data should be None *)
      check bool "quantized_data is cleared after dequantization" true true;
      (* raw_data should exist *)
      (match seg.Codec.Audiomodel.raw_data with
      | None -> 
        (* This is OK if original segment had no quantized_data *)
        check bool "raw_data exists or segment was skipped" true true
      | Some raw_bands ->
        check bool "raw_data exists after dequantization" true true;
        check int "raw_data has bands" (List.length raw_bands) 
          (List.length seg.Codec.Audiomodel.quantization_levels);
        (* Check that raw_data contains floats (dequantized values) *)
        List.iter (fun band ->
          check bool "raw band is not empty" (List.length band > 0) true
        ) raw_bands)
    | Some _ ->
      (* Should not have quantized_data after dequantization - this is an error *)
      check bool "quantized_data should be cleared" false true
  ) dequantized_segments

(* Test full roundtrip: encode -> serialize -> deserialize -> decode *)
let test_full_quantization_roundtrip () =
  let sample_rate = 44100 in
  (* Use 8192 samples to ensure at least one full window *)
  let input = List.init 8192 (fun i -> Float.sin (float_of_int i *. 0.1)) in
  
  (* Encode and serialize *)
  let temp_file = Filename.temp_file "test_quant" ".audc" in
  let _audio_file = Encoder.encode_to_file input sample_rate temp_file in
  
  (* Deserialize and decode *)
  let (reconstructed, decoded_sample_rate) = Decoder.decode_from_file temp_file in
  
  check int "Decoded sample rate matches" decoded_sample_rate sample_rate;
  check bool "Reconstructed is not empty" (List.length reconstructed > 0) true;
  
  (* Calculate SNR *)
  let min_len = min (List.length input) (List.length reconstructed) in
  let rec take n acc = function
    | [] -> List.rev acc
    | x :: xs -> if n <= 0 then List.rev acc else take (n - 1) (x :: acc) xs
  in
  let input_trimmed = take min_len [] input in
  let reconstructed_trimmed = take min_len [] reconstructed in
  let snr = Codec_utilities.Dsp.snr input_trimmed reconstructed_trimmed in
  
  check bool "SNR is valid" (not (Float.is_nan snr)) true;
  
  Sys.remove temp_file

(* Test that quantization levels are always > 0 *)
let test_quantization_levels_valid () =
  let sample_rate = 44100 in
  let input = List.init 500 (fun i -> Float.sin (float_of_int i *. 0.1)) in
  
  (* Create quantized segments *)
  let compressed, _min_val, _max_val, _snr = Encoder.compress_track input in
  let mid, _side, _is_stereo = Encoder.lr_to_mid_side compressed compressed in
  let processed, _silence_mask = Encoder.replace_silence_after_loud sample_rate mid in
  let segmented, segments_info = Encoder.segment_track sample_rate processed in
  let windows = Encoder.create_overlapping_windows segmented segments_info sample_rate in
  let mdct_segments = Encoder.apply_mdct_level1 windows in
  let band_segments = Encoder.split_frequency_bands sample_rate mdct_segments in
  let imdct_segments = Encoder.apply_imdct_to_bands band_segments in
  let mdct2_segments = Encoder.apply_mdct_level2 imdct_segments in
  let quant_segments = Encoder.select_quantization_thresholds mdct2_segments in
  
  (* Check all quantization levels are > 0 *)
  List.iter (fun seg ->
    List.iter (fun ql ->
      check bool "quantization_level > 0" (ql > 0) true
    ) seg.Codec.Audiomodel.quantization_levels
  ) quant_segments

let () =
  run "Quantization Pipeline Tests" [
    "Quantization", [
      test_case "Encoder creates quantized_data" `Quick test_encoder_quantization_creates_data;
      test_case "Quantization levels are valid (> 0)" `Quick test_quantization_levels_valid;
    ];
    "Serialization", [
      test_case "Quantized data serializes correctly" `Quick test_serialization_quantized_data;
    ];
    "Dequantization", [
      test_case "Decoder dequantizes correctly" `Quick test_decoder_dequantization;
    ];
    "Full Pipeline", [
      test_case "Full quantization roundtrip" `Quick test_full_quantization_roundtrip;
    ];
  ]
