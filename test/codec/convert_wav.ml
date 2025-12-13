(* WAV to AUDC converter - creates files in different quality modes *)

module Encoder = Codec.Encoder
module Decoder = Codec.Decoder
module Wav = Wav
module Dsp = Codec_utilities.Dsp

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

let () =
  Printf.printf "\n╔════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║         WAV to AUDC Converter (3 Quality Modes)           ║\n";
  Printf.printf "╚════════════════════════════════════════════════════════════╝\n\n";
  
  (* Input file *)
  let input_file = "test/wav/test_sample.wav" in
  
  if not (Sys.file_exists input_file) then (
    Printf.eprintf "Error: Input file not found: %s\n" input_file;
    exit 1
  );
  
  Printf.printf "📁 Input file: %s\n" input_file;
  
  (* Read WAV *)
  Printf.printf "📖 Reading WAV file...\n";
  let wav_data = Wav.Parser.read_wav input_file in
  let sample_rate = wav_data.Wav.Parser.info.Wav.Parser.sample_rate in
  let num_channels = wav_data.Wav.Parser.info.Wav.Parser.num_channels in
  let bits_per_sample = wav_data.Wav.Parser.info.Wav.Parser.bits_per_sample in
  
  Printf.printf "   Sample rate: %d Hz\n" sample_rate;
  Printf.printf "   Channels: %d\n" num_channels;
  Printf.printf "   Bits per sample: %d\n" bits_per_sample;
  Printf.printf "   Total samples: %d\n" (List.length wav_data.Wav.Parser.samples);
  
  (* Extract mono - process full file now that we optimized! *)
  let mono_samples = extract_mono wav_data.Wav.Parser.samples num_channels in
  Printf.printf "   Mono samples: %d (~%.2f sec of audio)\n\n" 
    (List.length mono_samples)
    (float_of_int (List.length mono_samples) /. float_of_int sample_rate);
  
  (* Create output directory *)
  let output_dir = "converted_audio" in
  (try Unix.mkdir output_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  Printf.printf "📂 Output directory: %s/\n\n" output_dir;
  
  (* Quality modes *)
  let modes = [
    ("high", "High Quality", 60.0, "🔊");
    ("medium", "Medium Quality", 40.0, "🔉");
    ("low", "Low Quality", 20.0, "🔈");
  ] in
  
  (* Process each mode *)
  List.iter (fun (mode_id, mode_name, snr_threshold, emoji) ->
    Printf.printf "════════════════════════════════════════════════════════════\n";
    Printf.printf "%s  %s (SNR threshold: %.0f dB)\n" emoji mode_name snr_threshold;
    Printf.printf "════════════════════════════════════════════════════════════\n";
    
    let encoded_file = Filename.concat output_dir (Printf.sprintf "test_sample_%s.audc" mode_id) in
    let decoded_file = Filename.concat output_dir (Printf.sprintf "test_sample_%s.wav" mode_id) in
    
    (* Encode *)
    Printf.printf "⚙️  Encoding... (this may take a while for %d samples)\n" (List.length mono_samples);
    flush stdout;
    let start_time = Unix.gettimeofday () in
    let _audio_file = Encoder.encode_to_file ~snr_threshold_db:snr_threshold mono_samples sample_rate encoded_file in
    let encode_time = Unix.gettimeofday () -. start_time in
    
    let encoded_size = (Unix.stat encoded_file).Unix.st_size in
    Printf.printf "   ✓ Encoded in %.3f seconds\n" encode_time;
    Printf.printf "   ✓ File: %s\n" encoded_file;
    Printf.printf "   ✓ Size: %d bytes (%.2f KB)\n" encoded_size (float_of_int encoded_size /. 1024.0);
    
    (* Decode *)
    Printf.printf "⚙️  Decoding...\n";
    let start_time = Unix.gettimeofday () in
    let (reconstructed, decoded_sample_rate) = Decoder.decode_from_file encoded_file in
    let decode_time = Unix.gettimeofday () -. start_time in
    
    Printf.printf "   ✓ Decoded in %.3f seconds\n" decode_time;
    Printf.printf "   ✓ Reconstructed samples: %d\n" (List.length reconstructed);
    
    (* Calculate SNR *)
    let rec take n acc = function
      | [] -> List.rev acc
      | x :: xs -> if n <= 0 then List.rev acc else take (n - 1) (x :: acc) xs
    in
    let min_len = min (List.length mono_samples) (List.length reconstructed) in
    let input_trimmed = take min_len [] mono_samples in
    let reconstructed_trimmed = take min_len [] reconstructed in
    let snr = Dsp.snr input_trimmed reconstructed_trimmed in
    
    Printf.printf "   ✓ Achieved SNR: %.2f dB\n" snr;
    
    (* Write decoded WAV *)
    Printf.printf "💾 Writing WAV...\n";
    let wav_info = {
      Wav.Writer.sample_rate = decoded_sample_rate;
      Wav.Writer.num_channels = 1;
      Wav.Writer.bits_per_sample = bits_per_sample;
    } in
    Wav.Writer.write_wav decoded_file wav_info reconstructed;
    
    let decoded_size = (Unix.stat decoded_file).Unix.st_size in
    Printf.printf "   ✓ File: %s\n" decoded_file;
    Printf.printf "   ✓ Size: %d bytes (%.2f KB)\n" decoded_size (float_of_int decoded_size /. 1024.0);
    
    (* Calculate compression ratio *)
    let original_size = (List.length mono_samples) * 2 + 44 in (* 16-bit samples + WAV header *)
    let compression_ratio = float_of_int original_size /. float_of_int encoded_size in
    Printf.printf "\n📊 Statistics:\n";
    Printf.printf "   Original size (approx): %d bytes (%.2f KB)\n" original_size (float_of_int original_size /. 1024.0);
    Printf.printf "   Compressed size: %d bytes (%.2f KB)\n" encoded_size (float_of_int encoded_size /. 1024.0);
    Printf.printf "   Compression ratio: %.2fx\n" compression_ratio;
    Printf.printf "   Space saved: %.1f%%\n" ((1.0 -. 1.0 /. compression_ratio) *. 100.0);
    Printf.printf "   Total time: %.3f seconds\n\n" (encode_time +. decode_time);
  ) modes;
  
  (* Summary *)
  Printf.printf "╔════════════════════════════════════════════════════════════╗\n";
  Printf.printf "║                      ✅ COMPLETE!                          ║\n";
  Printf.printf "╚════════════════════════════════════════════════════════════╝\n\n";
  Printf.printf "📁 All files saved to: %s/\n\n" output_dir;
  Printf.printf "Generated files:\n";
  Printf.printf "  🔊 High Quality:\n";
  Printf.printf "     • %s/test_sample_high.audc (compressed)\n" output_dir;
  Printf.printf "     • %s/test_sample_high.wav (decoded)\n" output_dir;
  Printf.printf "  🔉 Medium Quality:\n";
  Printf.printf "     • %s/test_sample_medium.audc (compressed)\n" output_dir;
  Printf.printf "     • %s/test_sample_medium.wav (decoded)\n" output_dir;
  Printf.printf "  🔈 Low Quality:\n";
  Printf.printf "     • %s/test_sample_low.audc (compressed)\n" output_dir;
  Printf.printf "     • %s/test_sample_low.wav (decoded)\n" output_dir;
  Printf.printf "\n🎧 You can now listen to the decoded WAV files!\n";
  Printf.printf "   Compare them with the original: %s\n\n" input_file
