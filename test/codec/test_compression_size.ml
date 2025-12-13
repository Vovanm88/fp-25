(* Test compression size *)

module Encoder = Codec.Encoder
module Decoder = Codec.Decoder
module Wav = Wav

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
  let input_file = "test/wav/test_sample.wav" in
  let wav_data = Wav.Parser.read_wav input_file in
  let sample_rate = wav_data.Wav.Parser.info.Wav.Parser.sample_rate in
  let num_channels = wav_data.Wav.Parser.info.Wav.Parser.num_channels in
  let mono_samples = extract_mono wav_data.Wav.Parser.samples num_channels in
  
  Printf.printf "Original WAV:\n";
  Printf.printf "  Samples: %d\n" (List.length mono_samples);
  Printf.printf "  Size (approx): %d bytes (%.2f KB)\n" 
    ((List.length mono_samples) * 2 + 44) 
    (float_of_int ((List.length mono_samples) * 2 + 44) /. 1024.0);
  
  let temp_file = Filename.temp_file "test_size" ".audc" in
  
  Printf.printf "\nEncoding...\n";
  let _audio_file = Encoder.encode_to_file mono_samples sample_rate temp_file in
  
  let encoded_size = (Unix.stat temp_file).Unix.st_size in
  Printf.printf "\nEncoded:\n";
  Printf.printf "  Size: %d bytes (%.2f KB)\n" encoded_size (float_of_int encoded_size /. 1024.0);
  Printf.printf "  Ratio: %.2fx\n" (float_of_int encoded_size /. float_of_int ((List.length mono_samples) * 2 + 44));
  
  Printf.printf "\nDecoding...\n";
  let (reconstructed, _) = Decoder.decode_from_file temp_file in
  Printf.printf "  Reconstructed samples: %d\n" (List.length reconstructed);
  
  Sys.remove temp_file
