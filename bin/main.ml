module Args = Cli.Args
module Parser = Wav.Parser
module Writer = Wav.Writer
module Encoder = Codec.Encoder
module Decoder = Codec.Decoder

let () =
  try
    let args = Args.parse_args () in

    match args.mode with
    | Args.Encode ->
        Printf.printf "Encoding %s to %s...\n" args.input_file args.output_file;
        Printf.printf "Quality: %.1f dB SNR\n" args.snr_threshold_db;
        flush stdout;

        let wav_data = Parser.read_wav args.input_file in
        Printf.printf "Input: %d Hz, %d channels, %d bits/sample, %d samples\n"
          wav_data.info.sample_rate wav_data.info.num_channels
          wav_data.info.bits_per_sample wav_data.info.num_samples;
        flush stdout;

        let _audio_file =
          Encoder.encode_to_file ~snr_threshold_db:args.snr_threshold_db
            wav_data.samples wav_data.info.sample_rate args.output_file
        in

        Printf.printf "Successfully encoded to %s\n" args.output_file;
        flush stdout
    | Args.Decode ->
        Printf.printf "Decoding %s to %s...\n" args.input_file args.output_file;
        flush stdout;

        let samples, sample_rate = Decoder.decode_from_file args.input_file in
        Printf.printf "Decoded: %d Hz, %d samples\n" sample_rate
          (List.length samples);
        flush stdout;

        let wav_info =
          { Writer.sample_rate; num_channels = 1; bits_per_sample = 16 }
        in

        Writer.write_wav args.output_file wav_info samples;

        Printf.printf "Successfully decoded to %s\n" args.output_file;
        flush stdout
  with
  | Args.Invalid_args msg ->
      Printf.eprintf "Error: %s\n" msg;
      exit 1
  | Parser.Invalid_wav_file msg ->
      Printf.eprintf "WAV file error: %s\n" msg;
      exit 1
  | e ->
      Printf.eprintf "Unexpected error: %s\n" (Printexc.to_string e);
      Printexc.print_backtrace stderr;
      exit 1
