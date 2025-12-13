(* Profile full encode_to_file including serialization *)

module Encoder = Codec.Encoder

let time_it name f =
  let start = Unix.gettimeofday () in
  let result = f () in
  let elapsed = Unix.gettimeofday () -. start in
  Printf.printf "%-35s: %8.3f sec\n" name elapsed;
  result

let () =
  let num_samples = 15000 in
  let samples =
    List.init num_samples (fun i -> Float.sin (float_of_int i *. 0.1))
  in
  let sample_rate = 44100 in

  Printf.printf "\n╔═══════════════════════════════════════════════════════╗\n";
  Printf.printf "║   Full Encode Pipeline (%d samples)            ║\n"
    num_samples;
  Printf.printf "╚═══════════════════════════════════════════════════════╝\n\n";

  let temp_file = Filename.temp_file "profile_test" ".audc" in

  let total_start = Unix.gettimeofday () in

  let _audio_file =
    time_it "encode_to_file (with serialization)" (fun () ->
        Encoder.encode_to_file samples sample_rate temp_file)
  in

  let total_time = Unix.gettimeofday () -. total_start in

  let file_size = (Unix.stat temp_file).Unix.st_size in

  Printf.printf "───────────────────────────────────────────────────────\n";
  Printf.printf "TOTAL                              : %8.3f sec\n" total_time;
  Printf.printf "File size                          : %8d bytes\n" file_size;
  Printf.printf "\n";

  Sys.remove temp_file
