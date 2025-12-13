(* WAV file parser *)

type wav_info = {
  sample_rate : int;
  num_channels : int;
  bits_per_sample : int;
  num_samples : int;
}

type wav_data = {
  info : wav_info;
  samples : float list;
}

exception Invalid_wav_file of string

let read_int32_le ic =
  let b0 = input_byte ic in
  let b1 = input_byte ic in
  let b2 = input_byte ic in
  let b3 = input_byte ic in
  (b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24))

let read_int16_le ic =
  let b0 = input_byte ic in
  let b1 = input_byte ic in
  let low = b0 in
  let high = b1 in
  let value = low lor (high lsl 8) in
  if high land 0x80 <> 0 then value - 0x10000 else value

let read_uint16_le ic =
  let b0 = input_byte ic in
  let b1 = input_byte ic in
  b0 lor (b1 lsl 8)

let read_string ic len =
  let buf = Bytes.create len in
  really_input ic buf 0 len;
  Bytes.to_string buf

let read_samples_16bit ic num_samples =
  let rec aux acc remaining =
    if remaining = 0 then List.rev acc
    else
      let sample = read_int16_le ic in
      let normalized = float_of_int sample /. 32768.0 in
      aux (normalized :: acc) (remaining - 1)
  in
  aux [] num_samples

let read_samples_24bit ic num_samples =
  let rec aux acc remaining =
    if remaining = 0 then List.rev acc
    else
      let b0 = input_byte ic in
      let b1 = input_byte ic in
      let b2 = input_byte ic in
      let sample = b0 lor (b1 lsl 8) lor (b2 lsl 16) in
      let sample = if b2 land 0x80 <> 0 then sample - 0x1000000 else sample in
      let normalized = float_of_int sample /. 8388608.0 in
      aux (normalized :: acc) (remaining - 1)
  in
  aux [] num_samples

let read_samples_32bit ic num_samples =
  let rec aux acc remaining =
    if remaining = 0 then List.rev acc
    else
      let bits = read_int32_le ic in
      (* Convert unsigned 32-bit to signed 32-bit *)
      let sample = if bits land 0x80000000 <> 0 then bits - 0x100000000 else bits in
      let normalized = float_of_int sample /. 2147483648.0 in
      aux (normalized :: acc) (remaining - 1)
  in
  aux [] num_samples

let read_samples_float32 ic num_samples =
  let rec aux acc remaining =
    if remaining = 0 then List.rev acc
    else
      let bits = read_int32_le ic in
      let sign = if bits land 0x80000000 <> 0 then -1.0 else 1.0 in
      let exponent = ((bits lsr 23) land 0xFF) - 127 in
      let mantissa = float_of_int (bits land 0x7FFFFF) /. 8388608.0 +. 1.0 in
      let sample = sign *. mantissa *. (2.0 ** float_of_int exponent) in
      aux (sample :: acc) (remaining - 1)
  in
  aux [] num_samples

let find_chunk ic chunk_id =
  let rec search pos =
    if pos >= in_channel_length ic then None
    else (
      seek_in ic pos;
      let id = read_string ic 4 in
      if id = chunk_id then (
        let size = read_int32_le ic in
        Some (pos + 8, size)
      ) else (
        let size = read_int32_le ic in
        let next_pos = pos + 8 + size in
        if size mod 2 <> 0 then search (next_pos + 1) else search next_pos
      )
    )
  in
  search 12

let read_wav_info filename =
  let ic = open_in_bin filename in
  try
    (* Read RIFF header *)
    let riff = read_string ic 4 in
    if riff <> "RIFF" then (
      close_in ic;
      raise (Invalid_wav_file "Not a RIFF file")
    );
    ignore (read_int32_le ic); (* file size *)
    let wave = read_string ic 4 in
    if wave <> "WAVE" then (
      close_in ic;
      raise (Invalid_wav_file "Not a WAVE file")
    );

    (* Find and read fmt chunk *)
    let fmt_pos_size = find_chunk ic "fmt " in
    match fmt_pos_size with
    | None ->
        close_in ic;
        raise (Invalid_wav_file "fmt chunk not found")
    | Some (pos, _size) ->
        seek_in ic pos;
        let audio_format = read_uint16_le ic in
        if audio_format <> 1 && audio_format <> 3 then (
          close_in ic;
          raise (Invalid_wav_file ("Unsupported audio format: " ^ string_of_int audio_format))
        );
        let num_channels = read_uint16_le ic in
        let sample_rate = read_int32_le ic in
        ignore (read_int32_le ic); (* byte rate *)
        ignore (read_uint16_le ic); (* block align *)
        let bits_per_sample = read_uint16_le ic in

        (* Find data chunk to get num_samples *)
        let num_samples =
          match find_chunk ic "data" with
          | None -> 0
          | Some (_, data_size) ->
              let bytes_per_sample = bits_per_sample / 8 in
              data_size / bytes_per_sample / num_channels
        in

        close_in ic;
        { sample_rate; num_channels; bits_per_sample; num_samples }
  with
  | End_of_file ->
      close_in ic;
      raise (Invalid_wav_file "Unexpected end of file")
  | e ->
      close_in ic;
      raise e

let read_wav filename =
  let ic = open_in_bin filename in
  try
    (* Read RIFF header *)
    let riff = read_string ic 4 in
    if riff <> "RIFF" then (
      close_in ic;
      raise (Invalid_wav_file "Not a RIFF file")
    );
    ignore (read_int32_le ic); (* file size *)
    let wave = read_string ic 4 in
    if wave <> "WAVE" then (
      close_in ic;
      raise (Invalid_wav_file "Not a WAVE file")
    );

    (* Find and read fmt chunk *)
    let fmt_pos_size = find_chunk ic "fmt " in
    let fmt_pos =
      match fmt_pos_size with
      | None ->
          close_in ic;
          raise (Invalid_wav_file "fmt chunk not found")
      | Some (pos, _size) -> pos
    in

    seek_in ic fmt_pos;
    let audio_format = read_uint16_le ic in
    if audio_format <> 1 && audio_format <> 3 then (
      close_in ic;
      raise (Invalid_wav_file ("Unsupported audio format: " ^ string_of_int audio_format))
    );
    let num_channels = read_uint16_le ic in
    let sample_rate = read_int32_le ic in
    ignore (read_int32_le ic); (* byte rate *)
    ignore (read_uint16_le ic); (* block align *)
    let bits_per_sample = read_uint16_le ic in

    (* Find and read data chunk *)
    let data_pos_size = find_chunk ic "data" in
    let data_pos, data_size =
      match data_pos_size with
      | None ->
          close_in ic;
          raise (Invalid_wav_file "data chunk not found")
      | Some (pos, size) -> (pos, size)
    in

    seek_in ic data_pos;
    let bytes_per_sample = bits_per_sample / 8 in
    let num_samples = data_size / bytes_per_sample / num_channels in

    (* Read samples based on format *)
    let samples =
      match (audio_format, bits_per_sample) with
      | 1, 16 -> read_samples_16bit ic (num_samples * num_channels)
      | 1, 24 -> read_samples_24bit ic (num_samples * num_channels)
      | 1, 32 -> read_samples_32bit ic (num_samples * num_channels)
      | 3, 32 -> read_samples_float32 ic (num_samples * num_channels)
      | _ ->
          close_in ic;
          raise
            (Invalid_wav_file
               ("Unsupported format: audio_format=" ^ string_of_int audio_format
              ^ ", bits_per_sample=" ^ string_of_int bits_per_sample))
    in

    close_in ic;
    {
      info =
        { sample_rate; num_channels; bits_per_sample; num_samples };
      samples;
    }
  with
  | End_of_file ->
      close_in ic;
      raise (Invalid_wav_file "Unexpected end of file")
  | e ->
      close_in ic;
      raise e
