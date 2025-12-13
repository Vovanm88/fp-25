(* WAV file writer *)

type wav_info = {
  sample_rate : int;
  num_channels : int;
  bits_per_sample : int;
}

let write_int32_le oc value =
  output_byte oc (value land 0xFF);
  output_byte oc ((value lsr 8) land 0xFF);
  output_byte oc ((value lsr 16) land 0xFF);
  output_byte oc ((value lsr 24) land 0xFF)

let write_int16_le oc value =
  let value = if value < 0 then value + 0x10000 else value in
  output_byte oc (value land 0xFF);
  output_byte oc ((value lsr 8) land 0xFF)

let write_uint16_le oc value =
  output_byte oc (value land 0xFF);
  output_byte oc ((value lsr 8) land 0xFF)

let write_string oc s =
  output_string oc s

let clamp_sample sample =
  if sample < -1.0 then -1.0 else if sample > 1.0 then 1.0 else sample

let write_sample_16bit oc sample =
  let clamped = clamp_sample sample in
  let int_sample = int_of_float (clamped *. 32767.0) in
  write_int16_le oc int_sample

let write_sample_24bit oc sample =
  let clamped = clamp_sample sample in
  let int_sample = int_of_float (clamped *. 8388607.0) in
  let value = if int_sample < 0 then int_sample + 0x1000000 else int_sample in
  output_byte oc (value land 0xFF);
  output_byte oc ((value lsr 8) land 0xFF);
  output_byte oc ((value lsr 16) land 0xFF)

let write_sample_32bit oc sample =
  let clamped = clamp_sample sample in
  let int_sample = int_of_float (clamped *. 2147483647.0) in
  write_int32_le oc int_sample

let write_wav filename info samples =
  let oc = open_out_bin filename in
  try
    let num_samples = List.length samples / info.num_channels in
    let bytes_per_sample = info.bits_per_sample / 8 in
    let data_size = num_samples * info.num_channels * bytes_per_sample in
    let fmt_size = 16 in
    let file_size = 4 + 8 + fmt_size + 8 + data_size in

    (* Write RIFF header *)
    write_string oc "RIFF";
    write_int32_le oc file_size;
    write_string oc "WAVE";

    (* Write fmt chunk *)
    write_string oc "fmt ";
    write_int32_le oc fmt_size;
    (* Format 1 = PCM, Format 3 = IEEE float
       For now, we use PCM (format 1) for all formats including 32-bit.
       Float32 would require format 3, but that's a separate feature. *)
    let audio_format = 1 in
    write_uint16_le oc audio_format;
    write_uint16_le oc info.num_channels;
    write_int32_le oc info.sample_rate;
    let byte_rate = info.sample_rate * info.num_channels * bytes_per_sample in
    write_int32_le oc byte_rate;
    let block_align = info.num_channels * bytes_per_sample in
    write_uint16_le oc block_align;
    write_uint16_le oc info.bits_per_sample;

    (* Write data chunk *)
    write_string oc "data";
    write_int32_le oc data_size;

    (* Write samples *)
    let write_sample =
      match info.bits_per_sample with
      | 16 -> write_sample_16bit
      | 24 -> write_sample_24bit
      | 32 -> write_sample_32bit
      | _ -> write_sample_16bit
    in
    List.iter (write_sample oc) samples;

    close_out oc
  with
  | e ->
      close_out oc;
      raise e
