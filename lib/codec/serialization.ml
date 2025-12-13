(* AudioFile serialization/deserialization *)

module Writer = Io.Writer
module Reader = Io.Reader

(* Note: We're inside the codec library, so we can access Audiomodel directly *)
(* without going through Codec wrapper *)

(* Serialize window_type *)
let serialize_window_type wt =
  let code = match wt with
  | Audiomodel.Short -> 0
  | Audiomodel.Long -> 1
  | Audiomodel.VeryLong -> 2
  | Audiomodel.EndFocused -> 3
  | Audiomodel.StartFocused -> 4
  in
  Writer.write_uint8 code

(* Deserialize window_type *)
let deserialize_window_type bytes =
  let code, remaining = Reader.read_uint8 bytes in
  let wt = match code with
  | 0 -> Audiomodel.Short
  | 1 -> Audiomodel.Long
  | 2 -> Audiomodel.VeryLong
  | 3 -> Audiomodel.EndFocused
  | 4 -> Audiomodel.StartFocused
  | _ -> failwith ("Invalid window_type code: " ^ string_of_int code)
  in
  (wt, remaining)

(* Serialize segment *)
let serialize_segment (seg : Audiomodel.segment) =
  let bytes = [] in
  (* window_type *)
  let bytes = bytes @ serialize_window_type seg.Audiomodel.window_type in
  (* window_data: optional float list *)
  let bytes = match seg.Audiomodel.window_data with
  | None -> bytes @ Writer.write_uint8 0
  | Some data -> bytes @ Writer.write_uint8 1 @ Writer.write_list_float data
  in
  (* start_sample: optional int *)
  let bytes = match seg.Audiomodel.start_sample with
  | None -> bytes @ Writer.write_uint8 0
  | Some v -> bytes @ Writer.write_uint8 1 @ Writer.write_int32 v
  in
  (* end_sample: optional int *)
  let bytes = match seg.Audiomodel.end_sample with
  | None -> bytes @ Writer.write_uint8 0
  | Some v -> bytes @ Writer.write_uint8 1 @ Writer.write_int32 v
  in
  (* attack_ms: optional float *)
  let bytes = match seg.Audiomodel.attack_ms with
  | None -> bytes @ Writer.write_uint8 0
  | Some v -> bytes @ Writer.write_uint8 1 @ Writer.write_float v
  in
  (* release_ms: optional float *)
  let bytes = match seg.Audiomodel.release_ms with
  | None -> bytes @ Writer.write_uint8 0
  | Some v -> bytes @ Writer.write_uint8 1 @ Writer.write_float v
  in
  (* original_length: optional int *)
  let bytes = match seg.Audiomodel.original_length with
  | None -> bytes @ Writer.write_uint8 0
  | Some v -> bytes @ Writer.write_uint8 1 @ Writer.write_int32 v
  in
  (* num_bands: int *)
  let bytes = bytes @ Writer.write_int32 seg.Audiomodel.num_bands in
  (* frequency_bands: (float * float) list *)
  let bytes = bytes @ Writer.write_int32 (List.length seg.Audiomodel.frequency_bands) in
  let bytes = List.fold_left (fun acc (f1, f2) ->
    acc @ Writer.write_float f1 @ Writer.write_float f2
  ) bytes seg.Audiomodel.frequency_bands in
  (* band_ranges: (float * float) list *)
  let bytes = bytes @ Writer.write_int32 (List.length seg.Audiomodel.band_ranges) in
  let bytes = List.fold_left (fun acc (f1, f2) ->
    acc @ Writer.write_float f1 @ Writer.write_float f2
  ) bytes seg.Audiomodel.band_ranges in
  (* quantization_levels: int list *)
  let bytes = bytes @ Writer.write_list_int32 seg.Audiomodel.quantization_levels in
  (* quantized_data: optional int list list *)
  let bytes = match seg.Audiomodel.quantized_data with
  | None -> bytes @ Writer.write_uint8 0
  | Some bands ->
    bytes @ Writer.write_uint8 1 @ 
    Writer.write_int32 (List.length bands) @
    List.concat (List.map (fun band -> Writer.write_list_int32 band) bands)
  in
  (* raw_data: optional float list list *)
  let bytes = match seg.Audiomodel.raw_data with
  | None -> bytes @ Writer.write_uint8 0
  | Some bands ->
    bytes @ Writer.write_uint8 1 @
    Writer.write_int32 (List.length bands) @
    List.concat (List.map (fun band -> Writer.write_list_float band) bands)
  in
  bytes

(* Deserialize segment *)
let deserialize_segment bytes =
  let bytes = ref bytes in
  (* window_type *)
  let window_type, rem = deserialize_window_type !bytes in
  bytes := rem;
  (* window_data *)
  let window_data = 
    let flag, rem = Reader.read_uint8 !bytes in
    bytes := rem;
    if flag = 0 then None
    else
      let data, rem = Reader.read_list_float !bytes in
      bytes := rem;
      Some data
  in
  (* start_sample *)
  let start_sample =
    let flag, rem = Reader.read_uint8 !bytes in
    bytes := rem;
    if flag = 0 then None
    else
      let v, rem = Reader.read_int32 !bytes in
      bytes := rem;
      Some v
  in
  (* end_sample *)
  let end_sample =
    let flag, rem = Reader.read_uint8 !bytes in
    bytes := rem;
    if flag = 0 then None
    else
      let v, rem = Reader.read_int32 !bytes in
      bytes := rem;
      Some v
  in
  (* attack_ms *)
  let attack_ms =
    let flag, rem = Reader.read_uint8 !bytes in
    bytes := rem;
    if flag = 0 then None
    else
      let v, rem = Reader.read_float !bytes in
      bytes := rem;
      Some v
  in
  (* release_ms *)
  let release_ms =
    let flag, rem = Reader.read_uint8 !bytes in
    bytes := rem;
    if flag = 0 then None
    else
      let v, rem = Reader.read_float !bytes in
      bytes := rem;
      Some v
  in
  (* original_length *)
  let original_length =
    let flag, rem = Reader.read_uint8 !bytes in
    bytes := rem;
    if flag = 0 then None
    else
      let v, rem = Reader.read_int32 !bytes in
      bytes := rem;
      Some v
  in
  (* num_bands *)
  let num_bands, rem = Reader.read_int32 !bytes in
  bytes := rem;
  (* frequency_bands *)
  let freq_bands_len, rem = Reader.read_int32 !bytes in
  bytes := rem;
  let rec read_freq_bands n acc rem =
    if n = 0 then (List.rev acc, rem)
    else
      let f1, rem = Reader.read_float rem in
      let f2, rem = Reader.read_float rem in
      read_freq_bands (n - 1) ((f1, f2) :: acc) rem
  in
  let frequency_bands, rem = read_freq_bands freq_bands_len [] !bytes in
  bytes := rem;
  (* band_ranges *)
  let band_ranges_len, rem = Reader.read_int32 !bytes in
  bytes := rem;
  let rec read_band_ranges n acc rem =
    if n = 0 then (List.rev acc, rem)
    else
      let f1, rem = Reader.read_float rem in
      let f2, rem = Reader.read_float rem in
      read_band_ranges (n - 1) ((f1, f2) :: acc) rem
  in
  let band_ranges, rem = read_band_ranges band_ranges_len [] !bytes in
  bytes := rem;
  (* quantization_levels *)
  let quantization_levels, rem = Reader.read_list_int32 !bytes in
  bytes := rem;
  (* quantized_data *)
  let quantized_data =
    let flag, rem = Reader.read_uint8 !bytes in
    bytes := rem;
    if flag = 0 then None
    else
      let num_bands, rem = Reader.read_int32 !bytes in
      bytes := rem;
      let rec read_bands n acc rem =
        if n = 0 then (Some (List.rev acc), rem)
        else
          let band, rem = Reader.read_list_int32 rem in
          read_bands (n - 1) (band :: acc) rem
      in
      let result, rem = read_bands num_bands [] !bytes in
      bytes := rem;
      result
  in
  (* raw_data *)
  let raw_data =
    let flag, rem = Reader.read_uint8 !bytes in
    bytes := rem;
    if flag = 0 then None
    else
      let num_bands, rem = Reader.read_int32 !bytes in
      bytes := rem;
      let rec read_bands n acc rem =
        if n = 0 then (Some (List.rev acc), rem)
        else
          let band, rem = Reader.read_list_float rem in
          read_bands (n - 1) (band :: acc) rem
      in
      let result, rem = read_bands num_bands [] !bytes in
      bytes := rem;
      result
  in
  let segment = {
    Audiomodel.window_type = window_type;
    Audiomodel.window_data = window_data;
    Audiomodel.start_sample = start_sample;
    Audiomodel.end_sample = end_sample;
    Audiomodel.attack_ms = attack_ms;
    Audiomodel.release_ms = release_ms;
    Audiomodel.original_length = original_length;
    Audiomodel.num_bands = num_bands;
    Audiomodel.frequency_bands = frequency_bands;
    Audiomodel.band_ranges = band_ranges;
    Audiomodel.quantization_levels = quantization_levels;
    Audiomodel.quantized_data = quantized_data;
    Audiomodel.raw_data = raw_data;
  } in
  (segment, !bytes)

(* Serialize track *)
let serialize_track track =
  let bytes = [] in
  (* num_segments *)
  let bytes = bytes @ Writer.write_int32 track.Audiomodel.num_segments in
  (* segments *)
  let bytes = bytes @ Writer.write_int32 (List.length track.Audiomodel.segments) in
  let bytes = List.fold_left (fun acc seg ->
    acc @ serialize_segment seg
  ) bytes track.Audiomodel.segments in
  (* length_samples *)
  let bytes = bytes @ Writer.write_int32 track.Audiomodel.length_samples in
  (* sample_rate *)
  let bytes = bytes @ Writer.write_int32 track.Audiomodel.sample_rate in
  (* compression_params: optional (float * float) *)
  let bytes = match track.Audiomodel.compression_params with
  | None -> bytes @ Writer.write_uint8 0
  | Some (min_val, max_val) ->
    bytes @ Writer.write_uint8 1 @ Writer.write_float min_val @ Writer.write_float max_val
  in
  bytes

(* Deserialize track *)
let deserialize_track bytes =
  let bytes = ref bytes in
  (* num_segments *)
  let num_segments, rem = Reader.read_int32 !bytes in
  bytes := rem;
  (* segments *)
  let segments_len, rem = Reader.read_int32 !bytes in
  bytes := rem;
  let rec read_segments n acc rem =
    if n = 0 then (List.rev acc, rem)
    else
      let seg, rem = deserialize_segment rem in
      read_segments (n - 1) (seg :: acc) rem
  in
  let segments, rem = read_segments segments_len [] !bytes in
  bytes := rem;
  (* length_samples *)
  let length_samples, rem = Reader.read_int32 !bytes in
  bytes := rem;
  (* sample_rate *)
  let sample_rate, rem = Reader.read_int32 !bytes in
  bytes := rem;
  (* compression_params *)
  let compression_params =
    let flag, rem = Reader.read_uint8 !bytes in
    bytes := rem;
    if flag = 0 then None
    else
      let min_val, rem = Reader.read_float rem in
      let max_val, rem = Reader.read_float rem in
      bytes := rem;
      Some (min_val, max_val)
  in
  let track = {
    Audiomodel.num_segments = num_segments;
    Audiomodel.segments = segments;
    Audiomodel.length_samples = length_samples;
    Audiomodel.sample_rate = sample_rate;
    Audiomodel.compression_params = compression_params;
  } in
  (track, !bytes)

(* Serialize AudioFile *)
let serialize_audio_file audio_file =
  let bytes = [] in
  (* Write magic and version *)
  let bytes = bytes @ Writer.write_file_header "AUDC" 1 in
  (* num_tracks *)
  let bytes = bytes @ Writer.write_int32 audio_file.Audiomodel.num_tracks in
  (* tracks *)
  let bytes = bytes @ Writer.write_int32 (List.length audio_file.Audiomodel.tracks) in
  let bytes = List.fold_left (fun acc track ->
    acc @ serialize_track track
  ) bytes audio_file.Audiomodel.tracks in
  (* bits_per_sample *)
  let bytes = bytes @ Writer.write_int32 audio_file.Audiomodel.bits_per_sample in
  (* sample_rate *)
  let bytes = bytes @ Writer.write_int32 audio_file.Audiomodel.sample_rate in
  bytes

(* Deserialize AudioFile *)
let deserialize_audio_file bytes =
  let bytes = ref bytes in
  (* Read magic and version *)
  let magic, version, rem = Reader.read_file_header !bytes in
  if magic <> "AUDC" then
    failwith ("Invalid magic: expected AUDC, got " ^ magic);
  if version <> 1 then
    failwith ("Unsupported version: " ^ string_of_int version);
  bytes := rem;
  (* num_tracks *)
  let num_tracks, rem = Reader.read_int32 !bytes in
  bytes := rem;
  (* tracks *)
  let tracks_len, rem = Reader.read_int32 !bytes in
  bytes := rem;
  let rec read_tracks n acc rem =
    if n = 0 then (List.rev acc, rem)
    else
      let track, rem = deserialize_track rem in
      read_tracks (n - 1) (track :: acc) rem
  in
  let tracks, rem = read_tracks tracks_len [] !bytes in
  bytes := rem;
  (* bits_per_sample *)
  let bits_per_sample, rem = Reader.read_int32 !bytes in
  bytes := rem;
  (* sample_rate *)
  let sample_rate, rem = Reader.read_int32 !bytes in
  bytes := rem;
  let audio_file = {
    Audiomodel.num_tracks = num_tracks;
    Audiomodel.tracks = tracks;
    Audiomodel.bits_per_sample = bits_per_sample;
    Audiomodel.sample_rate = sample_rate;
  } in
  (audio_file, rem)

(* Serialize AudioFile to file *)
let serialize_to_file filename audio_file =
  let bytes = serialize_audio_file audio_file in
  Writer.write_to_file filename bytes

(* Deserialize AudioFile from file *)
let deserialize_from_file filename =
  let bytes = Reader.read_bytes_from_file filename in
  let audio_file, _ = deserialize_audio_file bytes in
  audio_file

