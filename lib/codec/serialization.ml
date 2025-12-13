(* AudioFile serialization/deserialization *)

module Writer = Io.Writer
module Reader = Io.Reader
module Quantization = Codec_utilities.Quantization

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

(* Serialize segment - O(n) using List.concat *)
let serialize_segment (seg : Audiomodel.segment) =
  let parts = [] in
  (* window_type *)
  let parts = serialize_window_type seg.Audiomodel.window_type :: parts in
  (* window_data: optional float list - DON'T SAVE! Can be reconstructed from window_type *)
  (* This saves HUGE amount of space - each window is 512-4096 floats (2-16 KB) *)
  let parts = Writer.write_uint8 0 :: parts in  (* Always write 0 - we don't save window_data *)
  (* start_sample: optional int *)
  let parts = (match seg.Audiomodel.start_sample with
  | None -> Writer.write_uint8 0
  | Some v -> Writer.write_uint8 1 @ Writer.write_int32 v
  ) :: parts in
  (* end_sample: optional int *)
  let parts = (match seg.Audiomodel.end_sample with
  | None -> Writer.write_uint8 0
  | Some v -> Writer.write_uint8 1 @ Writer.write_int32 v
  ) :: parts in
  (* attack_ms: optional float *)
  let parts = (match seg.Audiomodel.attack_ms with
  | None -> Writer.write_uint8 0
  | Some v -> Writer.write_uint8 1 @ Writer.write_float v
  ) :: parts in
  (* release_ms: optional float *)
  let parts = (match seg.Audiomodel.release_ms with
  | None -> Writer.write_uint8 0
  | Some v -> Writer.write_uint8 1 @ Writer.write_float v
  ) :: parts in
  (* original_length: optional int *)
  let parts = (match seg.Audiomodel.original_length with
  | None -> Writer.write_uint8 0
  | Some v -> Writer.write_uint8 1 @ Writer.write_int32 v
  ) :: parts in
  (* num_bands: int *)
  let parts = Writer.write_int32 seg.Audiomodel.num_bands :: parts in
  (* frequency_bands: (float * float) list *)
  let parts = Writer.write_int32 (List.length seg.Audiomodel.frequency_bands) :: parts in
  (* Build in reverse, then concat *)
  let freq_bands_arr = Array.of_list seg.Audiomodel.frequency_bands in
  let freq_bands_bytes = 
    let rev_parts = Array.fold_left (fun acc (f1, f2) ->
      (Writer.write_float f1 @ Writer.write_float f2) :: acc
    ) [] freq_bands_arr in
    List.concat (List.rev rev_parts)
  in
  let parts = freq_bands_bytes :: parts in
  (* band_ranges: (float * float) list *)
  let parts = Writer.write_int32 (List.length seg.Audiomodel.band_ranges) :: parts in
  (* Build in reverse, then concat *)
  let band_ranges_arr = Array.of_list seg.Audiomodel.band_ranges in
  let band_ranges_bytes = 
    let rev_parts = Array.fold_left (fun acc (f1, f2) ->
      (Writer.write_float f1 @ Writer.write_float f2) :: acc
    ) [] band_ranges_arr in
    List.concat (List.rev rev_parts)
  in
  let parts = band_ranges_bytes :: parts in
  (* quantization_levels: int list *)
  let parts = Writer.write_list_int32 seg.Audiomodel.quantization_levels :: parts in
  (* quantized_data: optional int list list - use bit packing! *)
  let parts = (match seg.Audiomodel.quantized_data with
  | None -> Writer.write_uint8 0
  | Some bands ->
    Writer.write_uint8 1 @ 
    Writer.write_int32 (List.length bands) @
    (* Use bit packing for each band with corresponding quantization level *)
    (* Collect all band bytes in correct order *)
    let bands_arr = Array.of_list bands in
    let levels_arr = Array.of_list seg.Audiomodel.quantization_levels in
    let quant_bytes = 
      let rec fold_bands i acc =
        if i >= Array.length bands_arr then List.rev acc
        else
          let band = bands_arr.(i) in
          let quant_level = levels_arr.(i) in
          let packed = Quantization.encode_quantized_indices band quant_level in
          let band_bytes = Writer.write_int32 (List.length band) @ Writer.write_list_uint8 packed in
          fold_bands (i + 1) (band_bytes :: acc)
      in
      List.concat (fold_bands 0 [])
    in
    quant_bytes
  ) :: parts in
  (* raw_data: optional float list list *)
  let parts = (match seg.Audiomodel.raw_data with
  | None -> Writer.write_uint8 0
  | Some bands ->
    Writer.write_uint8 1 @
    Writer.write_int32 (List.length bands) @
    (* Build in reverse, then concat *)
    let bands_arr = Array.of_list bands in
    let raw_bytes_rev = Array.fold_left (fun acc band ->
      Writer.write_list_float band :: acc
    ) [] bands_arr in
    List.concat (List.rev raw_bytes_rev)
  ) :: parts in
  (* Simple approach: reverse parts and concatenate *)
  (* parts is in reverse order (newest first), so List.rev gives correct order *)
  List.concat (List.rev parts)

(* Deserialize segment - array-based version for performance *)
let deserialize_segment_array (arr, idx) =
  let state = ref (arr, idx) in
  (* window_type - still needs list conversion *)
  let window_type = 
    let bytes_list = Reader.array_to_list !state in
    let wt, rem = deserialize_window_type bytes_list in
    state := Reader.list_to_array rem;
    wt
  in
  (* window_data - not saved anymore, reconstruct from window_type *)
  let _flag = let (v, s) = Reader.read_uint8_array !state in state := s; v in
  let window_data = None in
  
  (* start_sample *)
  let start_sample =
    let flag = let (v, s) = Reader.read_uint8_array !state in state := s; v in
    if flag = 0 then None
    else Some (let (v, s) = Reader.read_int32_array !state in state := s; v)
  in
  
  (* end_sample *)
  let end_sample =
    let flag = let (v, s) = Reader.read_uint8_array !state in state := s; v in
    if flag = 0 then None
    else Some (let (v, s) = Reader.read_int32_array !state in state := s; v)
  in
  
  (* attack_ms *)
  let attack_ms =
    let flag = let (v, s) = Reader.read_uint8_array !state in state := s; v in
    if flag = 0 then None
    else Some (let (v, s) = Reader.read_float_array !state in state := s; v)
  in
  
  (* release_ms *)
  let release_ms =
    let flag = let (v, s) = Reader.read_uint8_array !state in state := s; v in
    if flag = 0 then None
    else Some (let (v, s) = Reader.read_float_array !state in state := s; v)
  in
  
  (* original_length *)
  let original_length =
    let flag = let (v, s) = Reader.read_uint8_array !state in state := s; v in
    if flag = 0 then None
    else Some (let (v, s) = Reader.read_int32_array !state in state := s; v)
  in
  
  (* num_bands *)
  let num_bands = let (v, s) = Reader.read_int32_array !state in state := s; v in
  
  (* frequency_bands *)
  let freq_bands_len = let (v, s) = Reader.read_int32_array !state in state := s; v in
  let rec read_freq_bands n acc st =
    if n = 0 then (List.rev acc, st)
    else
      let f1, st = Reader.read_float_array st in
      let f2, st = Reader.read_float_array st in
      read_freq_bands (n - 1) ((f1, f2) :: acc) st
  in
  let frequency_bands, new_state = read_freq_bands freq_bands_len [] !state in
  state := new_state;
  
  (* band_ranges *)
  let band_ranges_len = let (v, s) = Reader.read_int32_array !state in state := s; v in
  let rec read_band_ranges n acc st =
    if n = 0 then (List.rev acc, st)
    else
      let f1, st = Reader.read_float_array st in
      let f2, st = Reader.read_float_array st in
      read_band_ranges (n - 1) ((f1, f2) :: acc) st
  in
  let band_ranges, new_state = read_band_ranges band_ranges_len [] !state in
  state := new_state;
  
  (* quantization_levels *)
  let quantization_levels = let (v, s) = Reader.read_list_int32_array !state in state := s; v in
  (* quantized_data - decode from bit-packed format *)
  let quantized_data =
    let flag = let (v, s) = Reader.read_uint8_array !state in state := s; v in
    if flag = 0 then None
    else
      let num_bands = let (v, s) = Reader.read_int32_array !state in state := s; v in
      (* Convert quantization_levels to array for O(1) access *)
      let levels_arr = Array.of_list quantization_levels in
      let rec read_bands n acc st =
        if n = 0 then (Some (List.rev acc), st)
        else
          (* Read number of elements *)
          let num_elements, st = Reader.read_int32_array st in
          (* Read packed bytes using array API *)
          let packed_bytes, st = Reader.read_list_uint8_array st in
          (* Get quantization level from array (O(1) instead of O(n)) *)
          let band_index = num_bands - n in
          (* Check that quantization_levels has enough elements and band_index is valid *)
          if band_index >= Array.length levels_arr then
            failwith (Printf.sprintf "Band index %d >= quantization_levels length %d" 
              band_index (Array.length levels_arr))
          else
            let quant_level = levels_arr.(band_index) in
            (* Check that quant_level is valid *)
            if quant_level <= 0 then
              failwith (Printf.sprintf "Invalid quantization level %d at index %d" quant_level band_index)
            else
              (* Decode using bit unpacking *)
              let band = Quantization.decode_quantized_indices packed_bytes num_elements quant_level in
              read_bands (n - 1) (band :: acc) st
      in
      let result, new_state = read_bands num_bands [] !state in
      state := new_state;
      result
  in
  
  (* raw_data *)
  let raw_data =
    let flag = let (v, s) = Reader.read_uint8_array !state in state := s; v in
    if flag = 0 then None
    else
      let num_bands = let (v, s) = Reader.read_int32_array !state in state := s; v in
      let rec read_bands n acc st =
        if n = 0 then (Some (List.rev acc), st)
        else
          let band, st = Reader.read_list_float_array st in
          read_bands (n - 1) (band :: acc) st
      in
      let result, new_state = read_bands num_bands [] !state in
      state := new_state;
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
  (segment, !state)

(* Serialize track *)
let serialize_track track =
  let parts = [] in
  (* num_segments *)
  let parts = Writer.write_int32 track.Audiomodel.num_segments :: parts in
  (* segments *)
  let parts = Writer.write_int32 (List.length track.Audiomodel.segments) :: parts in
  let parts = (List.concat (List.map serialize_segment track.Audiomodel.segments)) :: parts in
  (* length_samples *)
  let parts = Writer.write_int32 track.Audiomodel.length_samples :: parts in
  (* sample_rate *)
  let parts = Writer.write_int32 track.Audiomodel.sample_rate :: parts in
  (* compression_params: optional (float * float) *)
  let parts = (match track.Audiomodel.compression_params with
  | None -> Writer.write_uint8 0
  | Some (min_val, max_val) ->
    Writer.write_uint8 1 @ Writer.write_float min_val @ Writer.write_float max_val
  ) :: parts in
  (* Simple approach: reverse parts and concatenate *)
  (* parts is in reverse order (newest first), so List.rev gives correct order *)
  List.concat (List.rev parts)

(* Deserialize track - array-based version *)
let deserialize_track_array (arr, idx) =
  let state = ref (arr, idx) in
  (* num_segments *)
  let num_segments = let (v, s) = Reader.read_int32_array !state in state := s; v in
  (* segments *)
  let segments_len = let (v, s) = Reader.read_int32_array !state in state := s; v in
  let rec read_segments n acc st =
    if n = 0 then (List.rev acc, st)
    else
      let seg, st = deserialize_segment_array st in
      read_segments (n - 1) (seg :: acc) st
  in
  let segments, new_state = read_segments segments_len [] !state in
  state := new_state;
  (* length_samples *)
  let length_samples = let (v, s) = Reader.read_int32_array !state in state := s; v in
  (* sample_rate *)
  let sample_rate = let (v, s) = Reader.read_int32_array !state in state := s; v in
  (* compression_params *)
  let compression_params =
    let flag = let (v, s) = Reader.read_uint8_array !state in state := s; v in
    if flag = 0 then None
    else
      let min_val, st = Reader.read_float_array !state in
      let max_val, st = Reader.read_float_array st in
      state := st;
      Some (min_val, max_val)
  in
  let track = {
    Audiomodel.num_segments = num_segments;
    Audiomodel.segments = segments;
    Audiomodel.length_samples = length_samples;
    Audiomodel.sample_rate = sample_rate;
    Audiomodel.compression_params = compression_params;
  } in
  (track, !state)

(* Serialize AudioFile *)
let serialize_audio_file audio_file =
  let parts = [] in
  (* Write magic and version *)
  let parts = Writer.write_file_header "AUDC" 1 :: parts in
  (* num_tracks *)
  let parts = Writer.write_int32 audio_file.Audiomodel.num_tracks :: parts in
  (* tracks *)
  let parts = Writer.write_int32 (List.length audio_file.Audiomodel.tracks) :: parts in
  let parts = (List.concat (List.map serialize_track audio_file.Audiomodel.tracks)) :: parts in
  (* bits_per_sample *)
  let parts = Writer.write_int32 audio_file.Audiomodel.bits_per_sample :: parts in
  (* sample_rate *)
  let parts = Writer.write_int32 audio_file.Audiomodel.sample_rate :: parts in
  (* Simple approach: reverse parts and concatenate *)
  (* parts is in reverse order (newest first), so List.rev gives correct order *)
  List.concat (List.rev parts)

(* Deserialize AudioFile - optimized with arrays *)
let deserialize_audio_file bytes =
  (* Convert to array once at the start *)
  let state = ref (Reader.list_to_array bytes) in
  (* Read magic and version - still needs list for header *)
  let bytes_list = Reader.array_to_list !state in
  let magic, version, rem = Reader.read_file_header bytes_list in
  if magic <> "AUDC" then
    failwith ("Invalid magic: expected AUDC, got " ^ magic);
  if version <> 1 then
    failwith ("Unsupported version: " ^ string_of_int version);
  state := Reader.list_to_array rem;
  
  (* num_tracks *)
  let num_tracks = let (v, s) = Reader.read_int32_array !state in state := s; v in
  (* tracks *)
  let tracks_len = let (v, s) = Reader.read_int32_array !state in state := s; v in
  let rec read_tracks n acc st =
    if n = 0 then (List.rev acc, st)
    else
      let track, st = deserialize_track_array st in
      read_tracks (n - 1) (track :: acc) st
  in
  let tracks, new_state = read_tracks tracks_len [] !state in
  state := new_state;
  (* bits_per_sample *)
  let bits_per_sample = let (v, s) = Reader.read_int32_array !state in state := s; v in
  (* sample_rate *)
  let sample_rate = let (v, s) = Reader.read_int32_array !state in state := s; v in
  
  let audio_file = {
    Audiomodel.num_tracks = num_tracks;
    Audiomodel.tracks = tracks;
    Audiomodel.bits_per_sample = bits_per_sample;
    Audiomodel.sample_rate = sample_rate;
  } in
  (audio_file, Reader.array_to_list !state)

(* Serialize AudioFile to file *)
let serialize_to_file filename audio_file =
  let bytes = serialize_audio_file audio_file in
  Writer.write_to_file filename bytes

(* Deserialize AudioFile from file *)
let deserialize_from_file filename =
  let bytes = Reader.read_bytes_from_file filename in
  let audio_file, _ = deserialize_audio_file bytes in
  audio_file

