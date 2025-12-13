(* File writer - low-level binary serialization functions *)

(* Write int32 as little-endian bytes *)
let write_int32 value =
  [
    value land 0xFF;
    (value lsr 8) land 0xFF;
    (value lsr 16) land 0xFF;
    (value lsr 24) land 0xFF;
  ]

(* Write int16 as little-endian bytes *)
let write_int16 value =
  let value = if value < 0 then value + 0x10000 else value in
  [ value land 0xFF; (value lsr 8) land 0xFF ]

(* Write uint8 as single byte *)
let write_uint8 value =
  if value < 0 || value > 255 then
    failwith ("Invalid uint8 value: " ^ string_of_int value)
  else [ value ]

(* Write float as IEEE 754 single precision (32-bit) *)
let write_float value =
  (* Convert float to 32-bit IEEE 754 representation *)
  let bits = Int32.bits_of_float value in
  let b0 = Int32.to_int (Int32.logand bits 0xFFl) in
  let b1 =
    Int32.to_int (Int32.logand (Int32.shift_right_logical bits 8) 0xFFl)
  in
  let b2 =
    Int32.to_int (Int32.logand (Int32.shift_right_logical bits 16) 0xFFl)
  in
  let b3 =
    Int32.to_int (Int32.logand (Int32.shift_right_logical bits 24) 0xFFl)
  in
  [ b0; b1; b2; b3 ]

(* Write string as length (int32) + bytes *)
let write_string s =
  let len = String.length s in
  let bytes = List.init len (fun i -> Char.code (String.get s i)) in
  write_int32 len @ bytes

(* Write list of int32 - optimized: build in reverse, then reverse once *)
let write_list_int32 lst =
  let len = List.length lst in
  let len_bytes = write_int32 len in
  let arr = Array.of_list lst in
  (* Build bytes in correct order using fold_right *)
  let data_bytes =
    Array.fold_right
      (fun value acc ->
        let bytes = write_int32 value in
        bytes @ acc)
      arr []
  in
  len_bytes @ data_bytes

(* Write list of float - optimized: build in reverse, then reverse once *)
let write_list_float lst =
  let len = List.length lst in
  let len_bytes = write_int32 len in
  let arr = Array.of_list lst in
  (* Build bytes in correct order using fold_right *)
  let data_bytes =
    Array.fold_right
      (fun value acc ->
        let bytes = write_float value in
        bytes @ acc)
      arr []
  in
  len_bytes @ data_bytes

(* Write list of uint8 - optimized: direct conversion *)
let write_list_uint8 lst =
  let len = List.length lst in
  let len_bytes = write_int32 len in
  (* uint8 is just one byte, so we can convert directly *)
  len_bytes @ lst

(* Write bytes to file - optimized with Array *)
let write_bytes_to_file filename bytes =
  let oc = open_out_bin filename in
  try
    let arr = Array.of_list bytes in
    Array.iter (output_byte oc) arr;
    close_out oc
  with e ->
    close_out oc;
    raise e

(* Write int32 to file *)
let write_int32_to_file filename value =
  write_bytes_to_file filename (write_int32 value)

(* Write int16 to file *)
let write_int16_to_file filename value =
  write_bytes_to_file filename (write_int16 value)

(* Write uint8 to file *)
let write_uint8_to_file filename value =
  write_bytes_to_file filename (write_uint8 value)

(* Write float to file *)
let write_float_to_file filename value =
  write_bytes_to_file filename (write_float value)

(* Write string to file *)
let write_string_to_file filename value =
  write_bytes_to_file filename (write_string value)

(* Write list of int32 to file *)
let write_list_int32_to_file filename lst =
  write_bytes_to_file filename (write_list_int32 lst)

(* Write list of float to file *)
let write_list_float_to_file filename lst =
  write_bytes_to_file filename (write_list_float lst)

(* Write arbitrary bytes list to file *)
let write_to_file filename bytes = write_bytes_to_file filename bytes

(* Chunk structure: ID (4 bytes) + Size (int32) + Data *)
(* Write chunk header: ID (4-byte string) + Size (int32) *)
let write_chunk_header chunk_id size =
  if String.length chunk_id <> 4 then
    failwith ("Chunk ID must be exactly 4 bytes, got: " ^ chunk_id)
  else
    let id_bytes = List.init 4 (fun i -> Char.code (String.get chunk_id i)) in
    id_bytes @ write_int32 size

(* Write chunk: header + data *)
let write_chunk chunk_id data =
  let size = List.length data in
  write_chunk_header chunk_id size @ data

(* Write chunk to file *)
let write_chunk_to_file filename chunk_id data =
  write_bytes_to_file filename (write_chunk chunk_id data)

(* File header structure: Magic (4 bytes) + Version (int32) + ... *)
(* Write file header with magic string and version *)
let write_file_header magic version =
  if String.length magic <> 4 then
    failwith ("Magic must be exactly 4 bytes, got: " ^ magic)
  else
    let magic_bytes = List.init 4 (fun i -> Char.code (String.get magic i)) in
    magic_bytes @ write_int32 version

(* Write file header to file *)
let write_file_header_to_file filename magic version =
  write_bytes_to_file filename (write_file_header magic version)
