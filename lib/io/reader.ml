(* File reader - low-level binary deserialization functions *)

(* Read int32 from little-endian bytes *)
(* Returns (value, remaining_bytes) *)
let read_int32 bytes =
  if List.length bytes < 4 then
    failwith "Not enough bytes to read int32"
  else
    let b0 = List.nth bytes 0 in
    let b1 = List.nth bytes 1 in
    let b2 = List.nth bytes 2 in
    let b3 = List.nth bytes 3 in
    let value = b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24) in
    (* Convert to signed int32 *)
    let value = if value land 0x80000000 <> 0 then value - 0x100000000 else value in
    (value, List.tl (List.tl (List.tl (List.tl bytes))))

(* Read int16 from little-endian bytes *)
(* Returns (value, remaining_bytes) *)
let read_int16 bytes =
  if List.length bytes < 2 then
    failwith "Not enough bytes to read int16"
  else
    let b0 = List.nth bytes 0 in
    let b1 = List.nth bytes 1 in
    let value = b0 lor (b1 lsl 8) in
    (* Convert to signed int16 *)
    let value = if value land 0x8000 <> 0 then value - 0x10000 else value in
    (value, List.tl (List.tl bytes))

(* Read uint8 from single byte *)
(* Returns (value, remaining_bytes) *)
let read_uint8 bytes =
  if List.length bytes < 1 then
    failwith "Not enough bytes to read uint8"
  else
    (List.hd bytes, List.tl bytes)

(* Read float from IEEE 754 single precision (32-bit) *)
(* Returns (value, remaining_bytes) *)
let read_float bytes =
  if List.length bytes < 4 then
    failwith "Not enough bytes to read float"
  else
    let b0 = Int32.of_int (List.nth bytes 0) in
    let b1 = Int32.of_int (List.nth bytes 1) in
    let b2 = Int32.of_int (List.nth bytes 2) in
    let b3 = Int32.of_int (List.nth bytes 3) in
    let bits = Int32.logor b0
                 (Int32.logor (Int32.shift_left b1 8)
                    (Int32.logor (Int32.shift_left b2 16)
                       (Int32.shift_left b3 24))) in
    let value = Int32.float_of_bits bits in
    (value, List.tl (List.tl (List.tl (List.tl bytes))))

(* Read string as length (int32) + bytes *)
(* Returns (value, remaining_bytes) *)
let read_string bytes =
  let (len, remaining) = read_int32 bytes in
  if List.length remaining < len then
    failwith "Not enough bytes to read string"
  else
    let str_bytes = List.init len (fun i -> List.nth remaining i) in
    let str = String.init len (fun i -> Char.chr (List.nth str_bytes i)) in
    let remaining_after = List.init (List.length remaining - len) 
      (fun i -> List.nth remaining (len + i)) in
    (str, remaining_after)

(* Read list of int32 *)
(* Returns (value, remaining_bytes) *)
let read_list_int32 bytes =
  let (len, remaining) = read_int32 bytes in
  let rec read_n acc remaining n =
    if n = 0 then (List.rev acc, remaining)
    else
      let (value, rest) = read_int32 remaining in
      read_n (value :: acc) rest (n - 1)
  in
  read_n [] remaining len

(* Read list of float *)
(* Returns (value, remaining_bytes) *)
let read_list_float bytes =
  let (len, remaining) = read_int32 bytes in
  let rec read_n acc remaining n =
    if n = 0 then (List.rev acc, remaining)
    else
      let (value, rest) = read_float remaining in
      read_n (value :: acc) rest (n - 1)
  in
  read_n [] remaining len

(* Read all bytes from file *)
let read_bytes_from_file filename =
  let ic = open_in_bin filename in
  try
    let rec read_all acc =
      try
        let byte = input_byte ic in
        read_all (byte :: acc)
      with
      | End_of_file -> List.rev acc
    in
    let bytes = read_all [] in
    close_in ic;
    bytes
  with
  | e ->
    close_in ic;
    raise e

(* Read int32 from file *)
let read_int32_from_file filename =
  let bytes = read_bytes_from_file filename in
  let (value, remaining) = read_int32 bytes in
  if List.length remaining > 0 then
    failwith "Extra bytes after reading int32 from file"
  else
    value

(* Read int16 from file *)
let read_int16_from_file filename =
  let bytes = read_bytes_from_file filename in
  let (value, remaining) = read_int16 bytes in
  if List.length remaining > 0 then
    failwith "Extra bytes after reading int16 from file"
  else
    value

(* Read uint8 from file *)
let read_uint8_from_file filename =
  let bytes = read_bytes_from_file filename in
  let (value, remaining) = read_uint8 bytes in
  if List.length remaining > 0 then
    failwith "Extra bytes after reading uint8 from file"
  else
    value

(* Read float from file *)
let read_float_from_file filename =
  let bytes = read_bytes_from_file filename in
  let (value, remaining) = read_float bytes in
  if List.length remaining > 0 then
    failwith "Extra bytes after reading float from file"
  else
    value

(* Read string from file *)
let read_string_from_file filename =
  let bytes = read_bytes_from_file filename in
  let (value, remaining) = read_string bytes in
  if List.length remaining > 0 then
    failwith "Extra bytes after reading string from file"
  else
    value

(* Read list of int32 from file *)
let read_list_int32_from_file filename =
  let bytes = read_bytes_from_file filename in
  let (value, remaining) = read_list_int32 bytes in
  if List.length remaining > 0 then
    failwith "Extra bytes after reading list_int32 from file"
  else
    value

(* Read list of float from file *)
let read_list_float_from_file filename =
  let bytes = read_bytes_from_file filename in
  let (value, remaining) = read_list_float bytes in
  if List.length remaining > 0 then
    failwith "Extra bytes after reading list_float from file"
  else
    value

(* Read all bytes from file (low-level) *)
let read_from_file filename =
  read_bytes_from_file filename

(* Read 4-byte string (chunk ID, magic, etc.) *)
(* Returns (string, remaining_bytes) *)
let read_string4 bytes =
  if List.length bytes < 4 then
    failwith "Not enough bytes to read 4-byte string"
  else
    let str = String.init 4 (fun i -> Char.chr (List.nth bytes i)) in
    (str, List.tl (List.tl (List.tl (List.tl bytes))))

(* Read chunk header: ID (4 bytes) + Size (int32) *)
(* Returns (chunk_id, size, remaining_bytes) *)
let read_chunk_header bytes =
  let (chunk_id, remaining) = read_string4 bytes in
  let (size, remaining) = read_int32 remaining in
  (chunk_id, size, remaining)

(* Read chunk: header + data *)
(* Returns (chunk_id, data, remaining_bytes) *)
let read_chunk bytes =
  let (chunk_id, size, remaining) = read_chunk_header bytes in
  if List.length remaining < size then
    failwith ("Not enough bytes to read chunk data: need " ^ string_of_int size ^ ", got " ^ string_of_int (List.length remaining))
  else
    let data = List.init size (fun i -> List.nth remaining i) in
    let remaining_after = List.init (List.length remaining - size)
      (fun i -> List.nth remaining (size + i)) in
    (chunk_id, data, remaining_after)

(* Read file header: Magic (4 bytes) + Version (int32) *)
(* Returns (magic, version, remaining_bytes) *)
let read_file_header bytes =
  let (magic, remaining) = read_string4 bytes in
  let (version, remaining) = read_int32 remaining in
  (magic, version, remaining)

(* Find chunk by ID in bytes *)
(* Returns Some (chunk_id, data, remaining_bytes) or None if not found *)
let find_chunk bytes chunk_id =
  let rec search remaining =
    if List.length remaining < 8 then (* Need at least 4 bytes ID + 4 bytes size *)
      None
    else
      let (id, size, data_remaining) = read_chunk_header remaining in
      if id = chunk_id then
        (* Found it, read the data *)
        if List.length data_remaining < size then
          None
        else
          let data = List.init size (fun i -> List.nth data_remaining i) in
          let remaining_after = List.init (List.length data_remaining - size)
            (fun i -> List.nth data_remaining (size + i)) in
          Some (id, data, remaining_after)
      else
        (* Skip this chunk and continue *)
        if List.length data_remaining < size then
          None
        else
          let remaining_after = List.init (List.length data_remaining - size)
            (fun i -> List.nth data_remaining (size + i)) in
          search remaining_after
  in
  search bytes

(* Read chunk from file *)
let read_chunk_from_file filename =
  let bytes = read_bytes_from_file filename in
  read_chunk bytes

(* Read file header from file *)
let read_file_header_from_file filename =
  let bytes = read_bytes_from_file filename in
  read_file_header bytes

(* Find chunk in file *)
let find_chunk_in_file filename chunk_id =
  let bytes = read_bytes_from_file filename in
  find_chunk bytes chunk_id

