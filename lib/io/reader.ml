(* File reader - optimized with Array for performance *)

(* Convert list to array for fast access *)
let list_to_array bytes = 
  (Array.of_list bytes, 0)

(* Convert array back to list (for remaining bytes) *)
let array_to_list (arr, idx) =
  if idx >= Array.length arr then []
  else Array.to_list (Array.sub arr idx (Array.length arr - idx))

(* Read int32 from array - O(1) *)
let read_int32_array (arr, idx) =
  if idx + 4 > Array.length arr then
    failwith "Not enough bytes to read int32"
  else
    let b0 = arr.(idx) in
    let b1 = arr.(idx + 1) in
    let b2 = arr.(idx + 2) in
    let b3 = arr.(idx + 3) in
    let value = b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24) in
    let value = if value land 0x80000000 <> 0 then value - 0x100000000 else value in
    (value, (arr, idx + 4))

(* Read int16 from array - O(1) *)
let read_int16_array (arr, idx) =
  if idx + 2 > Array.length arr then
    failwith "Not enough bytes to read int16"
  else
    let b0 = arr.(idx) in
    let b1 = arr.(idx + 1) in
    let value = b0 lor (b1 lsl 8) in
    let value = if value land 0x8000 <> 0 then value - 0x10000 else value in
    (value, (arr, idx + 2))

(* Read uint8 from array - O(1) *)
let read_uint8_array (arr, idx) =
  if idx >= Array.length arr then
    failwith "Not enough bytes to read uint8"
  else
    (arr.(idx), (arr, idx + 1))

(* Read float from array - O(1) *)
let read_float_array (arr, idx) =
  if idx + 4 > Array.length arr then
    failwith "Not enough bytes to read float"
  else
    let b0 = Int32.of_int arr.(idx) in
    let b1 = Int32.of_int arr.(idx + 1) in
    let b2 = Int32.of_int arr.(idx + 2) in
    let b3 = Int32.of_int arr.(idx + 3) in
    let bits = Int32.logor b0
                 (Int32.logor (Int32.shift_left b1 8)
                    (Int32.logor (Int32.shift_left b2 16)
                       (Int32.shift_left b3 24))) in
    let value = Int32.float_of_bits bits in
    (value, (arr, idx + 4))

(* Public API: Read int32 from list (converts to array internally) *)
let read_int32 bytes =
  let (value, (arr, idx)) = read_int32_array (list_to_array bytes) in
  (value, array_to_list (arr, idx))

(* Public API: Read int16 from list *)
let read_int16 bytes =
  let (value, (arr, idx)) = read_int16_array (list_to_array bytes) in
  (value, array_to_list (arr, idx))

(* Public API: Read uint8 from list *)
let read_uint8 bytes =
  let (value, (arr, idx)) = read_uint8_array (list_to_array bytes) in
  (value, array_to_list (arr, idx))

(* Public API: Read float from list *)
let read_float bytes =
  let (value, (arr, idx)) = read_float_array (list_to_array bytes) in
  (value, array_to_list (arr, idx))

(* Read string from array - O(n) but efficient *)
let read_string_array (arr, idx) =
  let (len, (arr, idx)) = read_int32_array (arr, idx) in
  if idx + len > Array.length arr then
    failwith "Not enough bytes to read string"
  else
    let str = String.init len (fun i -> Char.chr arr.(idx + i)) in
    (str, (arr, idx + len))

(* Public API: Read string from list *)
let read_string bytes =
  let (value, (arr, idx)) = read_string_array (list_to_array bytes) in
  (value, array_to_list (arr, idx))

(* Read list of int32 from array - O(n) but efficient *)
let read_list_int32_array (arr, idx) =
  let (len, (arr, idx)) = read_int32_array (arr, idx) in
  let result = Array.make len 0 in
  let rec read_n i (arr, idx) =
    if i >= len then (result, (arr, idx))
    else
      let (value, (arr, idx)) = read_int32_array (arr, idx) in
      result.(i) <- value;
      read_n (i + 1) (arr, idx)
  in
  let (result_arr, (arr, idx)) = read_n 0 (arr, idx) in
  (Array.to_list result_arr, (arr, idx))

(* Public API: Read list of int32 from list *)
let read_list_int32 bytes =
  let (value, (arr, idx)) = read_list_int32_array (list_to_array bytes) in
  (value, array_to_list (arr, idx))

(* Read list of float from array - O(n) but efficient *)
let read_list_float_array (arr, idx) =
  let (len, (arr, idx)) = read_int32_array (arr, idx) in
  let result = Array.make len 0.0 in
  let rec read_n i (arr, idx) =
    if i >= len then (result, (arr, idx))
    else
      let (value, (arr, idx)) = read_float_array (arr, idx) in
      result.(i) <- value;
      read_n (i + 1) (arr, idx)
  in
  let (result_arr, (arr, idx)) = read_n 0 (arr, idx) in
  (Array.to_list result_arr, (arr, idx))

(* Public API: Read list of float from list *)
let read_list_float bytes =
  let (value, (arr, idx)) = read_list_float_array (list_to_array bytes) in
  (value, array_to_list (arr, idx))

(* Read list of uint8 from array - O(n) but efficient *)
let read_list_uint8_array (arr, idx) =
  let (len, (arr, idx)) = read_int32_array (arr, idx) in
  let result = Array.make len 0 in
  let rec read_n i (arr, idx) =
    if i >= len then (result, (arr, idx))
    else
      let (value, (arr, idx)) = read_uint8_array (arr, idx) in
      result.(i) <- value;
      read_n (i + 1) (arr, idx)
  in
  let (result_arr, (arr, idx)) = read_n 0 (arr, idx) in
  (Array.to_list result_arr, (arr, idx))

(* Public API: Read list of uint8 from list *)
let read_list_uint8 bytes =
  let (value, (arr, idx)) = read_list_uint8_array (list_to_array bytes) in
  (value, array_to_list (arr, idx))

(* Read all bytes from file - optimized with Array *)
let read_bytes_from_file filename =
  let ic = open_in_bin filename in
  try
    let len = in_channel_length ic in
    let arr = Array.make len 0 in
    for i = 0 to len - 1 do
      arr.(i) <- input_byte ic
    done;
    close_in ic;
    Array.to_list arr
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

(* Read 4-byte string (chunk ID, magic, etc.) - optimized *)
let read_string4 bytes =
  let (arr, idx) = list_to_array bytes in
  if idx + 4 > Array.length arr then
    failwith "Not enough bytes to read 4-byte string"
  else
    let str = String.init 4 (fun i -> Char.chr arr.(idx + i)) in
    (str, array_to_list (arr, idx + 4))

(* Read chunk header: ID (4 bytes) + Size (int32) *)
let read_chunk_header bytes =
  let (chunk_id, remaining) = read_string4 bytes in
  let (size, remaining) = read_int32 remaining in
  (chunk_id, size, remaining)

(* Read chunk: header + data - optimized *)
let read_chunk bytes =
  let (chunk_id, size, remaining) = read_chunk_header bytes in
  let (arr, idx) = list_to_array remaining in
  if idx + size > Array.length arr then
    failwith ("Not enough bytes to read chunk data: need " ^ string_of_int size ^ ", got " ^ string_of_int (Array.length arr - idx))
  else
    let data = Array.to_list (Array.sub arr idx size) in
    let remaining_after = array_to_list (arr, idx + size) in
    (chunk_id, data, remaining_after)

(* Read file header: Magic (4 bytes) + Version (int32) *)
let read_file_header bytes =
  let (magic, remaining) = read_string4 bytes in
  let (version, remaining) = read_int32 remaining in
  (magic, version, remaining)

(* Find chunk by ID in bytes - optimized *)
let find_chunk bytes chunk_id =
  let (arr, start_idx) = list_to_array bytes in
  let rec search idx =
    if idx + 8 > Array.length arr then (* Need at least 4 bytes ID + 4 bytes size *)
      None
    else
      let (id, size, _) = read_chunk_header (array_to_list (arr, idx)) in
      if id = chunk_id then
        (* Found it, read the data *)
        if idx + 8 + size > Array.length arr then
          None
        else
          let data = Array.to_list (Array.sub arr (idx + 8) size) in
          let remaining_after = array_to_list (arr, idx + 8 + size) in
          Some (id, data, remaining_after)
      else
        (* Skip this chunk and continue *)
        if idx + 8 + size > Array.length arr then
          None
        else
          search (idx + 8 + size)
  in
  search start_idx

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
