(* File reader interface *)

(* Array conversion helpers for efficient reading *)
val list_to_array : int list -> int array * int
val array_to_list : int array * int -> int list

(* Array-based reading functions for performance *)
val read_int32_array : int array * int -> int * (int array * int)
val read_uint8_array : int array * int -> int * (int array * int)
val read_float_array : int array * int -> float * (int array * int)
val read_list_int32_array : int array * int -> int list * (int array * int)
val read_list_uint8_array : int array * int -> int list * (int array * int)
val read_list_float_array : int array * int -> float list * (int array * int)

(* Read int32 from little-endian bytes *)
(* Returns (value, remaining_bytes) *)
val read_int32 : int list -> int * int list

(* Read int16 from little-endian bytes *)
(* Returns (value, remaining_bytes) *)
val read_int16 : int list -> int * int list

(* Read uint8 from single byte *)
(* Returns (value, remaining_bytes) *)
val read_uint8 : int list -> int * int list

(* Read float from IEEE 754 single precision (32-bit) *)
(* Returns (value, remaining_bytes) *)
val read_float : int list -> float * int list

(* Read string as length (int32) + bytes *)
(* Returns (value, remaining_bytes) *)
val read_string : int list -> string * int list

(* Read list of int32 *)
(* Returns (value, remaining_bytes) *)
val read_list_int32 : int list -> int list * int list

(* Read list of float *)
(* Returns (value, remaining_bytes) *)
val read_list_float : int list -> float list * int list

(* Read list of uint8 *)
(* Returns (value, remaining_bytes) *)
val read_list_uint8 : int list -> int list * int list

(* Read all bytes from file *)
val read_bytes_from_file : string -> int list

(* Read int32 from file *)
val read_int32_from_file : string -> int

(* Read int16 from file *)
val read_int16_from_file : string -> int

(* Read uint8 from file *)
val read_uint8_from_file : string -> int

(* Read float from file *)
val read_float_from_file : string -> float

(* Read string from file *)
val read_string_from_file : string -> string

(* Read list of int32 from file *)
val read_list_int32_from_file : string -> int list

(* Read list of float from file *)
val read_list_float_from_file : string -> float list

(* Read all bytes from file (low-level) *)
val read_from_file : string -> int list

(* Read 4-byte string (chunk ID, magic, etc.) *)
(* Returns (string, remaining_bytes) *)
val read_string4 : int list -> string * int list

(* Read chunk header: ID (4 bytes) + Size (int32) *)
(* Returns (chunk_id, size, remaining_bytes) *)
val read_chunk_header : int list -> string * int * int list

(* Read chunk: header + data *)
(* Returns (chunk_id, data, remaining_bytes) *)
val read_chunk : int list -> string * int list * int list

(* Read file header: Magic (4 bytes) + Version (int32) *)
(* Returns (magic, version, remaining_bytes) *)
val read_file_header : int list -> string * int * int list

(* Find chunk by ID in bytes *)
(* Returns Some (chunk_id, data, remaining_bytes) or None if not found *)
val find_chunk : int list -> string -> (string * int list * int list) option

(* Read chunk from file *)
val read_chunk_from_file : string -> string * int list * int list

(* Read file header from file *)
val read_file_header_from_file : string -> string * int * int list

(* Find chunk in file *)
val find_chunk_in_file :
  string -> string -> (string * int list * int list) option
