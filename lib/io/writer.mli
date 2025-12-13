(* File writer interface *)

(* Write int32 as little-endian bytes *)
val write_int32 : int -> int list

(* Write int16 as little-endian bytes *)
val write_int16 : int -> int list

(* Write uint8 as single byte *)
val write_uint8 : int -> int list

(* Write float as IEEE 754 single precision (32-bit) *)
val write_float : float -> int list

(* Write string as length (int32) + bytes *)
val write_string : string -> int list

(* Write list of int32 *)
val write_list_int32 : int list -> int list

(* Write list of float *)
val write_list_float : float list -> int list

(* Write list of uint8 *)
val write_list_uint8 : int list -> int list

(* Write bytes to file *)
val write_bytes_to_file : string -> int list -> unit

(* Write int32 to file *)
val write_int32_to_file : string -> int -> unit

(* Write int16 to file *)
val write_int16_to_file : string -> int -> unit

(* Write uint8 to file *)
val write_uint8_to_file : string -> int -> unit

(* Write float to file *)
val write_float_to_file : string -> float -> unit

(* Write string to file *)
val write_string_to_file : string -> string -> unit

(* Write list of int32 to file *)
val write_list_int32_to_file : string -> int list -> unit

(* Write list of float to file *)
val write_list_float_to_file : string -> float list -> unit

(* Write arbitrary bytes list to file *)
val write_to_file : string -> int list -> unit

(* Chunk structure: ID (4 bytes) + Size (int32) + Data *)
(* Write chunk header: ID (4-byte string) + Size (int32) *)
val write_chunk_header : string -> int -> int list

(* Write chunk: header + data *)
val write_chunk : string -> int list -> int list

(* Write chunk to file *)
val write_chunk_to_file : string -> string -> int list -> unit

(* File header structure: Magic (4 bytes) + Version (int32) *)
(* Write file header with magic string and version *)
val write_file_header : string -> int -> int list

(* Write file header to file *)
val write_file_header_to_file : string -> string -> int -> unit
