(* Huffman coding interface *)

type huffman_tree =
  | Leaf of char * int
  | Node of int * huffman_tree * huffman_tree

val encode : char list -> bool list * huffman_tree option
val decode : bool list -> huffman_tree option -> char list
val bools_to_bytes : bool list -> int list
val bytes_to_bools : int list -> bool list

(* Encode bytes (int list, values 0-255) using Huffman coding *)
(* Returns (encoded_bytes, num_bits, tree) where num_bits is the actual number of bits *)
val encode_bytes : int list -> int list * int * huffman_tree option

(* Decode bytes using Huffman tree *)
(* num_bits specifies how many bits to read (to handle padding) *)
val decode_bytes : int list -> int -> huffman_tree option -> int list

(* Serialize Huffman tree to bytes *)
val serialize_tree : huffman_tree -> int list

(* Deserialize Huffman tree from bytes *)
(* Returns (tree, remaining_bytes) *)
val deserialize_tree : int list -> huffman_tree * int list
