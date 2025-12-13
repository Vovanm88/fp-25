(* Uniform quantization interface *)

val quantize : ?min_val:float -> ?max_val:float -> float list -> int -> int list
val dequantize : int list -> int -> float -> float -> float list
val quantize_with_range : float list -> int -> int list * float * float
val dequantize_with_range : int list -> int -> float -> float -> float list

(* Efficient encoding/decoding of quantized indices *)
(* encode_quantized_indices: packs indices into bytes using ceil(log2(n)) bits per element *)
val encode_quantized_indices : int list -> int -> int list
(* decode_quantized_indices: unpacks indices from bytes *)
(* bytes: list of bytes (int list, 0-255), num_elements: number of indices, n: quantization levels *)
val decode_quantized_indices : int list -> int -> int -> int list

