(* Uniform quantization and dequantization *)

(* Uniform quantizer: quantizes float values to discrete levels *)
(* Parameters:
   - data: input signal (list of floats)
   - num_levels: number of quantization levels (must be > 0)
   - min_val: minimum value in the range (optional, auto-detected if None)
   - max_val: maximum value in the range (optional, auto-detected if None)
*)
let quantize ?min_val ?max_val data num_levels =
  if num_levels <= 0 then failwith "Number of quantization levels must be > 0"
  else if List.length data = 0 then []
  else
    (* Determine min and max values *)
    let min_v = match min_val with
      | Some v -> v
      | None -> List.fold_left min infinity data
    in
    let max_v = match max_val with
      | Some v -> v
      | None -> List.fold_left max neg_infinity data
    in
    if max_v = min_v then
      (* All values are the same, quantize to 0 - use efficient approach *)
      let len = List.length data in
      if len = 0 then []
      else
        (* Create list of zeros more efficiently *)
        let rec zeros n acc = if n <= 0 then acc else zeros (n - 1) (0 :: acc) in
        zeros len []
    else
      (* Calculate step size *)
      let step = (max_v -. min_v) /. float_of_int num_levels in
      (* Quantize each value *)
      List.map
        (fun x ->
          (* Clamp value to [min_v, max_v] *)
          let clamped = max min_v (min max_v x) in
          (* Quantize: map to level index *)
          let level = int_of_float ((clamped -. min_v) /. step) in
          (* Ensure level is in valid range [0, num_levels-1] *)
          min (num_levels - 1) (max 0 level))
        data

(* Uniform dequantizer: reconstructs float values from quantized levels *)
(* Parameters:
   - quantized: quantized levels (list of integers)
   - num_levels: number of quantization levels (must match quantization)
   - min_val: minimum value in the range
   - max_val: maximum value in the range
*)
let dequantize quantized num_levels min_val max_val =
  if num_levels <= 0 then failwith "Number of quantization levels must be > 0"
  else if max_val = min_val then
    (* All values should be the same - use efficient approach *)
    let len = List.length quantized in
    if len = 0 then []
    else
      (* Create list of constant values more efficiently *)
      let rec constant n acc = if n <= 0 then acc else constant (n - 1) (min_val :: acc) in
      constant len []
  else
    (* Calculate step size *)
    let step = (max_val -. min_val) /. float_of_int num_levels in
    (* Dequantize each level *)
    List.map
      (fun level ->
        (* Clamp level to valid range *)
        let clamped_level = min (num_levels - 1) (max 0 level) in
        (* Reconstruct value: midpoint of the quantization bin *)
        min_val +. (float_of_int clamped_level +. 0.5) *. step)
      quantized

(* Quantize with automatic range detection and return range info *)
let quantize_with_range data num_levels =
  if List.length data = 0 then ([], 0.0, 0.0)
  else
    let min_v = List.fold_left min infinity data in
    let max_v = List.fold_left max neg_infinity data in
    let quantized = quantize ~min_val:min_v ~max_val:max_v data num_levels in
    (quantized, min_v, max_v)

(* Dequantize using range info *)
let dequantize_with_range quantized num_levels min_val max_val =
  dequantize quantized num_levels min_val max_val

(* Calculate bit-width needed for n quantization levels *)
(* Uses ceil(log2(n)) with minimum 1 bit, maximum 32 bits *)
let calculate_bit_width n =
  if n <= 0 then failwith "Number of quantization levels must be > 0"
  else if n = 1 then 1
  else
    let log2_n = Float.log2 (float_of_int n) in
    let bits = int_of_float (Float.ceil log2_n) in
    min 32 (max 1 bits)

(* Encode quantized indices into packed bytes *)
(* Parameters:
   - indices: list of quantized indices (0 to n-1)
   - n: number of quantization levels
   Returns: list of bytes (int list, each int 0-255)
*)
let encode_quantized_indices indices n =
  if n <= 0 then failwith "Number of quantization levels must be > 0"
  else if List.length indices = 0 then []
  else
    let bit_width = calculate_bit_width n in
    let max_index = n - 1 in
    (* Pack bits into bytes *)
    let rec pack_bits acc current_byte bit_pos = function
      | [] ->
        (* Flush remaining bits if any *)
        if bit_pos = 0 then List.rev acc
        else List.rev (current_byte :: acc)
      | index :: rest ->
        (* Clamp index to valid range *)
        let clamped_index = min max_index (max 0 index) in
        (* Pack this index's bits into current byte *)
        let rec pack_index idx remaining_bits byte pos =
          if remaining_bits = 0 then (byte, pos)
          else
            let bit = (idx land 1) in
            let new_byte = if bit = 1 then byte lor (1 lsl pos) else byte in
            pack_index (idx lsr 1) (remaining_bits - 1) new_byte (pos + 1)
        in
        let (new_byte, new_pos) = pack_index clamped_index bit_width current_byte bit_pos in
        if new_pos >= 8 then
          (* Current byte is full, start new byte *)
          let byte_to_add = new_byte land 0xFF in
          let remaining_bits = new_pos - 8 in
          let next_byte = (new_byte lsr 8) land ((1 lsl remaining_bits) - 1) in
          pack_bits (byte_to_add :: acc) next_byte remaining_bits rest
        else
          pack_bits acc new_byte new_pos rest
    in
    pack_bits [] 0 0 indices

(* Decode quantized indices from packed bytes *)
(* Parameters:
   - bytes: list of bytes (int list, each int 0-255)
   - num_elements: number of indices to decode
   - n: number of quantization levels
   Returns: list of indices
*)
let decode_quantized_indices bytes num_elements n =
  if n <= 0 then failwith "Number of quantization levels must be > 0"
  else if num_elements <= 0 then []
  else if List.length bytes = 0 then []
  else
    let bit_width = calculate_bit_width n in
    (* Convert bytes to bit stream (list of bools, LSB first per byte, bytes in order) *)
    let bits = List.fold_left
      (fun acc byte ->
        let rec byte_to_bits b pos =
          if pos >= 8 then []
          else ((b land (1 lsl pos)) <> 0) :: byte_to_bits b (pos + 1)
        in
        acc @ byte_to_bits byte 0)
      []
      bytes
    in
    (* Extract indices from bit stream *)
    let rec extract_indices acc remaining_bits count =
      if count = 0 then List.rev acc
      else if List.length remaining_bits < bit_width then
        (* Not enough bits, return what we have *)
        List.rev acc
      else
        (* Extract bit_width bits *)
        let rec bits_to_index bits_left acc_idx = function
          | [] -> (acc_idx, [])
          | b :: rest when bits_left > 0 ->
            let bit_val = if b then 1 else 0 in
            let new_acc = acc_idx lor (bit_val lsl (bit_width - bits_left)) in
            bits_to_index (bits_left - 1) new_acc rest
          | rest -> (acc_idx, rest)
        in
        let (index, remaining) = bits_to_index bit_width 0 remaining_bits in
        (* Clamp index to valid range *)
        let clamped_index = min (n - 1) (max 0 index) in
        extract_indices (clamped_index :: acc) remaining (count - 1)
    in
    extract_indices [] bits num_elements

