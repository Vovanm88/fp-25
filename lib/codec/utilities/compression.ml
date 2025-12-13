(* Huffman coding for data compression *)
module CharMap = Map.Make(Char)
module IntMap = Map.Make(Int)

(* Binary tree for Huffman coding *)
type huffman_tree =
  | Leaf of char * int
  | Node of int * huffman_tree * huffman_tree

(* Frequency table: count occurrences of each character *)
let build_frequency_table data =
  let rec count acc = function
    | [] -> acc
    | c :: rest ->
      let count' = try CharMap.find c acc with Not_found -> 0 in
      count (CharMap.add c (count' + 1) acc) rest
  in
  count CharMap.empty data

(* Build Huffman tree from frequency table *)
let build_huffman_tree freq_map =
  let freq_list = CharMap.bindings freq_map in
  if List.length freq_list = 0 then
    failwith "Empty frequency table"
  else if List.length freq_list = 1 then
    let (c, freq) = List.hd freq_list in
    Leaf (c, freq)
  else
    (* Create priority queue (sorted list) of trees *)
    let rec insert_tree tree = function
      | [] -> [tree]
      | hd :: tl as lst ->
        let freq = function
          | Leaf (_, f) -> f
          | Node (f, _, _) -> f
        in
        if freq tree <= freq hd then tree :: lst
        else hd :: insert_tree tree tl
    in
    (* Build initial list of leaf nodes *)
    let initial_trees = List.map (fun (c, f) -> Leaf (c, f)) freq_list in
    let sorted_trees = List.sort (fun t1 t2 ->
      let freq = function
        | Leaf (_, f) -> f
        | Node (f, _, _) -> f
      in
      compare (freq t1) (freq t2)) initial_trees in
    (* Build tree by combining two smallest trees *)
    let rec build_tree = function
      | [] -> failwith "Empty tree list"
      | [t] -> t
      | t1 :: t2 :: rest ->
        let freq = function
          | Leaf (_, f) -> f
          | Node (f, _, _) -> f
        in
        let f1 = freq t1 in
        let f2 = freq t2 in
        let new_node = Node (f1 + f2, t1, t2) in
        build_tree (insert_tree new_node rest)
    in
    build_tree sorted_trees

(* Build encoding table from Huffman tree *)
let build_encoding_table tree =
  let rec traverse path = function
    | Leaf (c, _) -> [(c, List.rev path)]
    | Node (_, left, right) ->
      traverse (false :: path) left @ traverse (true :: path) right
  in
  let encoding_list = traverse [] tree in
  List.fold_left
    (fun acc (c, code) -> CharMap.add c code acc)
    CharMap.empty
    encoding_list

(* Encode data using Huffman coding *)
let encode data =
  if List.length data = 0 then ([], None)
  else
    let freq_table = build_frequency_table data in
    let tree = build_huffman_tree freq_table in
    let encoding_table = build_encoding_table tree in
    (* Special case: if tree is a single leaf, all characters are the same *)
    let encoded = match tree with
      | Leaf (_, _) -> 
        (* For single character, we don't need to encode bits *)
        []
      | Node _ ->
        List.fold_left
          (fun acc c ->
            try
              let code = CharMap.find c encoding_table in
              acc @ code
            with Not_found ->
              failwith ("Character not found in encoding table: " ^ String.make 1 c))
          []
          data
    in
    (encoded, Some tree)

(* Decode data using Huffman tree *)
let decode encoded tree =
  match tree with
  | None -> []
  | Some t ->
    (* Special case: tree with single character *)
    (match t with
     | Leaf (c, count) ->
       (* For single character, return the character count times *)
       List.init count (fun _ -> c)
     | Node _ ->
       let rec decode_aux current_tree = function
         | [] -> []
         | bits ->
           let rec traverse_tree tr = function
             | [] -> 
               (* If we reach end of bits while in tree, it's an error unless we're at a leaf *)
               (match tr with
                | Leaf (c, _) -> (c, [])
                | Node _ -> failwith "Unexpected end of encoded data")
             | bit :: rest ->
               match tr with
               | Leaf (c, _) -> (c, bit :: rest)
               | Node (_, left, right) ->
                 if bit then traverse_tree right rest
                 else traverse_tree left rest
           in
           let (c, remaining) = traverse_tree current_tree bits in
           if List.length remaining = 0 then [c]
           else c :: decode_aux t remaining
       in
       decode_aux t encoded)

(* Convert boolean list to bytes for storage *)
let bools_to_bytes bools =
  let rec aux acc current_byte bit_pos = function
    | [] -> 
      if bit_pos = 0 then acc
      else acc @ [current_byte]
    | b :: rest ->
      let new_byte = if b then (current_byte lsl 1) lor 1 else (current_byte lsl 1) in
      let new_bit_pos = (bit_pos + 1) mod 8 in
      if new_bit_pos = 0 then
        aux (acc @ [new_byte]) 0 0 rest
      else
        aux acc new_byte new_bit_pos rest
  in
  aux [] 0 0 bools

(* Convert bytes to boolean list *)
(* Reads bits from MSB to LSB to match bools_to_bytes *)
let bytes_to_bools bytes =
  let rec aux acc = function
    | [] -> acc
    | byte :: rest ->
      let rec byte_to_bools b pos =
        if pos >= 8 then []
        else
          ((b land (1 lsl (7 - pos))) <> 0) :: byte_to_bools b (pos + 1)
      in
      let bits = byte_to_bools byte 0 in
      aux (acc @ bits) rest
  in
  aux [] bytes

(* Encode bytes using Huffman coding *)
(* Uses existing huffman_tree type by converting bytes to chars *)
(* Returns (encoded_bytes, num_bits, tree) where num_bits is the actual number of bits *)
let encode_bytes data =
  if List.length data = 0 then ([], 0, None)
  else
    (* Convert bytes to chars for encoding *)
    let char_data = List.map (fun b -> 
      if b < 0 || b > 255 then
        failwith ("Invalid byte value: " ^ string_of_int b)
      else Char.chr b) data in
    (* Use existing encode function *)
    let (encoded_bools, tree) = encode char_data in
    let num_bits = List.length encoded_bools in
    (* Convert bools to bytes for return *)
    let encoded_bytes = bools_to_bytes encoded_bools in
    (encoded_bytes, num_bits, tree)

(* Decode bytes using Huffman tree *)
(* Converts encoded bytes back to bools, decodes, then converts chars back to bytes *)
(* num_bits specifies how many bits to read (to handle padding) *)
let decode_bytes encoded_bytes num_bits tree =
  (* Convert bytes to bools *)
  let all_bools = bytes_to_bools encoded_bytes in
  (* Take only the first num_bits bits to avoid padding issues *)
  let encoded_bools = 
    if num_bits = 0 then
      []
    else if List.length all_bools >= num_bits then
      let rec take n acc = function
        | [] -> List.rev acc
        | hd :: tl when n > 0 -> take (n - 1) (hd :: acc) tl
        | _ -> List.rev acc
      in
      take num_bits [] all_bools
    else
      all_bools
  in
  (* Use existing decode function *)
  let decoded_chars = decode encoded_bools tree in
  (* Convert chars back to bytes *)
  List.map (fun c -> Char.code c) decoded_chars

(* Serialize Huffman tree to bytes *)
(* Format: 
   - 0x00 = Leaf, followed by char (1 byte), frequency (4 bytes, little-endian)
   - 0x01 = Node, followed by frequency (4 bytes), then left subtree, then right subtree
*)
let serialize_tree tree =
  let rec serialize = function
    | Leaf (c, freq) ->
      let char_byte = Char.code c in
      [0x00; char_byte] @
      (* Write frequency as 4 bytes little-endian *)
      [freq land 0xFF;
       (freq lsr 8) land 0xFF;
       (freq lsr 16) land 0xFF;
       (freq lsr 24) land 0xFF]
    | Node (freq, left, right) ->
      [0x01] @
      (* Write frequency as 4 bytes little-endian *)
      [freq land 0xFF;
       (freq lsr 8) land 0xFF;
       (freq lsr 16) land 0xFF;
       (freq lsr 24) land 0xFF] @
      serialize left @
      serialize right
  in
  serialize tree

(* Deserialize Huffman tree from bytes *)
(* Returns (tree, remaining_bytes) *)
let deserialize_tree bytes =
  let read_int32_le bytes =
    if List.length bytes < 4 then
      failwith "Not enough bytes to read int32"
    else
      let b0 = List.nth bytes 0 in
      let b1 = List.nth bytes 1 in
      let b2 = List.nth bytes 2 in
      let b3 = List.nth bytes 3 in
      (b0 lor (b1 lsl 8) lor (b2 lsl 16) lor (b3 lsl 24), List.tl (List.tl (List.tl (List.tl bytes))))
  in
  let rec deserialize bytes =
    if List.length bytes = 0 then
      failwith "Unexpected end of bytes while deserializing tree"
    else
      match List.hd bytes with
      | 0x00 -> (* Leaf *)
        if List.length bytes < 6 then
          failwith "Not enough bytes for Leaf node"
        else
          let char_byte = List.nth bytes 1 in
          let (freq, remaining) = read_int32_le (List.tl (List.tl bytes)) in
          (Leaf (Char.chr char_byte, freq), remaining)
      | 0x01 -> (* Node *)
        if List.length bytes < 5 then
          failwith "Not enough bytes for Node frequency"
        else
          let (freq, remaining_after_freq) = read_int32_le (List.tl bytes) in
          let (left, remaining_after_left) = deserialize remaining_after_freq in
          let (right, remaining) = deserialize remaining_after_left in
          (Node (freq, left, right), remaining)
      | _ -> failwith ("Invalid tree node type: " ^ string_of_int (List.hd bytes))
  in
  deserialize bytes

