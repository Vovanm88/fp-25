let triangle_data =
  [
    [ 75 ];
    [ 95; 64 ];
    [ 17; 47; 82 ];
    [ 18; 35; 87; 10 ];
    [ 20; 04; 82; 47; 65 ];
    [ 19; 01; 23; 75; 03; 34 ];
    [ 88; 02; 77; 73; 07; 63; 67 ];
    [ 99; 65; 04; 28; 06; 16; 70; 92 ];
    [ 41; 41; 26; 56; 83; 40; 80; 70; 33 ];
    [ 41; 48; 72; 33; 47; 32; 37; 16; 94; 29 ];
    [ 53; 71; 44; 65; 25; 43; 91; 52; 97; 51; 14 ];
    [ 70; 11; 33; 28; 77; 73; 17; 78; 39; 68; 17; 57 ];
    [ 91; 71; 52; 38; 17; 14; 91; 43; 58; 50; 27; 29; 48 ];
    [ 63; 66; 04; 68; 89; 53; 67; 30; 73; 16; 69; 87; 40; 31 ];
    [ 04; 62; 98; 27; 23; 09; 70; 98; 73; 93; 38; 53; 60; 04; 23 ];
  ]

(* 1. Монолитная реализация с хвостовой рекурсией *)
let solve2_tail_rec triangle =
  let rec max_path_sum row_idx col_idx acc =
    if row_idx >= List.length triangle then acc
    else
      let current_row = List.nth triangle row_idx in
      let current_val = List.nth current_row col_idx in
      let new_acc = acc + current_val in
      if row_idx + 1 >= List.length triangle then new_acc
      else
        let left_sum = max_path_sum (row_idx + 1) col_idx new_acc in
        let right_sum = max_path_sum (row_idx + 1) (col_idx + 1) new_acc in
        max left_sum right_sum
  in
  max_path_sum 0 0 0

(* 1. Монолитная реализация с обычной рекурсией *)
let solve2_rec triangle =
  let rec max_path_sum row_idx col_idx =
    if row_idx >= List.length triangle then 0
    else
      let current_row = List.nth triangle row_idx in
      let current_val = List.nth current_row col_idx in
      if row_idx + 1 >= List.length triangle then current_val
      else
        let left_sum = max_path_sum (row_idx + 1) col_idx in
        let right_sum = max_path_sum (row_idx + 1) (col_idx + 1) in
        current_val + max left_sum right_sum
  in
  max_path_sum 0 0

let solve2_dp triangle =
  let rows = List.length triangle in
  let rec process_row row_idx =
    if row_idx >= rows then []
    else if row_idx = rows - 1 then List.nth triangle row_idx
    else
      let current_row = List.nth triangle row_idx in
      let next_row = process_row (row_idx + 1) in
      let rec combine_rows current next acc =
        match (current, next) with
        | [], _ | _, [] -> List.rev acc
        | [ x ], n1 :: n2 :: _ -> List.rev ((x + max n1 n2) :: acc)
        | x :: xs, n1 :: n2 :: ns ->
            let max_next = max n1 n2 in
            combine_rows xs (n2 :: ns) ((x + max_next) :: acc)
        | _ -> List.rev acc
      in
      combine_rows current_row next_row []
  in
  let result = process_row 0 in
  List.hd result

let solve2_modular triangle =
  let generate_paths triangle =
    let rec gen_paths row_idx col_idx path =
      if row_idx >= List.length triangle then [ List.rev path ]
      else
        let current_row = List.nth triangle row_idx in
        let current_val = List.nth current_row col_idx in
        let new_path = current_val :: path in
        if row_idx + 1 >= List.length triangle then [ List.rev new_path ]
        else
          let left_paths = gen_paths (row_idx + 1) col_idx new_path in
          let right_paths = gen_paths (row_idx + 1) (col_idx + 1) new_path in
          left_paths @ right_paths
    in
    gen_paths 0 0 []
  in

  let path_sum = List.fold_left ( + ) 0 in

  let max_sum paths =
    List.fold_left (fun max_val path -> max max_val (path_sum path)) 0 paths
  in

  generate_paths triangle |> max_sum

let solve2_map triangle =
  let rows = List.length triangle in
  let indices = List.init rows (fun i -> rows - 1 - i) in

  let rec build_max_sums remaining_indices prev_row =
    match remaining_indices with
    | [] -> List.hd prev_row
    | row_idx :: rest ->
        let current_row = List.nth triangle row_idx in
        let new_row =
          List.mapi
            (fun i val_ ->
              if i + 1 < List.length prev_row then
                val_ + max (List.nth prev_row i) (List.nth prev_row (i + 1))
              else val_ + List.nth prev_row i)
            current_row
        in
        build_max_sums rest new_row
  in

  let last_row = List.nth triangle (rows - 1) in
  build_max_sums (List.tl indices) last_row

let solve2_for_loop triangle =
  let rows = List.length triangle in
  let triangle_array = Array.of_list (List.map Array.of_list triangle) in
  let max_sums = Array.make_matrix rows rows 0 in

  for i = 0 to Array.length triangle_array.(rows - 1) - 1 do
    max_sums.(rows - 1).(i) <- triangle_array.(rows - 1).(i)
  done;

  for row = rows - 2 downto 0 do
    for col = 0 to Array.length triangle_array.(row) - 1 do
      let left = max_sums.(row + 1).(col) in
      let right = max_sums.(row + 1).(col + 1) in
      max_sums.(row).(col) <- triangle_array.(row).(col) + max left right
    done
  done;

  max_sums.(0).(0)

(* 5. Работа с бесконечными последовательностями (Seq) *)
let solve2_seq triangle =
  let reversed_triangle = List.rev triangle in
  let triangle_seq = List.to_seq reversed_triangle in

  let rec drop n seq =
    if n <= 0 then seq
    else
      match seq () with
      | Seq.Nil -> fun () -> Seq.Nil
      | Seq.Cons (_, rest) -> drop (n - 1) rest
  in

  let rec process_rows remaining_rows prev_max =
    match remaining_rows () with
    | Seq.Nil -> List.hd prev_max
    | Seq.Cons (current_row, rest) ->
        let new_max =
          List.mapi
            (fun i val_ ->
              if i + 1 < List.length prev_max then
                val_ + max (List.nth prev_max i) (List.nth prev_max (i + 1))
              else val_ + List.nth prev_max i)
            current_row
        in
        process_rows rest new_max
  in

  let last_row = List.hd reversed_triangle in
  let rest_rows = drop 1 triangle_seq in
  process_rows rest_rows last_row

let solve2 triangle = solve2_dp triangle
