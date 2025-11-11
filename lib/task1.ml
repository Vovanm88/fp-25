let triangle_number n = n * (n + 1) / 2

let count_divisors n =
  let rec count_divisors_rec i count =
    if i * i > n then count
    else if n mod i = 0 then
      if i * i = n then count_divisors_rec (i + 1) (count + 1)
      else count_divisors_rec (i + 1) (count + 2)
    else count_divisors_rec (i + 1) count
  in
  count_divisors_rec 1 0

(* 1. Монолитная реализация с хвостовой рекурсией *)
let solve1_tail_rec min_divisors =
  let rec find_triangle n =
    let tri = triangle_number n in
    let divisors = count_divisors tri in
    if divisors > min_divisors then tri else find_triangle (n + 1)
  in
  find_triangle 1

(* 1. Монолитная реализация с обычной рекурсией *)
let solve1_rec min_divisors =
  let rec find_triangle n =
    let tri = triangle_number n in
    let divisors = count_divisors tri in
    if divisors > min_divisors then tri
    else
      let next = find_triangle (n + 1) in
      next
  in
  find_triangle 1

let solve1_modular min_divisors =
  let generate_triangles n =
    let rec gen acc i =
      if i > n then List.rev acc else gen (triangle_number i :: acc) (i + 1)
    in
    gen [] 1
  in

  let filter_sufficient_divisors tri_list =
    List.filter (fun tri -> count_divisors tri > min_divisors) tri_list
  in

  let find_first = function
    | [] -> failwith "No triangle number found"
    | x :: _ -> x
  in

  let limit = 20000 in
  generate_triangles limit |> filter_sufficient_divisors |> find_first

(* 3. Генерация последовательности при помощи map *)
let solve1_map min_divisors =
  let limit = 20000 in
  let indices = List.init limit (fun i -> i + 1) in
  let triangles = List.map triangle_number indices in
  let with_divisors =
    List.map (fun tri -> (tri, count_divisors tri)) triangles
  in
  let filtered =
    List.filter (fun (_, divs) -> divs > min_divisors) with_divisors
  in
  match filtered with
  | [] -> failwith "No triangle number found"
  | (tri, _) :: _ -> tri

(* 4. Работа со спец. синтаксисом для циклов (for) *)
let solve1_for_loop min_divisors =
  let result = ref None in
  let found = ref false in
  for n = 1 to 20000 do
    if not !found then
      let tri = triangle_number n in
      let divisors = count_divisors tri in
      if divisors > min_divisors then (
        result := Some tri;
        found := true)
  done;
  match !result with Some x -> x | None -> failwith "No triangle number found"

(* 5. Работа с бесконечными последовательностями (Seq) *)
let solve1_seq min_divisors =
  let rec ints_from n = fun () -> Seq.Cons (n, ints_from (n + 1)) in
  let triangles = Seq.map triangle_number (ints_from 1) in
  let with_divisors =
    Seq.map (fun tri -> (tri, count_divisors tri)) triangles
  in
  let filtered =
    Seq.filter (fun (_, divs) -> divs > min_divisors) with_divisors
  in
  match filtered () with
  | Seq.Cons ((tri, _), _) -> tri
  | Seq.Nil -> failwith "No triangle number found"
