type color = Red | Black

type 'a tree =
  | Leaf
  | Node of color * 'a tree * 'a * 'a tree

let blacken = function
  | Leaf -> Leaf
  | Node (_, l, x, r) -> Node (Black, l, x, r)

let rec insert cmp x = function
  | Leaf -> Node (Red, Leaf, x, Leaf)
  | Node (col, l, v, r) ->
      let comp = cmp x v in
      if comp < 0 then balance (Node (col, insert cmp x l, v, r))
      else if comp > 0 then balance (Node (col, l, v, insert cmp x r))
      else Node (col, l, v, r)

and balance = function
  | Node (Black, Node (Red, Node (Red, a, x, b), y, c), z, d)
  | Node (Black, Node (Red, a, x, Node (Red, b, y, c)), z, d)
  | Node (Black, a, x, Node (Red, Node (Red, b, y, c), z, d))
  | Node (Black, a, x, Node (Red, b, y, Node (Red, c, z, d))) ->
      Node (Red, Node (Black, a, x, b), y, Node (Black, c, z, d))
  | t -> t

let rec remove_min = function
  | Leaf -> (Leaf, None)
  | Node (_, Leaf, x, r) -> (r, Some x)
  | Node (c, l, x, r) ->
      let l', min = remove_min l in
      match min with
      | None -> (r, Some x)
      | Some m -> (balance (Node (c, l', x, r)), Some m)

let rec remove cmp x = function
  | Leaf -> Leaf
  | Node (col, l, v, r) ->
      let comp = cmp x v in
      if comp < 0 then balance (Node (col, remove cmp x l, v, r))
      else if comp > 0 then balance (Node (col, l, v, remove cmp x r))
      else
        match remove_min r with
        | Leaf, None -> blacken l
        | Node (_, _, _, _), None -> blacken l
        | r', Some min -> balance (Node (col, l, min, r'))

let rec fold_left f acc = function
  | Leaf -> acc
  | Node (_, l, x, r) -> fold_left f (f (fold_left f acc l) x) r

let rec fold_right f = function
  | Leaf -> fun acc -> acc
  | Node (_, l, x, r) -> fun acc -> f x (fold_right f r (fold_right f l acc))

let rec to_list = function
  | Leaf -> []
  | Node (_, l, x, r) -> to_list l @ [ x ] @ to_list r

let union cmp t1 t2 =
  match (t1, t2) with
  | Leaf, t | t, Leaf -> t
  | _ ->
      (* Простое объединение: добавляем все элементы из t1 в t2 *)
      fold_left (fun acc x -> insert cmp x acc) t2 t1

let rec filter p cmp = function
  | Leaf -> Leaf
  | Node (_, l, x, r) ->
      let l' = filter p cmp l in
      let r' = filter p cmp r in
      if p x then
        let merged = union cmp l' r' in
        insert cmp x merged
      else union cmp l' r'

let merge l r =
  match remove_min r with
  | Leaf, None -> blacken l
  | Node (_, _, _, _), None -> blacken l
  | r', Some min -> balance (Node (Black, l, min, r'))

let rec map f cmp = function
  | Leaf -> Leaf
  | Node (_, l, x, r) ->
      let x' = f x in
      let l' = map f cmp l in
      let r' = map f cmp r in
      insert cmp x' (union cmp l' r')

let compare_trees cmp t1 t2 =
  let list1 = to_list t1 in
  let list2 = to_list t2 in
  let rec compare_lists l1 l2 =
    match (l1, l2) with
    | [], [] -> 0
    | [], _ -> -1
    | _, [] -> 1
    | x1 :: r1, x2 :: r2 ->
        let c = cmp x1 x2 in
        if c <> 0 then c else compare_lists r1 r2
  in
  compare_lists list1 list2

let of_list cmp xs =
  List.fold_left (fun acc x -> insert cmp x acc) Leaf xs

