type 'a t = { cmp : 'a -> 'a -> int; tree : 'a Rb_set_impl.tree }

let make cmp = { cmp; tree = Rb_set_impl.Leaf }

let empty = { cmp = (fun _ _ -> 0); tree = Rb_set_impl.Leaf }

let add x s = { s with tree = Rb_set_impl.insert s.cmp x s.tree |> Rb_set_impl.blacken }

let remove x s = { s with tree = Rb_set_impl.remove s.cmp x s.tree }

let filter p s = { s with tree = Rb_set_impl.filter p s.cmp s.tree }

let map f cmp' s =
  { cmp = cmp'; tree = Rb_set_impl.map f cmp' s.tree |> Rb_set_impl.blacken }

let fold_left f acc s = Rb_set_impl.fold_left f acc s.tree

let fold_right f s acc = Rb_set_impl.fold_right f s.tree acc

let append s1 s2 =
  if s1.cmp != s2.cmp then
    invalid_arg "Rb_set.append: comparison functions must be the same";
  { s1 with tree = Rb_set_impl.union s1.cmp s1.tree s2.tree |> Rb_set_impl.blacken }

let compare s1 s2 =
  if s1.cmp != s2.cmp then
    invalid_arg "Rb_set.compare: comparison functions must be the same";
  Rb_set_impl.compare_trees s1.cmp s1.tree s2.tree

let to_list s = Rb_set_impl.to_list s.tree

let of_list cmp xs =
  { cmp; tree = Rb_set_impl.of_list cmp xs |> Rb_set_impl.blacken }

