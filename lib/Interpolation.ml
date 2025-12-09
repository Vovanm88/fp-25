type point = { x : float; y : float }

let linear_interpolate p1 p2 x =
  if p1.x = p2.x then p1.y
  else
    let t = (x -. p1.x) /. (p2.x -. p1.x) in
    p1.y +. (t *. (p2.y -. p1.y))

let lagrange_interpolate points x =
  let n = List.length points in
  let points_list = Array.of_list points in
  let lagrange_basis i =
    let xi = points_list.(i).x in
    let yi = points_list.(i).y in
    let product = ref 1.0 in
    for j = 0 to n - 1 do
      if i <> j then
        let xj = points_list.(j).x in
        product := !product *. ((x -. xj) /. (xi -. xj))
    done;
    !product *. yi
  in
  let sum = ref 0.0 in
  for i = 0 to n - 1 do
    sum := !sum +. lagrange_basis i
  done;
  !sum

let newton_interpolate points x =
  let n = List.length points in
  let points_list = Array.of_list points in
  let dd = Array.make_matrix n n 0.0 in
  for i = 0 to n - 1 do
    dd.(i).(0) <- points_list.(i).y
  done;
  for j = 1 to n - 1 do
    for i = 0 to n - j - 1 do
      let xi = points_list.(i).x in
      let xij = points_list.(i + j).x in
      dd.(i).(j) <- (dd.(i + 1).(j - 1) -. dd.(i).(j - 1)) /. (xij -. xi)
    done
  done;
  let result = ref dd.(0).(n - 1) in
  for i = n - 2 downto 0 do
    result := (!result *. (x -. points_list.(i).x)) +. dd.(0).(i)
  done;
  !result

let generate_points p1 p2 step =
  let rec aux x acc =
    if x >= p2.x then List.rev acc
    else
      let y = linear_interpolate p1 p2 x in
      aux (x +. step) ({ x; y } :: acc)
  in
  aux p1.x []

let generate_points_inclusive p1 p2 step =
  let rec aux x acc =
    if x > p2.x then List.rev acc
    else if x = p2.x then List.rev ({ x = p2.x; y = p2.y } :: acc)
    else
      let y = linear_interpolate p1 p2 x in
      aux (x +. step) ({ x; y } :: acc)
  in
  aux p1.x []
