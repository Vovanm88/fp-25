(* Lab 3: Property-Based Tests for interpolation methods *)

open Alcotest
open Lab3.Interpolation

module QCheck = QCheck2

let float_gen min max =
  QCheck.Gen.map
    (fun x -> min +. ((max -. min) *. (Float.of_int x /. 1000.0)))
    (QCheck.Gen.int_range 0 1000)

let point_gen =
  QCheck.Gen.(
    map2 (fun x y -> { x; y }) (float_gen (-100.0) 100.0) (float_gen (-100.0) 100.0))

(* Generate n distinct sorted x values and corresponding points *)
let sorted_points_gen n =
  QCheck.Gen.(
    let xs_gen = 
      list_repeat n (float_gen (-50.0) 50.0)
      |> map (fun xs ->
        let sorted = List.sort_uniq compare xs in
        (* Ensure we have exactly n points by adding small offsets if needed *)
        let rec ensure_n acc xs count =
          if count >= n then List.rev acc |> fun l -> List.filteri (fun i _ -> i < n) l
          else match xs with
          | [] -> 
            let last = if acc = [] then 0.0 else List.hd acc in
            ensure_n ((last +. 0.1 *. float_of_int (n - count)) :: acc) [] (count + 1)
          | x :: rest -> ensure_n (x :: acc) rest (count + 1)
        in
        ensure_n [] sorted 0 |> List.sort compare)
    in
    let ys_gen = list_repeat n (float_gen (-100.0) 100.0) in
    map2 (fun xs ys -> 
      List.map2 (fun x y -> { x; y }) xs ys
    ) xs_gen ys_gen)

let test_linear_interpolate_at_endpoints p1 p2 =
  if p1.x <> p2.x then (
    let y1 = linear_interpolate p1 p2 p1.x in
    let y2 = linear_interpolate p1 p2 p2.x in
    let eps = 0.0001 in
    abs_float (y1 -. p1.y) < eps && abs_float (y2 -. p2.y) < eps)
  else true

let test_linear_interpolate_monotonic p1 p2 =
  if p1.x < p2.x then (
    let x1 = p1.x +. ((p2.x -. p1.x) *. 0.25) in
    let x2 = p1.x +. ((p2.x -. p1.x) *. 0.75) in
    let y1 = linear_interpolate p1 p2 x1 in
    let y2 = linear_interpolate p1 p2 x2 in
    (* If function is monotonic, y1 and y2 should be in correct order *)
    if p1.y <= p2.y then y1 <= y2 else y1 >= y2)
  else true

let test_generate_points_ordered p1 p2 step =
  if p1.x < p2.x && step > 0.0 then (
    let points = generate_points p1 p2 step in
    match points with
    | [] -> true
    | hd :: _ ->
        (* Check that first point is p1 *)
        let first_ok = abs_float (hd.x -. p1.x) < 0.0001 in
        (* Check that points are sorted *)
        let xs = List.map (fun p -> p.x) points in
        let rec is_sorted = function
          | [] | [ _ ] -> true
          | a :: b :: rest -> a <= b && is_sorted (b :: rest)
        in
        first_ok && is_sorted xs)
  else true

let test_generate_points_in_range p1 p2 step =
  if p1.x < p2.x && step > 0.0 then (
    let points = generate_points p1 p2 step in
    List.for_all
      (fun p -> p.x >= p1.x && p.x < p2.x)
      points)
  else true

let test_generate_points_interpolated p1 p2 step =
  if p1.x < p2.x && step > 0.0 then (
    let points = generate_points p1 p2 step in
    List.for_all
      (fun p ->
        let expected_y = linear_interpolate p1 p2 p.x in
        abs_float (p.y -. expected_y) < 0.0001)
      points)
  else true

(* Lagrange PBT tests *)

(* Property: Lagrange interpolation passes through all given points *)
let test_lagrange_passes_through_points points =
  if List.length points < 2 then true
  else
    let eps = 0.001 in
    List.for_all (fun p ->
      let y = lagrange_interpolate points p.x in
      abs_float (y -. p.y) < eps
    ) points

(* Property: For linear function y = ax + b, Lagrange gives exact result *)
let test_lagrange_exact_for_linear a b points x =
  if List.length points < 2 then true
  else
    let linear_points = List.map (fun p -> { x = p.x; y = a *. p.x +. b }) points in
    let y = lagrange_interpolate linear_points x in
    let expected = a *. x +. b in
    abs_float (y -. expected) < 0.001

(* Newton PBT tests *)

(* Property: Newton interpolation passes through all given points *)
let test_newton_passes_through_points points =
  if List.length points < 2 then true
  else
    let eps = 0.001 in
    List.for_all (fun p ->
      let y = newton_interpolate points p.x in
      abs_float (y -. p.y) < eps
    ) points

(* Property: For linear function y = ax + b, Newton gives exact result *)
let test_newton_exact_for_linear a b points x =
  if List.length points < 2 then true
  else
    let linear_points = List.map (fun p -> { x = p.x; y = a *. p.x +. b }) points in
    let y = newton_interpolate linear_points x in
    let expected = a *. x +. b in
    abs_float (y -. expected) < 0.001

(* Property: Lagrange and Newton give same results *)
let test_lagrange_newton_equal points x =
  if List.length points < 2 then true
  else
    let lagr = lagrange_interpolate points x in
    let newt = newton_interpolate points x in
    abs_float (lagr -. newt) < 0.001

(* Property: For quadratic y = x^2, 3 points give exact interpolation *)
let test_quadratic_exact_with_3_points points x =
  if List.length points < 3 then true
  else
    let quad_points = List.map (fun p -> { x = p.x; y = p.x *. p.x }) points in
    let three_points = List.filteri (fun i _ -> i < 3) quad_points in
    let y_lagr = lagrange_interpolate three_points x in
    let y_newt = newton_interpolate three_points x in
    let expected = x *. x in
    abs_float (y_lagr -. expected) < 0.01 && abs_float (y_newt -. expected) < 0.01

let pbt_tests =
  [
    ( "linear_interpolate_at_endpoints",
      `Quick,
      fun () ->
        let gen = QCheck.Gen.pair point_gen point_gen in
        QCheck.Test.check_exn
          (QCheck.Test.make ~count:1000
             ~name:"linear_interpolate returns correct values at endpoints" gen
             (fun (p1, p2) -> test_linear_interpolate_at_endpoints p1 p2)) );
    ( "linear_interpolate_monotonic",
      `Quick,
      fun () ->
        let gen = QCheck.Gen.pair point_gen point_gen in
        QCheck.Test.check_exn
          (QCheck.Test.make ~count:1000 ~name:"linear_interpolate preserves monotonicity"
             gen (fun (p1, p2) -> test_linear_interpolate_monotonic p1 p2)) );
    ( "generate_points_ordered",
      `Quick,
      fun () ->
        let gen =
          QCheck.Gen.(
            triple point_gen point_gen (float_gen 0.001 10.0)
            |> map (fun (p1, p2, step) ->
                   if p1.x < p2.x then (p1, p2, step) else (p2, p1, step)))
        in
        QCheck.Test.check_exn
          (QCheck.Test.make ~count:1000
             ~name:"generate_points produces ordered points starting from p1.x"
             gen (fun (p1, p2, step) -> test_generate_points_ordered p1 p2 step)) );
    ( "generate_points_in_range",
      `Quick,
      fun () ->
        let gen =
          QCheck.Gen.(
            triple point_gen point_gen (float_gen 0.001 10.0)
            |> map (fun (p1, p2, step) ->
                   if p1.x < p2.x then (p1, p2, step) else (p2, p1, step)))
        in
        QCheck.Test.check_exn
          (QCheck.Test.make ~count:1000
             ~name:"generate_points produces points in range [p1.x, p2.x)" gen
             (fun (p1, p2, step) -> test_generate_points_in_range p1 p2 step)) );
    ( "generate_points_interpolated",
      `Quick,
      fun () ->
        let gen =
          QCheck.Gen.(
            triple point_gen point_gen (float_gen 0.001 10.0)
            |> map (fun (p1, p2, step) ->
                   if p1.x < p2.x then (p1, p2, step) else (p2, p1, step)))
        in
        QCheck.Test.check_exn
          (QCheck.Test.make ~count:1000
             ~name:"generate_points produces correctly interpolated points" gen
             (fun (p1, p2, step) -> test_generate_points_interpolated p1 p2 step)) );
    (* Lagrange PBT tests *)
    ( "lagrange_passes_through_points",
      `Quick,
      fun () ->
        let gen = sorted_points_gen 4 in
        QCheck.Test.check_exn
          (QCheck.Test.make ~count:500
             ~name:"lagrange passes through all given points" gen
             test_lagrange_passes_through_points) );
    ( "lagrange_exact_for_linear",
      `Quick,
      fun () ->
        let gen = QCheck.Gen.(
          quad (float_gen (-10.0) 10.0) (float_gen (-10.0) 10.0) 
               (sorted_points_gen 3) (float_gen (-50.0) 50.0))
        in
        QCheck.Test.check_exn
          (QCheck.Test.make ~count:500
             ~name:"lagrange is exact for linear functions" gen
             (fun (a, b, points, x) -> test_lagrange_exact_for_linear a b points x)) );
    (* Newton PBT tests *)
    ( "newton_passes_through_points",
      `Quick,
      fun () ->
        let gen = sorted_points_gen 4 in
        QCheck.Test.check_exn
          (QCheck.Test.make ~count:500
             ~name:"newton passes through all given points" gen
             test_newton_passes_through_points) );
    ( "newton_exact_for_linear",
      `Quick,
      fun () ->
        let gen = QCheck.Gen.(
          quad (float_gen (-10.0) 10.0) (float_gen (-10.0) 10.0) 
               (sorted_points_gen 3) (float_gen (-50.0) 50.0))
        in
        QCheck.Test.check_exn
          (QCheck.Test.make ~count:500
             ~name:"newton is exact for linear functions" gen
             (fun (a, b, points, x) -> test_newton_exact_for_linear a b points x)) );
    (* Equivalence test *)
    ( "lagrange_newton_equal",
      `Quick,
      fun () ->
        let gen = QCheck.Gen.(pair (sorted_points_gen 4) (float_gen (-50.0) 50.0)) in
        QCheck.Test.check_exn
          (QCheck.Test.make ~count:500
             ~name:"lagrange and newton give same results" gen
             (fun (points, x) -> test_lagrange_newton_equal points x)) );
    (* Quadratic exactness test *)
    ( "quadratic_exact_with_3_points",
      `Quick,
      fun () ->
        let gen = QCheck.Gen.(pair (sorted_points_gen 3) (float_gen (-10.0) 10.0)) in
        QCheck.Test.check_exn
          (QCheck.Test.make ~count:500
             ~name:"quadratic function exact with 3 points" gen
             (fun (points, x) -> test_quadratic_exact_with_3_points points x)) );
  ]

let () = run "Lab3 PBT" [ ("pbt", pbt_tests) ]

