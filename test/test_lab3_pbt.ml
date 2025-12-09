open Alcotest
open Lab3.Interpolation
module QCheck = QCheck2

let float_gen min max =
  QCheck.Gen.map
    (fun x -> min +. ((max -. min) *. (Float.of_int x /. 1000.0)))
    (QCheck.Gen.int_range 0 1000)

let point_gen =
  QCheck.Gen.(
    map2
      (fun x y -> { x; y })
      (float_gen (-100.0) 100.0) (float_gen (-100.0) 100.0))

let points_list_gen =
  QCheck.Gen.(
    sized (fun size ->
        let n = min 10 (max 2 ((size / 5) + 2)) in
        let rec build_unique_points acc remaining min_x =
          if remaining <= 0 then return (List.rev acc)
          else
            let x = float_gen (min_x +. 0.1) (min_x +. 50.0) in
            let y = float_gen (-100.0) 100.0 in
            x >>= fun x_val ->
            y >>= fun y_val ->
            let new_point = { x = x_val; y = y_val } in
            build_unique_points (new_point :: acc) (remaining - 1) x_val
        in
        let first_x = float_gen (-100.0) 50.0 in
        first_x >>= fun start_x ->
        let first_y = float_gen (-100.0) 100.0 in
        first_y >>= fun start_y ->
        build_unique_points [ { x = start_x; y = start_y } ] (n - 1) start_x))

let test_linear_interpolate_at_endpoints p1 p2 =
  if p1.x <> p2.x then
    let y1 = linear_interpolate p1 p2 p1.x in
    let y2 = linear_interpolate p1 p2 p2.x in
    let eps = 0.0001 in
    abs_float (y1 -. p1.y) < eps && abs_float (y2 -. p2.y) < eps
  else true

let test_linear_interpolate_monotonic p1 p2 =
  if p1.x < p2.x then
    let x1 = p1.x +. ((p2.x -. p1.x) *. 0.25) in
    let x2 = p1.x +. ((p2.x -. p1.x) *. 0.75) in
    let y1 = linear_interpolate p1 p2 x1 in
    let y2 = linear_interpolate p1 p2 x2 in
    if p1.y <= p2.y then y1 <= y2 else y1 >= y2
  else true

let test_generate_points_ordered p1 p2 step =
  if p1.x < p2.x && step > 0.0 then
    let points = generate_points p1 p2 step in
    match points with
    | [] -> true
    | hd :: _ ->
        let first_ok = abs_float (hd.x -. p1.x) < 0.0001 in
        let xs = List.map (fun p -> p.x) points in
        let rec is_sorted = function
          | [] | [ _ ] -> true
          | a :: b :: rest -> a <= b && is_sorted (b :: rest)
        in
        first_ok && is_sorted xs
  else true

let test_generate_points_in_range p1 p2 step =
  if p1.x < p2.x && step > 0.0 then
    let points = generate_points p1 p2 step in
    List.for_all (fun p -> p.x >= p1.x && p.x < p2.x) points
  else true

let test_generate_points_interpolated p1 p2 step =
  if p1.x < p2.x && step > 0.0 then
    let points = generate_points p1 p2 step in
    List.for_all
      (fun p ->
        let expected_y = linear_interpolate p1 p2 p.x in
        abs_float (p.y -. expected_y) < 0.0001)
      points
  else true

let test_lagrange_interpolate_at_points points =
  if List.length points >= 2 then
    let eps = 0.0001 in
    List.for_all
      (fun p ->
        let result = lagrange_interpolate points p.x in
        (not (Float.is_nan result))
        && (not (Float.is_infinite result))
        && abs_float (result -. p.y) < eps)
      points
  else true

let test_lagrange_interpolate_two_points p1 p2 x =
  if p1.x <> p2.x && x >= min p1.x p2.x && x <= max p1.x p2.x then
    let lagrange_result = lagrange_interpolate [ p1; p2 ] x in
    let linear_result = linear_interpolate p1 p2 x in
    let eps = 0.0001 in
    (not (Float.is_nan lagrange_result))
    && (not (Float.is_infinite lagrange_result))
    && abs_float (lagrange_result -. linear_result) < eps
  else true

let test_newton_interpolate_at_points points =
  if List.length points >= 2 then
    let eps = 0.0001 in
    List.for_all
      (fun p ->
        let result = newton_interpolate points p.x in
        (not (Float.is_nan result))
        && (not (Float.is_infinite result))
        && abs_float (result -. p.y) < eps)
      points
  else true

let test_newton_interpolate_two_points p1 p2 x =
  if p1.x <> p2.x && x >= min p1.x p2.x && x <= max p1.x p2.x then
    let newton_result = newton_interpolate [ p1; p2 ] x in
    let linear_result = linear_interpolate p1 p2 x in
    let eps = 0.0001 in
    (not (Float.is_nan newton_result))
    && (not (Float.is_infinite newton_result))
    && abs_float (newton_result -. linear_result) < eps
  else true

let test_lagrange_newton_equivalence points x =
  if List.length points >= 2 then
    let sorted_points =
      List.sort (fun p1 p2 -> Float.compare p1.x p2.x) points
    in
    let all_unique =
      let rec check_unique = function
        | [] | [ _ ] -> true
        | a :: b :: rest -> a.x <> b.x && check_unique (b :: rest)
      in
      check_unique sorted_points
    in
    if all_unique then
      let lagrange_result = lagrange_interpolate sorted_points x in
      let newton_result = newton_interpolate sorted_points x in
      let eps = 0.0001 in
      (not (Float.is_nan lagrange_result))
      && (not (Float.is_infinite lagrange_result))
      && (not (Float.is_nan newton_result))
      && (not (Float.is_infinite newton_result))
      && abs_float (lagrange_result -. newton_result) < eps
    else true
  else true

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
          (QCheck.Test.make ~count:1000
             ~name:"linear_interpolate preserves monotonicity" gen
             (fun (p1, p2) -> test_linear_interpolate_monotonic p1 p2)) );
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
             gen (fun (p1, p2, step) -> test_generate_points_ordered p1 p2 step))
    );
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
             (fun (p1, p2, step) -> test_generate_points_in_range p1 p2 step))
    );
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
             (fun (p1, p2, step) ->
               test_generate_points_interpolated p1 p2 step)) );
    ( "lagrange_interpolate_at_points",
      `Quick,
      fun () ->
        QCheck.Test.check_exn
          (QCheck.Test.make ~count:500
             ~name:"lagrange_interpolate passes through all given points"
             points_list_gen test_lagrange_interpolate_at_points) );
    ( "lagrange_interpolate_two_points",
      `Quick,
      fun () ->
        let gen =
          QCheck.Gen.(
            triple point_gen point_gen (float_gen (-100.0) 100.0)
            |> map (fun (p1, p2, x) ->
                let min_x = min p1.x p2.x in
                let max_x = max p1.x p2.x in
                let clamped_x = max min_x (min max_x x) in
                (p1, p2, clamped_x)))
        in
        QCheck.Test.check_exn
          (QCheck.Test.make ~count:1000
             ~name:
               "lagrange_interpolate matches linear_interpolate for two points"
             gen (fun (p1, p2, x) ->
               test_lagrange_interpolate_two_points p1 p2 x)) );
    ( "newton_interpolate_at_points",
      `Quick,
      fun () ->
        QCheck.Test.check_exn
          (QCheck.Test.make ~count:500
             ~name:"newton_interpolate passes through all given points"
             points_list_gen test_newton_interpolate_at_points) );
    ( "newton_interpolate_two_points",
      `Quick,
      fun () ->
        let gen =
          QCheck.Gen.(
            triple point_gen point_gen (float_gen (-100.0) 100.0)
            |> map (fun (p1, p2, x) ->
                let min_x = min p1.x p2.x in
                let max_x = max p1.x p2.x in
                let clamped_x = max min_x (min max_x x) in
                (p1, p2, clamped_x)))
        in
        QCheck.Test.check_exn
          (QCheck.Test.make ~count:1000
             ~name:
               "newton_interpolate matches linear_interpolate for two points"
             gen (fun (p1, p2, x) -> test_newton_interpolate_two_points p1 p2 x))
    );
    ( "lagrange_newton_equivalence",
      `Quick,
      fun () ->
        let gen =
          QCheck.Gen.(
            pair points_list_gen (float_gen (-150.0) 150.0)
            |> map (fun (points, x) ->
                let sorted_points =
                  List.sort (fun p1 p2 -> Float.compare p1.x p2.x) points
                in
                (sorted_points, x)))
        in
        QCheck.Test.check_exn
          (QCheck.Test.make ~count:500
             ~name:
               "lagrange_interpolate and newton_interpolate give same results"
             gen (fun (points, x) -> test_lagrange_newton_equivalence points x))
    );
  ]

let () = run "Lab3 PBT" [ ("pbt", pbt_tests) ]
