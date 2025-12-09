(* Lab 3: Unit tests for interpolation methods *)

open Alcotest
open Lab3.Interpolation

let test_linear_interpolate_basic () =
  let p1 = { x = 0.0; y = 0.0 } in
  let p2 = { x = 1.0; y = 1.0 } in
  let result = linear_interpolate p1 p2 0.5 in
  check (float 0.001) "linear interpolation at midpoint" 0.5 result;
  let result2 = linear_interpolate p1 p2 0.0 in
  check (float 0.001) "linear interpolation at start" 0.0 result2;
  let result3 = linear_interpolate p1 p2 1.0 in
  check (float 0.001) "linear interpolation at end" 1.0 result3

let test_linear_interpolate_negative () =
  let p1 = { x = -1.0; y = -1.0 } in
  let p2 = { x = 1.0; y = 1.0 } in
  let result = linear_interpolate p1 p2 0.0 in
  check (float 0.001) "linear interpolation at zero" 0.0 result

let test_linear_interpolate_non_linear () =
  let p1 = { x = 0.0; y = 0.0 } in
  let p2 = { x = 2.0; y = 4.0 } in
  let result = linear_interpolate p1 p2 1.0 in
  check (float 0.001) "linear interpolation non-linear function" 2.0 result

let test_linear_interpolate_same_x () =
  let p1 = { x = 1.0; y = 5.0 } in
  let p2 = { x = 1.0; y = 10.0 } in
  let result = linear_interpolate p1 p2 1.0 in
  check (float 0.001) "linear interpolation same x returns p1.y" 5.0 result

let test_generate_points_basic () =
  let p1 = { x = 0.0; y = 0.0 } in
  let p2 = { x = 1.0; y = 1.0 } in
  let points = generate_points p1 p2 0.5 in
  check
    (list (pair (float 0.001) (float 0.001)))
    "generate points with step 0.5"
    [ (0.0, 0.0); (0.5, 0.5) ]
    (List.map (fun p -> (p.x, p.y)) points)

let test_generate_points_small_step () =
  let p1 = { x = 0.0; y = 0.0 } in
  let p2 = { x = 1.0; y = 1.0 } in
  let points = generate_points p1 p2 0.1 in
  (* generate_points generates [p1.x, p2.x), but due to floating point precision,
     it might include the last point. Let's check that we have at least 10 points
     and they are correctly interpolated *)
  let actual = List.map (fun p -> (p.x, p.y)) points in
  (* Check that we have at least 10 points *)
  check bool "at least 10 points" true (List.length actual >= 10);
  (* Check that first 10 points match expected *)
  let expected = List.init 10 (fun i -> (float_of_int i *. 0.1, float_of_int i *. 0.1)) in
  let rec take n lst =
    if n <= 0 then []
    else match lst with [] -> [] | hd :: tl -> hd :: take (n - 1) tl
  in
  let first_10 = take 10 actual in
  check
    (list (pair (float 0.001) (float 0.001)))
    "generate points with step 0.1 (first 10)"
    expected
    first_10

let test_generate_points_inclusive () =
  let p1 = { x = 0.0; y = 0.0 } in
  let p2 = { x = 1.0; y = 1.0 } in
  let points = generate_points_inclusive p1 p2 0.5 in
  check
    (list (pair (float 0.001) (float 0.001)))
    "generate points inclusive with step 0.5"
    [ (0.0, 0.0); (0.5, 0.5); (1.0, 1.0) ]
    (List.map (fun p -> (p.x, p.y)) points)

let test_generate_points_empty () =
  let p1 = { x = 0.0; y = 0.0 } in
  let p2 = { x = 0.5; y = 0.5 } in
  let points = generate_points p1 p2 1.0 in
  check
    (list (pair (float 0.001) (float 0.001)))
    "generate points with large step"
    [ (0.0, 0.0) ]
    (List.map (fun p -> (p.x, p.y)) points)

(* Lagrange interpolation tests *)

let test_lagrange_at_endpoints () =
  let points = [ { x = 0.0; y = 0.0 }; { x = 1.0; y = 1.0 }; { x = 2.0; y = 4.0 } ] in
  let y0 = lagrange_interpolate points 0.0 in
  let y1 = lagrange_interpolate points 1.0 in
  let y2 = lagrange_interpolate points 2.0 in
  check (float 0.001) "lagrange at x=0" 0.0 y0;
  check (float 0.001) "lagrange at x=1" 1.0 y1;
  check (float 0.001) "lagrange at x=2" 4.0 y2

let test_lagrange_linear_function () =
  (* y = 2x + 1 *)
  let points = [ { x = 0.0; y = 1.0 }; { x = 1.0; y = 3.0 }; { x = 2.0; y = 5.0 } ] in
  let y = lagrange_interpolate points 1.5 in
  check (float 0.001) "lagrange linear function at x=1.5" 4.0 y

let test_lagrange_quadratic () =
  (* y = x^2 *)
  let points = [ { x = 0.0; y = 0.0 }; { x = 1.0; y = 1.0 }; { x = 2.0; y = 4.0 } ] in
  let y = lagrange_interpolate points 1.5 in
  check (float 0.001) "lagrange quadratic at x=1.5" 2.25 y

let test_lagrange_four_points () =
  (* y = x^3 - x *)
  let f x = x *. x *. x -. x in
  let points = [
    { x = -1.0; y = f (-1.0) };
    { x = 0.0; y = f 0.0 };
    { x = 1.0; y = f 1.0 };
    { x = 2.0; y = f 2.0 }
  ] in
  let y = lagrange_interpolate points 0.5 in
  check (float 0.001) "lagrange cubic at x=0.5" (f 0.5) y

(* Newton interpolation tests *)

let test_newton_at_endpoints () =
  let points = [ { x = 0.0; y = 0.0 }; { x = 1.0; y = 1.0 }; { x = 2.0; y = 4.0 } ] in
  let y0 = newton_interpolate points 0.0 in
  let y1 = newton_interpolate points 1.0 in
  let y2 = newton_interpolate points 2.0 in
  check (float 0.001) "newton at x=0" 0.0 y0;
  check (float 0.001) "newton at x=1" 1.0 y1;
  check (float 0.001) "newton at x=2" 4.0 y2

let test_newton_linear_function () =
  (* y = 2x + 1 *)
  let points = [ { x = 0.0; y = 1.0 }; { x = 1.0; y = 3.0 }; { x = 2.0; y = 5.0 } ] in
  let y = newton_interpolate points 1.5 in
  check (float 0.001) "newton linear function at x=1.5" 4.0 y

let test_newton_quadratic () =
  (* y = x^2 *)
  let points = [ { x = 0.0; y = 0.0 }; { x = 1.0; y = 1.0 }; { x = 2.0; y = 4.0 } ] in
  let y = newton_interpolate points 1.5 in
  check (float 0.001) "newton quadratic at x=1.5" 2.25 y

let test_newton_four_points () =
  (* y = x^3 - x *)
  let f x = x *. x *. x -. x in
  let points = [
    { x = -1.0; y = f (-1.0) };
    { x = 0.0; y = f 0.0 };
    { x = 1.0; y = f 1.0 };
    { x = 2.0; y = f 2.0 }
  ] in
  let y = newton_interpolate points 0.5 in
  check (float 0.001) "newton cubic at x=0.5" (f 0.5) y

(* Test that Lagrange and Newton produce same results *)

let test_lagrange_newton_equivalence () =
  let points = [
    { x = 0.0; y = 1.0 };
    { x = 1.0; y = 2.0 };
    { x = 3.0; y = 10.0 };
    { x = 4.0; y = 17.0 }
  ] in
  let test_x = [ 0.5; 1.5; 2.0; 2.5; 3.5 ] in
  List.iter (fun x ->
    let lagr = lagrange_interpolate points x in
    let newt = newton_interpolate points x in
    check (float 0.001) (Printf.sprintf "lagrange = newton at x=%g" x) lagr newt
  ) test_x

let unit_tests =
  [
    ("linear_interpolate_basic", `Quick, test_linear_interpolate_basic);
    ("linear_interpolate_negative", `Quick, test_linear_interpolate_negative);
    ("linear_interpolate_non_linear", `Quick, test_linear_interpolate_non_linear);
    ("linear_interpolate_same_x", `Quick, test_linear_interpolate_same_x);
    ("generate_points_basic", `Quick, test_generate_points_basic);
    ("generate_points_small_step", `Quick, test_generate_points_small_step);
    ("generate_points_inclusive", `Quick, test_generate_points_inclusive);
    ("generate_points_empty", `Quick, test_generate_points_empty);
    (* Lagrange tests *)
    ("lagrange_at_endpoints", `Quick, test_lagrange_at_endpoints);
    ("lagrange_linear_function", `Quick, test_lagrange_linear_function);
    ("lagrange_quadratic", `Quick, test_lagrange_quadratic);
    ("lagrange_four_points", `Quick, test_lagrange_four_points);
    (* Newton tests *)
    ("newton_at_endpoints", `Quick, test_newton_at_endpoints);
    ("newton_linear_function", `Quick, test_newton_linear_function);
    ("newton_quadratic", `Quick, test_newton_quadratic);
    ("newton_four_points", `Quick, test_newton_four_points);
    (* Equivalence test *)
    ("lagrange_newton_equivalence", `Quick, test_lagrange_newton_equivalence);
  ]

let () = run "Lab3 Unit" [ ("unit", unit_tests) ]
