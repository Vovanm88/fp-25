(* Lab 3: Output formatting *)

open Interpolation

(* Print a point with method label *)
let print_point method_type point =
  let label = Interpolator.method_name method_type in
  Printf.printf "%s: %.6g %.6g\n" label point.x point.y;
  flush stdout

(* Print a point with custom label *)
let print_point_labeled label point =
  Printf.printf "%s: %.6g %.6g\n" label point.x point.y;
  flush stdout
