open Interpolation

let print_point label point =
  Printf.printf "%s: %.6g %.6g\n" label point.x point.y;
  flush stdout
