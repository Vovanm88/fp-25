(* Lab 3: Point generator for interpolation *)

(* Generate sequence of x values from start to end with step *)
let linspace ~start_x ~end_x ~step =
  let eps = 1e-8 in
  Seq.unfold
    (fun x -> if x < end_x +. eps then Some (x, x +. step) else None)
    start_x

(* Generate x values for interpolation in a window *)
let generate_x_values ~start_x ~end_x ~step =
  if start_x > end_x then Seq.empty else linspace ~start_x ~end_x ~step

