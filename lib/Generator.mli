(* Lab 3: Point generator for interpolation *)

(** Generate sequence of x values from start to end with step *)
val linspace : start_x:float -> end_x:float -> step:float -> float Seq.t

(** Generate x values for interpolation in a window *)
val generate_x_values : start_x:float -> end_x:float -> step:float -> float Seq.t

