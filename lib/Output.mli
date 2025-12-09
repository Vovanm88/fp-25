(* Lab 3: Output formatting *)

open Interpolation

(** Print a point with method label *)
val print_point : Interpolator.method_type -> point -> unit

(** Print a point with custom label *)
val print_point_labeled : string -> point -> unit
