(* Lab 3: Interpolation methods wrapper *)

open Interpolation

(** Available interpolation methods *)
type method_type = Linear | Lagrange | Newton

(** Get method name as string *)
val method_name : method_type -> string

(** Get required window size for a method *)
val window_size_for_method : default_window:int -> method_type -> int

(** Take first n elements from list *)
val take : int -> 'a list -> 'a list

(** Get window of points for interpolation, returns None if not enough points *)
val get_window :
  method_type:method_type -> window_size:int -> point list -> point list option

(** Apply interpolation method to a window of points at x *)
val interpolate : method_type:method_type -> window:point list -> float -> float

(** Interpolate a sequence of x values, producing points *)
val interpolate_seq :
  method_type:method_type -> window:point list -> float Seq.t -> point Seq.t

