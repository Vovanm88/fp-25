(* Lab 3: Input parsing *)

open Interpolation

(* Parse a line in format "x y" or "x;y" or "x\ty" *)
val parse_line : string -> point option

