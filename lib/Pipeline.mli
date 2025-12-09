(* Lab 3: Streaming pipeline for interpolation *)

open Interpolation

(** State for tracking interpolation progress per method *)
type method_state

(** Calculate maximum window size needed across all methods *)
val max_window_size : window_size:int -> Interpolator.method_type list -> int

(** Streaming processor: reads input, applies interpolation, outputs results
    
    @param methods List of interpolation methods to use
    @param step Step size for generating x values
    @param window_size Default window size for Lagrange/Newton
    @param parse_line Function to parse input line into point
    @param output Function to output interpolated point *)
val run :
  methods:Interpolator.method_type list ->
  step:float ->
  window_size:int ->
  parse_line:(string -> point option) ->
  output:(Interpolator.method_type -> point -> unit) ->
  unit

