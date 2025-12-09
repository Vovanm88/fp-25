open Interpolation

type interpolation_method = Linear | Lagrange | Newton

val process_stream :
  methods:interpolation_method list ->
  step:float ->
  window_size:int ->
  parse_line:(string -> point option) ->
  print_point:(string -> point -> unit) ->
  unit
