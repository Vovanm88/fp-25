(* Lab 3: Interpolation methods *)

type point = { x : float; y : float }

(* Linear interpolation between two points *)
val linear_interpolate : point -> point -> float -> float

(* Lagrange interpolation for n points *)
val lagrange_interpolate : point list -> float -> float

(* Newton interpolation using divided differences for n points *)
val newton_interpolate : point list -> float -> float

(* Generate interpolated points for a segment [p1, p2) with given step *)
val generate_points : point -> point -> float -> point list

(* Generate points for the last segment [p1, p2] including the endpoint *)
val generate_points_inclusive : point -> point -> float -> point list

