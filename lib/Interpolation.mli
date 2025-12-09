type point = { x : float; y : float }

val linear_interpolate : point -> point -> float -> float
val lagrange_interpolate : point list -> float -> float
val newton_interpolate : point list -> float -> float
val generate_points : point -> point -> float -> point list
val generate_points_inclusive : point -> point -> float -> point list
