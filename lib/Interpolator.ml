(* Lab 3: Interpolation methods wrapper *)

open Interpolation

(* Available interpolation methods *)
type method_type = Linear | Lagrange | Newton

(* Get method name as string *)
let method_name = function
  | Linear -> "linear"
  | Lagrange -> "lagrange"
  | Newton -> "newton"

(* Get required window size for a method *)
let window_size_for_method ~default_window = function
  | Linear -> 2
  | Lagrange | Newton -> default_window

(* Take first n elements from list *)
let rec take n lst =
  if n <= 0 then []
  else match lst with [] -> [] | hd :: tl -> hd :: take (n - 1) tl

(* Get window of points for interpolation *)
let get_window ~method_type ~window_size points =
  let required_size = window_size_for_method ~default_window:window_size method_type in
  let window =
    if required_size = 2 then
      (* For linear: take last 2 points *)
      take 2 (List.rev points) |> List.rev
    else
      (* For Lagrange/Newton: take first n points *)
      take required_size points
  in
  if List.length window < required_size then None else Some window

(* Apply interpolation method to a window of points at x *)
let interpolate ~method_type ~window x =
  match method_type with
  | Linear -> (
      match window with
      | [ p1; p2 ] -> linear_interpolate p1 p2 x
      | _ -> failwith "Linear interpolation requires exactly 2 points")
  | Lagrange -> lagrange_interpolate window x
  | Newton -> newton_interpolate window x

(* Interpolate a sequence of x values, producing (x, y) points *)
let interpolate_seq ~method_type ~window x_seq =
  Seq.map
    (fun x ->
      let y = interpolate ~method_type ~window x in
      { x; y })
    x_seq

