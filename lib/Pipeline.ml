(* Lab 3: Streaming pipeline for interpolation *)

open Interpolation

(* State for tracking interpolation progress per method *)
type method_state = {
  method_type : Interpolator.method_type;
  mutable last_x : float option;
}

(* Create initial state for methods *)
let init_method_states methods =
  List.map (fun m -> { method_type = m; last_x = None }) methods

(* Process a single method: generate x values, interpolate, output *)
let process_method ~step ~window_size ~output state points current_x =
  match
    Interpolator.get_window ~method_type:state.method_type ~window_size points
  with
  | None -> () (* Not enough points yet *)
  | Some window ->
      let start_x =
        match state.last_x with
        | None -> (List.hd window).x +. step
        | Some x -> x +. step
      in
      let end_x = current_x in
      if start_x <= end_x then (
        let x_seq = Generator.generate_x_values ~start_x ~end_x ~step in
        let interpolated =
          Interpolator.interpolate_seq ~method_type:state.method_type ~window
            x_seq
        in
        (* Track the last x value while outputting *)
        let last_x_ref = ref state.last_x in
        Seq.iter
          (fun p ->
            output state.method_type p;
            last_x_ref := Some p.x)
          interpolated;
        state.last_x <- !last_x_ref)

(* Output first point for all methods *)
let output_first_point ~output states point =
  List.iter
    (fun state ->
      output state.method_type point;
      state.last_x <- Some point.x)
    states

(* Process all methods for current points buffer *)
let process_all_methods ~step ~window_size ~output states points current_x =
  List.iter
    (fun state ->
      process_method ~step ~window_size ~output state points current_x)
    states

(* Calculate maximum window size needed *)
let max_window_size ~window_size methods =
  List.fold_left
    (fun acc m ->
      max acc (Interpolator.window_size_for_method ~default_window:window_size m))
    0 methods

(* Output final point if not yet output *)
let output_final_point ~output states last_point =
  List.iter
    (fun state ->
      let should_output =
        match state.last_x with
        | None -> true
        | Some x -> x < last_point.x -. 1e-8
      in
      if should_output then output state.method_type last_point)
    states

(* Streaming processor: reads input, applies interpolation, outputs results *)
let run ~methods ~step ~window_size ~parse_line ~output =
  let states = init_method_states methods in
  let points_buffer = ref [] in
  let max_win = max_window_size ~window_size methods in

  let rec process_line () =
    match read_line () with
    | exception End_of_file -> (
        (* EOF: process remaining points and output final point *)
        match !points_buffer with
        | [] -> ()
        | points ->
            let last_point = List.hd (List.rev points) in
            process_all_methods ~step ~window_size ~output states points
              last_point.x;
            output_final_point ~output states last_point)
    | line -> (
        match parse_line line with
        | None -> process_line ()
        | Some point ->
            points_buffer := !points_buffer @ [ point ];
            (* Output first point immediately *)
            if List.length !points_buffer = 1 then
              output_first_point ~output states point;
            (* Process all methods *)
            process_all_methods ~step ~window_size ~output states !points_buffer
              point.x;
            (* Trim buffer to max window size *)
            if List.length !points_buffer > max_win then
              points_buffer :=
                List.rev
                  (Interpolator.take max_win (List.rev !points_buffer));
            process_line ())
  in
  process_line ()

