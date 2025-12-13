open Interpolation

type interpolation_method = Linear | Lagrange | Newton

let method_name = function
  | Linear -> "linear"
  | Lagrange -> "lagrange"
  | Newton -> "newton"

let interpolate method_ points x =
  match method_ with
  | Linear -> (
      match points with
      | [ p1; p2 ] -> linear_interpolate p1 p2 x
      | _ -> failwith "Linear interpolation requires exactly 2 points")
  | Lagrange -> lagrange_interpolate points x
  | Newton -> newton_interpolate points x

let rec take n lst =
  if n <= 0 then []
  else match lst with [] -> [] | hd :: tl -> hd :: take (n - 1) tl

let linspace start_x end_x step =
  let eps = 1e-8 in
  let rec aux x acc =
    if x < end_x +. eps then aux (x +. step) (x :: acc) else List.rev acc
  in
  aux start_x []

let process_stream ~methods ~step ~window_size ~parse_line ~print_point =
  let points_buffer = ref [] in
  let last_computed_x = ref (List.map (fun m -> (m, None)) methods) in

  let get_last_x method_ =
    List.assoc_opt method_ !last_computed_x |> Option.join
  in

  let set_last_x method_ x =
    last_computed_x :=
      List.map
        (fun (m, _) -> if m = method_ then (m, Some x) else (m, get_last_x m))
        !last_computed_x
  in

  let process_method method_ points current_point =
    let method_window_size =
      match method_ with Linear -> 2 | Lagrange | Newton -> window_size
    in
    let window =
      if method_window_size = 2 then take 2 (List.rev points) |> List.rev
      else take method_window_size points
    in
    if List.length window < method_window_size then ()
    else
      let start_x =
        match get_last_x method_ with
        | None -> (List.hd window).x
        | Some x -> x +. step
      in
      let end_x = current_point.x in
      if start_x <= end_x then (
        let x_values = linspace start_x end_x step in
        List.iter
          (fun x ->
            let y = interpolate method_ window x in
            print_point (method_name method_) { x; y })
          x_values;
        match x_values with
        | [] -> ()
        | _ -> set_last_x method_ (List.hd (List.rev x_values)))
  in

  let rec process_line () =
    match read_line () with
    | exception End_of_file -> (
        match !points_buffer with
        | [] -> ()
        | points ->
            let last_point = List.hd (List.rev points) in
            List.iter
              (fun method_ ->
                process_method method_ points last_point;
                let last_x = get_last_x method_ in
                (* Only print final point if we've produced interpolated values
                   for this method (i.e. last_x is set) and the last printed x
                   is different from the final point's x. This avoids printing
                   the very first point when the window was never full. *)
                match last_x with
                | None -> ()
                | Some x ->
                    if abs_float (x -. last_point.x) > 1e-8 then
                      print_point (method_name method_) last_point)
              methods)
    | line -> (
        match parse_line line with
        | None -> process_line ()
        | Some point ->
            points_buffer := !points_buffer @ [ point ];
            (* Do not print the very first point immediately. Wait until the
               window for each method is filled and interpolation produces
               values. This prevents emitting a single dot at the stream
               start. *)
            ();
            List.iter
              (fun method_ -> process_method method_ !points_buffer point)
              methods;
            let max_window_size =
              List.fold_left
                (fun acc method_ ->
                  match method_ with
                  | Linear -> max acc 2
                  | Lagrange | Newton -> max acc window_size)
                0 methods
            in
            if List.length !points_buffer > max_window_size then
              points_buffer :=
                List.rev (take max_window_size (List.rev !points_buffer));
            process_line ())
  in
  process_line ()
