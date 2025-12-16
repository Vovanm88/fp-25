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

let linspace start_x end_x step =
  let eps = 1e-8 in
  let rec aux x acc =
    if x < end_x +. eps then aux (x +. step) (x :: acc) else List.rev acc
  in
  aux start_x []

let process_stream ~methods ~step ~window_size ~parse_line ~print_point =
  let points_buffer = ref [] in
  (* Отслеживаем последнюю обработанную x для каждого метода *)
  let last_processed_x = ref (List.map (fun m -> (m, None)) methods) in

  let get_last_x method_ =
    List.assoc_opt method_ !last_processed_x |> Option.join
  in

  let set_last_x method_ x =
    last_processed_x :=
      List.map
        (fun (m, _) -> if m = method_ then (m, Some x) else (m, get_last_x m))
        !last_processed_x
  in

  (* Линейный поиск ближайшей точки слева *)
  let find_center_index x points =
    let rec aux idx best_idx =
      if idx >= List.length points then best_idx
      else
        let point = List.nth points idx in
        if point.x <= x then aux (idx + 1) idx else best_idx
    in
    aux 0 0
  in

  let select_window x_i points method_window_size is_eof =
    if List.length points < method_window_size then None
    else
      let center_idx = find_center_index x_i points in
      let n = method_window_size in
      let half_n = n / 2 in
      let ideal_start, ideal_end =
        if n mod 2 = 0 then (center_idx - (half_n - 1), center_idx + half_n)
        else (center_idx - half_n, center_idx + half_n)
      in
      (* Есть 8 вариантов:
      1. Не EOF и центрированное окно
      2. Не EOF и справа не хватает точек
      3. Не EOF и слева не хватает точек
      4. Не EOF и справа и слева не хватает точек
      5. EOF и центрированное окно
      6. EOF и справа не хватает точек
      7. EOF и слева не хватает точек
      8. EOF и справа и слева не хватает точек
      *)
      (* отсекаем варианты где нет EOF и справа не хватает точек, 2, 4*)
      if
        (not is_eof)
        && (ideal_end >= List.length points || List.length points < n)
      then None
      else
        let window_start_idx, window_end_idx =
          (* вариант где всего хватает, 1 и 5*)
          if ideal_start >= 0 && ideal_end < List.length points then
            (ideal_start, ideal_end)
            (* вариант где справа не хватает точек, тут EOF так что берем что есть, 6 и 8*)
          else if ideal_end >= List.length points then
            (max 0 (List.length points - n), List.length points - 1)
          else
            (* варианты где слева не хватает точек, 3 и 7*)
            (0, min (List.length points - 1) (n - 1))
        in
        (* Убеждаемся, что окно имеет правильный размер *)
        let final_start_idx, final_end_idx =
          let actual_size = window_end_idx - window_start_idx + 1 in
          if actual_size = n then (window_start_idx, window_end_idx)
          else if actual_size < n then
            (* Окно меньше нужного - используем последние n точек (только при EOF) *)
            (max 0 (List.length points - n), List.length points - 1)
          else
            (* Окно больше нужного - обрезаем до n точек *)
            (window_start_idx, window_start_idx + n - 1)
        in

        (* Извлекаем окно точек *)
        let window =
          let rec extract_window start_idx end_idx acc current_idx =
            if current_idx > end_idx then List.rev acc
            else if current_idx >= List.length points then List.rev acc
            else
              extract_window start_idx end_idx
                (List.nth points current_idx :: acc)
                (current_idx + 1)
          in
          extract_window final_start_idx final_end_idx [] final_start_idx
        in

        if List.length window = method_window_size then Some window else None
  in

  (* Обрабатывает точки на решетке интерполяции для метода *)
  let process_interpolation_points method_ points is_eof =
    let method_window_size =
      match method_ with Linear -> 2 | Lagrange | Newton -> window_size
    in

    if List.length points < method_window_size then ()
    else
      let first_point = List.hd points in
      let last_point = List.hd (List.rev points) in
      let start_x = first_point.x in
      let end_x = last_point.x in

      let process_start_x =
        match get_last_x method_ with None -> start_x | Some x -> x +. step
      in

      let interpolation_x_values = linspace process_start_x end_x step in

      List.iter
        (fun x_i ->
          match select_window x_i points method_window_size is_eof with
          | None -> ()
          | Some window ->
              let y = interpolate method_ window x_i in
              print_point (method_name method_) { x = x_i; y };
              set_last_x method_ x_i)
        interpolation_x_values;

      if is_eof then
        let last_x = get_last_x method_ in
        match last_x with
        | None -> ()
        | Some x ->
            if abs_float (x -. last_point.x) > 1e-8 then
              print_point (method_name method_) last_point
  in

  let rec process_line () =
    match read_line () with
    | exception End_of_file ->
        if !points_buffer <> [] then
          List.iter
            (fun method_ ->
              process_interpolation_points method_ !points_buffer true)
            methods
    | line -> (
        match parse_line line with
        | None -> process_line ()
        | Some point ->
            points_buffer := !points_buffer @ [ point ];
            List.iter
              (fun method_ ->
                process_interpolation_points method_ !points_buffer false)
              methods;
            process_line ())
  in
  process_line ()
