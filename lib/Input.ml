(* Lab 3: Input parsing *)

open Interpolation

(* Parse a line in format "x y" or "x;y" or "x\ty" *)
let parse_line line =
  try
    let parts =
      if String.contains line ';' then String.split_on_char ';' line
      else if String.contains line '\t' then String.split_on_char '\t' line
      else String.split_on_char ' ' line
    in
    (* Filter out empty strings from multiple consecutive separators *)
    let parts = List.filter (fun s -> String.trim s <> "") parts in
    match parts with
    | [ x_str; y_str ] ->
        let x = float_of_string (String.trim x_str) in
        let y = float_of_string (String.trim y_str) in
        Some { x; y }
    | _ -> None
  with
  | _ -> None

