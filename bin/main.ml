(* Lab 2: Red-Black Tree Set implementation *)

module Rb_set = Lab2.Rb_set

let int_cmp a b = if a < b then -1 else if a > b then 1 else 0

let () =
  print_endline "Lab 2: Red-Black Tree Set";
  print_endline "";

  (* Создаём пустое множество *)
  let s = Rb_set.make int_cmp in

  (* Добавляем элементы *)
  let s = Rb_set.add 5 s in
  let s = Rb_set.add 3 s in
  let s = Rb_set.add 7 s in
  let s = Rb_set.add 1 s in
  let s = Rb_set.add 9 s in
  let s = Rb_set.add 16 s in

  (* Преобразуем в список и выводим *)
  let elements = Rb_set.to_list s in
  print_string "Множество после добавления элементов: ";
  List.iter (fun x -> print_string (string_of_int x ^ " ")) elements;
  print_endline "";

  (* Удаляем элемент *)
  let s = Rb_set.remove 3 s in
  let elements = Rb_set.to_list s in
  print_string "Множество после удаления 3: ";
  List.iter (fun x -> print_string (string_of_int x ^ " ")) elements;
  print_endline "";

  (* Фильтруем только чётные числа *)
  let s = Rb_set.filter (fun x -> x mod 2 = 0) s in
  let elements = Rb_set.to_list s in
  print_string "Только чётные числа: ";
  List.iter (fun x -> print_string (string_of_int x ^ " ")) elements;
  print_endline "";

  (* Создаём множество из списка *)
  let s2 = Rb_set.of_list int_cmp [ 10; 20; 30; 40 ] in
  let elements = Rb_set.to_list s2 in
  print_string "Множество из списка: ";
  List.iter (fun x -> print_string (string_of_int x ^ " ")) elements;
  print_endline "";

  (* Объединяем два множества *)
  let s3 = Rb_set.append s s2 in
  let elements = Rb_set.to_list s3 in
  print_string "Объединение множеств: ";
  List.iter (fun x -> print_string (string_of_int x ^ " ")) elements;
  print_endline "";

  (* Вычисляем сумму элементов *)
  let sum = Rb_set.fold_left ( + ) 0 s3 in
  print_string "Сумма всех элементов: ";
  print_endline (string_of_int sum)
