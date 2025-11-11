open Lab1.Task1
open Lab1.Task2

let () =
  print_endline "=== Задача 1: Треугольные числа с более чем 500 делителями ===";
  print_newline ();

  let min_divisors = 500 in

  print_endline "1. Хвостовая рекурсия:";
  let start_time = Sys.time () in
  let result1 = solve1_tail_rec min_divisors in
  let end_time = Sys.time () in
  Printf.printf "   Результат: %d (время: %.4f сек)\n" result1
    (end_time -. start_time);
  print_newline ();

  print_endline "2. Обычная рекурсия:";
  let start_time = Sys.time () in
  let result2 = solve1_rec min_divisors in
  let end_time = Sys.time () in
  Printf.printf "   Результат: %d (время: %.4f сек)\n" result2
    (end_time -. start_time);
  print_newline ();

  print_endline "3. Модульная реализация:";
  let start_time = Sys.time () in
  let result3 = solve1_modular min_divisors in
  let end_time = Sys.time () in
  Printf.printf "   Результат: %d (время: %.4f сек)\n" result3
    (end_time -. start_time);
  print_newline ();

  print_endline "4. С использованием map:";
  let start_time = Sys.time () in
  let result4 = solve1_map min_divisors in
  let end_time = Sys.time () in
  Printf.printf "   Результат: %d (время: %.4f сек)\n" result4
    (end_time -. start_time);
  print_newline ();

  print_endline "5. С использованием for-цикла:";
  let start_time = Sys.time () in
  let result5 = solve1_for_loop min_divisors in
  let end_time = Sys.time () in
  Printf.printf "   Результат: %d (время: %.4f сек)\n" result5
    (end_time -. start_time);
  print_newline ();

  print_endline "6. С использованием бесконечных последовательностей (Seq):";
  let start_time = Sys.time () in
  let result6 = solve1_seq min_divisors in
  let end_time = Sys.time () in
  Printf.printf "   Результат: %d (время: %.4f сек)\n" result6
    (end_time -. start_time);
  print_newline ();

  print_endline "=== Задача 2: Максимальная сумма пути в треугольнике ===";
  print_newline ();

  let triangle = triangle_data in

  print_endline "1. Хвостовая рекурсия:";
  let start_time = Sys.time () in
  let result1 = solve2_tail_rec triangle in
  let end_time = Sys.time () in
  Printf.printf "   Результат: %d (время: %.4f сек)\n" result1
    (end_time -. start_time);
  print_newline ();

  print_endline "2. Обычная рекурсия:";
  let start_time = Sys.time () in
  let result2 = solve2_rec triangle in
  let end_time = Sys.time () in
  Printf.printf "   Результат: %d (время: %.4f сек)\n" result2
    (end_time -. start_time);
  print_newline ();

  print_endline "3. Модульная реализация:";
  let start_time = Sys.time () in
  let result3 = solve2_modular triangle in
  let end_time = Sys.time () in
  Printf.printf "   Результат: %d (время: %.4f сек)\n" result3
    (end_time -. start_time);
  print_newline ();

  print_endline "4. С использованием map:";
  let start_time = Sys.time () in
  let result4 = solve2_map triangle in
  let end_time = Sys.time () in
  Printf.printf "   Результат: %d (время: %.4f сек)\n" result4
    (end_time -. start_time);
  print_newline ();

  print_endline "5. С использованием for-цикла:";
  let start_time = Sys.time () in
  let result5 = solve2_for_loop triangle in
  let end_time = Sys.time () in
  Printf.printf "   Результат: %d (время: %.4f сек)\n" result5
    (end_time -. start_time);
  print_newline ();

  print_endline "6. С использованием бесконечных последовательностей (Seq):";
  let start_time = Sys.time () in
  let result6 = solve2_seq triangle in
  let end_time = Sys.time () in
  Printf.printf "   Результат: %d (время: %.4f сек)\n" result6
    (end_time -. start_time);
  print_newline ()
