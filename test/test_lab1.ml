open Lab1.Task1
open Lab1.Task2

let test_count = ref 0
let pass_count = ref 0
let fail_count = ref 0

let assert_equal msg expected actual =
  incr test_count;
  if expected <> actual then (
    incr fail_count;
    Printf.eprintf "FAIL: %s - expected %d, got %d\n" msg expected actual;
    exit 1
  ) else (
    incr pass_count;
    Printf.printf "PASS: %s\n" msg
  )

let assert_true msg condition =
  incr test_count;
  if not condition then (
    incr fail_count;
    Printf.eprintf "FAIL: %s\n" msg;
    exit 1
  ) else (
    incr pass_count;
    Printf.printf "PASS: %s\n" msg
  )

let test_triangle_number () =
  assert_equal "triangle_number(1)" 1 (triangle_number 1);
  assert_equal "triangle_number(2)" 3 (triangle_number 2);
  assert_equal "triangle_number(3)" 6 (triangle_number 3);
  assert_equal "triangle_number(7)" 28 (triangle_number 7);
  assert_equal "triangle_number(10)" 55 (triangle_number 10)

let test_count_divisors () =
  assert_equal "count_divisors(1)" 1 (count_divisors 1);
  assert_equal "count_divisors(3)" 2 (count_divisors 3);
  assert_equal "count_divisors(6)" 4 (count_divisors 6);
  assert_equal "count_divisors(10)" 4 (count_divisors 10);
  assert_equal "count_divisors(28)" 6 (count_divisors 28)

let test_solve1_small () =
  let result_5 = solve1_tail_rec 5 in
  assert_equal "solve1_tail_rec(5) = 28" 28 result_5;
  
  let result_5_rec = solve1_rec 5 in
  assert_equal "solve1_rec(5) = 28" 28 result_5_rec;
  
  let result_5_modular = solve1_modular 5 in
  assert_equal "solve1_modular(5) = 28" 28 result_5_modular;
  
  let result_5_map = solve1_map 5 in
  assert_equal "solve1_map(5) = 28" 28 result_5_map;
  
  let result_5_for = solve1_for_loop 5 in
  assert_equal "solve1_for_loop(5) = 28" 28 result_5_for;
  
  let result_5_seq = solve1_seq 5 in
  assert_equal "solve1_seq(5) = 28" 28 result_5_seq

let test_solve1_all_implementations () =
  let min_divisors = 5 in
  let expected = 28 in
  
  let result_tail = solve1_tail_rec min_divisors in
  let result_rec = solve1_rec min_divisors in
  let result_modular = solve1_modular min_divisors in
  let result_map = solve1_map min_divisors in
  let result_for = solve1_for_loop min_divisors in
  let result_seq = solve1_seq min_divisors in
  
  assert_equal "All implementations return same result (tail)" expected result_tail;
  assert_equal "All implementations return same result (rec)" expected result_rec;
  assert_equal "All implementations return same result (modular)" expected result_modular;
  assert_equal "All implementations return same result (map)" expected result_map;
  assert_equal "All implementations return same result (for)" expected result_for;
  assert_equal "All implementations return same result (seq)" expected result_seq

let test_solve1_main () =
  let result = solve1_tail_rec 500 in
  assert_equal "solve1_tail_rec(500) = 76576500" 76576500 result

let test_solve2_small_triangle () =
  let small_triangle = [
    [3];
    [7; 4];
    [2; 4; 6];
    [8; 5; 9; 3]
  ] in
  
  let expected = 23 in
  
  let result_tail = solve2_tail_rec small_triangle in
  let result_rec = solve2_rec small_triangle in
  let result_modular = solve2_modular small_triangle in
  let result_map = solve2_map small_triangle in
  let result_for = solve2_for_loop small_triangle in
  let result_seq = solve2_seq small_triangle in
  
  assert_equal "solve2_tail_rec(small) = 23" expected result_tail;
  assert_equal "solve2_rec(small) = 23" expected result_rec;
  assert_equal "solve2_modular(small) = 23" expected result_modular;
  assert_equal "solve2_map(small) = 23" expected result_map;
  assert_equal "solve2_for_loop(small) = 23" expected result_for;
  assert_equal "solve2_seq(small) = 23" expected result_seq

let test_solve2_single_row () =
  let single = [[42]] in
  let expected = 42 in
  
  let result_tail = solve2_tail_rec single in
  let result_rec = solve2_rec single in
  let result_dp = solve2_dp single in
  
  assert_equal "solve2_tail_rec(single) = 42" expected result_tail;
  assert_equal "solve2_rec(single) = 42" expected result_rec;
  assert_equal "solve2_dp(single) = 42" expected result_dp

let test_solve2_two_rows () =
  let two_rows = [
    [1];
    [2; 3]
  ] in
  let expected = 4 in
  
  let result = solve2_dp two_rows in
  assert_equal "solve2_dp(two_rows) = 4" expected result

let test_solve2_all_implementations () =
  let expected = 1074 in
  
  let result_tail = solve2_tail_rec triangle_data in
  let result_rec = solve2_rec triangle_data in
  let result_modular = solve2_modular triangle_data in
  let result_map = solve2_map triangle_data in
  let result_for = solve2_for_loop triangle_data in
  let result_seq = solve2_seq triangle_data in
  let result_dp = solve2_dp triangle_data in
  
  assert_equal "solve2_tail_rec(main) = 1074" expected result_tail;
  assert_equal "solve2_rec(main) = 1074" expected result_rec;
  assert_equal "solve2_modular(main) = 1074" expected result_modular;
  assert_equal "solve2_map(main) = 1074" expected result_map;
  assert_equal "solve2_for_loop(main) = 1074" expected result_for;
  assert_equal "solve2_seq(main) = 1074" expected result_seq;
  assert_equal "solve2_dp(main) = 1074" expected result_dp

let test_solve2_consistency () =
  let small = [
    [1];
    [2; 3];
    [4; 5; 6]
  ] in
  
  let results = [
    solve2_tail_rec small;
    solve2_rec small;
    solve2_modular small;
    solve2_map small;
    solve2_for_loop small;
    solve2_seq small;
    solve2_dp small
  ] in
  
  let first = List.hd results in
  let all_same = List.for_all (fun x -> x = first) results in
  assert_true "All solve2 implementations return same result" all_same

let test_solve1_edge_cases () =
  assert_equal "solve1_tail_rec(1) finds first with >1 divisor" 3 (solve1_tail_rec 1);
  assert_equal "solve1_tail_rec(2) finds first with >2 divisors" 6 (solve1_tail_rec 2);
  assert_equal "solve1_tail_rec(3) finds first with >3 divisors" 6 (solve1_tail_rec 3)

let test_solve2_edge_cases () =
  let one_element = [[100]] in
  assert_equal "solve2_dp(one_element)" 100 (solve2_dp one_element);
  
  let two_elements = [[1]; [2; 3]] in
  assert_equal "solve2_dp(two_elements)" 4 (solve2_dp two_elements);
  
  let all_same = [[1]; [1; 1]; [1; 1; 1]] in
  assert_equal "solve2_dp(all_same)" 3 (solve2_dp all_same)

let test_triangle_number_properties () =
  for n = 1 to 20 do
    let tri = triangle_number n in
    assert_true (Printf.sprintf "triangle_number(%d) >= %d" n n) (tri >= n);
    assert_true (Printf.sprintf "triangle_number(%d) > 0" n) (tri > 0)
  done

let test_count_divisors_properties () =
  for n = 1 to 100 do
    let divs = count_divisors n in
    assert_true (Printf.sprintf "count_divisors(%d) > 0" n) (divs > 0);
    assert_true (Printf.sprintf "count_divisors(%d) <= %d" n n) (divs <= n)
  done

let run_tests () =
  test_count := 0;
  pass_count := 0;
  fail_count := 0;
  
  Printf.printf "\n=== Тесты для задачи 1 ===\n\n";
  test_triangle_number ();
  test_triangle_number_properties ();
  test_count_divisors ();
  test_count_divisors_properties ();
  test_solve1_small ();
  test_solve1_edge_cases ();
  test_solve1_all_implementations ();
  test_solve1_main ();
  
  Printf.printf "\n=== Тесты для задачи 2 ===\n\n";
  test_solve2_small_triangle ();
  test_solve2_single_row ();
  test_solve2_two_rows ();
  test_solve2_edge_cases ();
  test_solve2_all_implementations ();
  test_solve2_consistency ();
  
  Printf.printf "\n=== Результаты тестирования ===\n";
  Printf.printf "Всего тестов: %d\n" !test_count;
  Printf.printf "Пройдено: %d\n" !pass_count;
  Printf.printf "Провалено: %d\n" !fail_count;
  
  if !fail_count = 0 then
    Printf.printf "\n✓ Все тесты успешно пройдены!\n\n"
  else
    (Printf.printf "\n✗ Некоторые тесты провалены!\n\n"; exit 1)

let () = run_tests ()

