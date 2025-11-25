open Alcotest
open Lab2.Rb_set

module A = Alcotest

let int_cmp a b = if a < b then -1 else if a > b then 1 else 0

let test_golden_operations () =
  let s = make int_cmp in
  let s = List.fold_left (fun acc x -> add x acc) s [ 5; 3; 7; 1; 9; 2; 6; 4; 8 ] in
  let result = to_list s in
  let expected = [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ] in
  A.check (list int) "golden operations insert" expected result;
  let s = remove 5 s in
  let result = to_list s in
  let expected = [ 1; 2; 3; 4; 6; 7; 8; 9 ] in
  A.check (list int) "golden operations remove" expected result;
  let s = filter (fun x -> x mod 2 = 0) s in
  let result = to_list s in
  let expected = [ 2; 4; 6; 8 ] in
  A.check (list int) "golden operations filter" expected result;
  let s = map (fun x -> x * 10) int_cmp s in
  let result = to_list s in
  let expected = [ 20; 40; 60; 80 ] in
  A.check (list int) "golden operations map" expected result

let test_golden_folds () =
  let s = of_list int_cmp [ 1; 2; 3; 4; 5 ] in
  let sum_left = fold_left ( + ) 0 s in
  A.check int "golden fold_left sum" 15 sum_left;
  let sum_right = fold_right ( + ) s 0 in
  A.check int "golden fold_right sum" 15 sum_right;
  let concat_left = fold_left (fun acc x -> acc ^ string_of_int x) "" s in
  A.check string "golden fold_left concat" "12345" concat_left;
  let concat_right = fold_right (fun x acc -> string_of_int x ^ acc) s "" in
  let result_list = to_list s in
  let expected_concat = String.concat "" (List.map string_of_int result_list) in
  A.check string "golden fold_right concat" expected_concat concat_right

let test_golden_append () =
  let s1 = of_list int_cmp [ 1; 2; 3 ] in
  let s2 = of_list int_cmp [ 4; 5; 6 ] in
  let s3 = of_list int_cmp [ 7; 8; 9 ] in
  let s = append (append s1 s2) s3 in
  let result = to_list s in
  let expected = [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ] in
  A.check (list int) "golden append chain" expected result;
  let s1 = of_list int_cmp [ 1; 2; 3; 4; 5 ] in
  let s2 = of_list int_cmp [ 3; 4; 5; 6; 7 ] in
  let s = append s1 s2 in
  let result = to_list s in
  let expected = [ 1; 2; 3; 4; 5; 6; 7 ] in
  A.check (list int) "golden append overlap" expected result

let test_golden_compare () =
  let s1 = of_list int_cmp [ 1; 2; 3 ] in
  let s2 = of_list int_cmp [ 1; 2; 3 ] in
  A.check int "golden compare equal" 0 (compare s1 s2);
  let s1 = of_list int_cmp [ 1; 2; 3 ] in
  let s2 = of_list int_cmp [ 1; 2; 4 ] in
  let c = compare s1 s2 in
  A.check bool "golden compare less" true (c < 0);
  let s1 = of_list int_cmp [ 1; 2; 4 ] in
  let s2 = of_list int_cmp [ 1; 2; 3 ] in
  let c = compare s1 s2 in
  A.check bool "golden compare greater" true (c > 0);
  let s1 = of_list int_cmp [ 1; 2; 3; 4 ] in
  let s2 = of_list int_cmp [ 1; 2; 3 ] in
  let c = compare s1 s2 in
  A.check bool "golden compare longer" true (c > 0)

let golden_tests =
  [
    ("operations", `Quick, test_golden_operations);
    ("folds", `Quick, test_golden_folds);
    ("append", `Quick, test_golden_append);
    ("compare", `Quick, test_golden_compare);
  ]

let () = run "Lab2 Golden Tests" [ ("rb-set", golden_tests) ]

