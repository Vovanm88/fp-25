open Alcotest
open Lab2.Rb_set

module A = Alcotest

let int_cmp a b = if a < b then -1 else if a > b then 1 else 0

let test_empty () =
  let s = make int_cmp in
  A.check A.bool "empty to_list" true ((to_list s) = []);
  A.check A.int "empty fold_left" 0 (fold_left ( + ) 0 s)

let test_add () =
  let s = make int_cmp in
  let s = add 1 s in
  let s = add 2 s in
  let s = add 3 s in
  let lst = to_list s in
  A.check A.bool "add elements" true (lst = [ 1; 2; 3 ]);
  let s = add 2 s in
  let lst = to_list s in
  A.check A.bool "add duplicate" true (lst = [ 1; 2; 3 ])

let test_remove () =
  let s = of_list int_cmp [ 1; 2; 3; 4; 5 ] in
  let s = remove 3 s in
  let lst = to_list s in
  A.check A.bool "remove middle" true (lst = [ 1; 2; 4; 5 ]);
  let s = remove 1 s in
  let lst = to_list s in
  A.check A.bool "remove first" true (lst = [ 2; 4; 5 ]);
  let s = remove 5 s in
  let lst = to_list s in
  A.check A.bool "remove last" true (lst = [ 2; 4 ]);
  let s = remove 10 s in
  let lst = to_list s in
  A.check A.bool "remove non-existent" true (lst = [ 2; 4 ])

let test_filter () =
  let s = of_list int_cmp [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] in
  let s = filter (fun x -> x mod 2 = 0) s in
  let lst = to_list s in
  A.check A.bool "filter even" true (lst = [ 2; 4; 6; 8; 10 ]);
  let s = filter (fun _ -> true) s in
  let lst = to_list s in
  A.check A.bool "filter all" true (lst = [ 2; 4; 6; 8; 10 ]);
  let s = filter (fun _ -> false) s in
  let lst = to_list s in
  A.check A.bool "filter none" true (lst = [])

let test_map () =
  let s = of_list int_cmp [ 1; 2; 3 ] in
  let s = map (fun x -> x * 2) int_cmp s in
  let lst = to_list s in
  A.check A.bool "map double" true (lst = [ 2; 4; 6 ])

let test_fold_left () =
  let s = of_list int_cmp [ 1; 2; 3; 4; 5 ] in
  let sum = fold_left ( + ) 0 s in
  A.check A.int "fold_left sum" 15 sum;
  let product = fold_left ( * ) 1 s in
  A.check A.int "fold_left product" 120 product

let test_fold_right () =
  let s = of_list int_cmp [ 1; 2; 3; 4; 5 ] in
  let sum = fold_right ( + ) s 0 in
  A.check A.int "fold_right sum" 15 sum;
  let diff = fold_right ( - ) s 0 in
  A.check A.int "fold_right diff" 3 diff

let test_append () =
  let s1 = of_list int_cmp [ 1; 2; 3 ] in
  let s2 = of_list int_cmp [ 4; 5; 6 ] in
  let s = append s1 s2 in
  let lst = to_list s in
  A.check A.bool "append disjoint" true (lst = [ 1; 2; 3; 4; 5; 6 ]);
  let s1 = of_list int_cmp [ 1; 2; 3 ] in
  let s2 = of_list int_cmp [ 2; 3; 4 ] in
  let s = append s1 s2 in
  let lst = to_list s in
  A.check A.bool "append overlap" true (lst = [ 1; 2; 3; 4 ])

let test_compare () =
  let s1 = of_list int_cmp [ 1; 2; 3 ] in
  let s2 = of_list int_cmp [ 1; 2; 3 ] in
  A.check A.int "compare equal" 0 (compare s1 s2);
  let s1 = of_list int_cmp [ 1; 2; 3 ] in
  let s2 = of_list int_cmp [ 1; 2; 4 ] in
  let c = compare s1 s2 in
  A.check A.bool "compare less" true (c < 0);
  let s1 = of_list int_cmp [ 1; 2; 4 ] in
  let s2 = of_list int_cmp [ 1; 2; 3 ] in
  let c = compare s1 s2 in
  A.check A.bool "compare greater" true (c > 0)

let unit_tests =
  [
    ("empty", `Quick, test_empty);
    ("add", `Quick, test_add);
    ("remove", `Quick, test_remove);
    ("filter", `Quick, test_filter);
    ("map", `Quick, test_map);
    ("fold_left", `Quick, test_fold_left);
    ("fold_right", `Quick, test_fold_right);
    ("append", `Quick, test_append);
    ("compare", `Quick, test_compare);
  ]

let () = run "Lab2 Unit Tests" [ ("rb-set", unit_tests) ]

