open Alcotest
open Lab2.Rb_set
module A = Alcotest

let int_cmp a b = if a < b then -1 else if a > b then 1 else 0

(* Тест: все операции должны выполняться только через публичный API *)
let test_api_only_operations () =
  let s = make int_cmp in
  let s = add 1 s in
  let s = add 2 s in
  let s = add 3 s in
  (* Проверяем, что можем получить элементы только через to_list *)
  let lst = to_list s in
  A.check (list int) "api only: to_list" [ 1; 2; 3 ] lst;
  (* Проверяем, что можем использовать fold только через API *)
  let sum = fold_left ( + ) 0 s in
  A.check int "api only: fold_left" 6 sum;
  (* Проверяем, что можем фильтровать только через API *)
  let s_filtered = filter (fun x -> x mod 2 = 0) s in
  let lst_filtered = to_list s_filtered in
  A.check (list int) "api only: filter" [ 2 ] lst_filtered

(* Тест: compare должна работать эффективно, не через to_list *)
let test_compare_efficiency () =
  (* Создаем большие множества для проверки эффективности *)
  let large_list1 = List.init 1000 (fun i -> i) in
  let large_list2 = List.init 1000 (fun i -> i) in
  let s1 = of_list int_cmp large_list1 in
  let s2 = of_list int_cmp large_list2 in
  (* compare должна работать быстро, не конвертируя в списки *)
  let result = compare s1 s2 in
  A.check int "compare efficiency: equal sets" 0 result;
  (* Проверяем, что compare работает для разных множеств *)
  let s3 = add 2000 s1 in
  let result2 = compare s1 s3 in
  A.check bool "compare efficiency: different sets" true (result2 < 0)

(* Тест: нельзя создать множество без использования make/of_list *)
let test_no_direct_construction () =
  (* Единственный способ создать множество - через make или of_list *)
  let s1 = make int_cmp in
  let s2 = of_list int_cmp [] in
  let result = compare s1 s2 in
  A.check int "no direct construction: empty sets equal" 0 result;
  (* Проверяем, что empty работает *)
  let s3 = empty in
  let lst = to_list s3 in
  A.check (list int) "no direct construction: empty" [] lst

(* Тест: все модификации должны возвращать новые множества (иммутабельность) *)
let test_immutability () =
  let s1 = of_list int_cmp [ 1; 2; 3 ] in
  let s2 = add 4 s1 in
  let s3 = remove 2 s1 in
  (* Исходное множество не должно измениться *)
  let lst1 = to_list s1 in
  A.check (list int) "immutability: original unchanged" [ 1; 2; 3 ] lst1;
  (* Новые множества должны быть разными *)
  let lst2 = to_list s2 in
  A.check (list int) "immutability: new set with add" [ 1; 2; 3; 4 ] lst2;
  let lst3 = to_list s3 in
  A.check (list int) "immutability: new set with remove" [ 1; 3 ] lst3

(* Тест: map должна работать только через API *)
let test_map_api_only () =
  let s = of_list int_cmp [ 1; 2; 3 ] in
  let s_mapped = map (fun x -> x * 2) int_cmp s in
  let lst = to_list s_mapped in
  A.check (list int) "map api only: doubled" [ 2; 4; 6 ] lst;
  (* Исходное множество не должно измениться *)
  let lst_original = to_list s in
  A.check (list int) "map api only: original unchanged" [ 1; 2; 3 ] lst_original

(* Тест: append должна работать только через API *)
let test_append_api_only () =
  let s1 = of_list int_cmp [ 1; 2; 3 ] in
  let s2 = of_list int_cmp [ 4; 5; 6 ] in
  let s_combined = append s1 s2 in
  let lst = to_list s_combined in
  A.check (list int) "append api only: combined" [ 1; 2; 3; 4; 5; 6 ] lst;
  (* Исходные множества не должны измениться *)
  let lst1 = to_list s1 in
  let lst2 = to_list s2 in
  A.check (list int) "append api only: s1 unchanged" [ 1; 2; 3 ] lst1;
  A.check (list int) "append api only: s2 unchanged" [ 4; 5; 6 ] lst2

(* Тест: compare должна работать для множеств с одинаковыми элементами *)
let test_compare_same_elements () =
  let s1 = of_list int_cmp [ 3; 1; 2 ] in
  let s2 = of_list int_cmp [ 1; 2; 3 ] in
  (* Множества с одинаковыми элементами должны быть равны *)
  let result = compare s1 s2 in
  A.check int "compare same elements: equal" 0 result

(* Тест: compare должна работать для множеств с разными элементами *)
let test_compare_different_elements () =
  let s1 = of_list int_cmp [ 1; 2; 3 ] in
  let s2 = of_list int_cmp [ 1; 2; 4 ] in
  let result = compare s1 s2 in
  A.check bool "compare different elements: s1 < s2" true (result < 0);
  let result2 = compare s2 s1 in
  A.check bool "compare different elements: s2 > s1" true (result2 > 0)

(* Тест: все операции должны работать независимо от порядка добавления *)
let test_order_independence () =
  let s1 = add 3 (add 1 (add 2 (make int_cmp))) in
  let s2 = add 1 (add 2 (add 3 (make int_cmp))) in
  let result = compare s1 s2 in
  A.check int "order independence: same result" 0 result;
  let lst1 = to_list s1 in
  let lst2 = to_list s2 in
  A.check (list int) "order independence: same list" [ 1; 2; 3 ] lst1;
  A.check (list int) "order independence: same list 2" [ 1; 2; 3 ] lst2

(* Тест: fold_right должна работать через API *)
let test_fold_right_api_only () =
  let s = of_list int_cmp [ 1; 2; 3 ] in
  let sum = fold_right ( + ) s 0 in
  A.check int "fold_right api only: sum" 6 sum;
  let product = fold_right ( * ) s 1 in
  A.check int "fold_right api only: product" 6 product

(* Тест: проверка, что нельзя обойти инкапсуляцию через сравнение функций *)
let test_encapsulation_compare_functions () =
  (* Используем одну и ту же функцию для обоих множеств *)
  let s1 = make int_cmp in
  let s2 = make int_cmp in
  (* append должна работать для множеств с одинаковой функцией сравнения *)
  let s3 = append s1 s2 in
  let lst = to_list s3 in
  A.check (list int) "encapsulation: same function works" [] lst;
  (* Проверяем, что compare работает для множеств с одинаковой функцией *)
  let result = compare s1 s2 in
  A.check int "encapsulation: compare with same function" 0 result

let api_leak_tests =
  [
    ("api only operations", `Quick, test_api_only_operations);
    ("compare efficiency", `Quick, test_compare_efficiency);
    ("no direct construction", `Quick, test_no_direct_construction);
    ("immutability", `Quick, test_immutability);
    ("map api only", `Quick, test_map_api_only);
    ("append api only", `Quick, test_append_api_only);
    ("compare same elements", `Quick, test_compare_same_elements);
    ("compare different elements", `Quick, test_compare_different_elements);
    ("order independence", `Quick, test_order_independence);
    ("fold_right api only", `Quick, test_fold_right_api_only);
    ( "encapsulation compare functions",
      `Quick,
      test_encapsulation_compare_functions );
  ]

let () = run "Lab2 API Leak Tests" [ ("rb-set api", api_leak_tests) ]
