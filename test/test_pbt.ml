open Alcotest
open QCheck
open Lab2.Rb_set

let int_cmp a b = if a < b then -1 else if a > b then 1 else 0
let gen_int_list = Gen.(list_size (int_range 0 20) (int_range (-100) 100))

let gen_set =
  Gen.(
    gen_int_list >>= fun (lst : int list) ->
    let unique_lst = List.sort_uniq Stdlib.compare lst in
    return (of_list int_cmp unique_lst))

let arb_set = QCheck.make gen_set

let prop_monoid_left_unit s =
  let empty_set = make int_cmp in
  let s' = append empty_set s in
  compare s s' = 0

let prop_monoid_right_unit s =
  let empty_set = make int_cmp in
  let s' = append s empty_set in
  compare s s' = 0

let prop_monoid_assoc s1 s2 s3 =
  let s12_3 = append (append s1 s2) s3 in
  let s1_23 = append s1 (append s2 s3) in
  compare s12_3 s1_23 = 0

let prop_add_idempotent x s =
  let s1 = add x (add x s) in
  let s2 = add x s in
  compare s1 s2 = 0

let prop_append_commutative s1 s2 =
  let s12 = append s1 s2 in
  let s21 = append s2 s1 in
  let lst1 = List.sort Stdlib.compare (to_list s12) in
  let lst2 = List.sort Stdlib.compare (to_list s21) in
  lst1 = lst2

let prop_filter_true s =
  let s' = filter (fun _ -> true) s in
  compare s s' = 0

let prop_filter_false s =
  let s' = filter (fun _ -> false) s in
  let empty_set = make int_cmp in
  compare s' empty_set = 0

let prop_map_identity s =
  let s' = map (fun x -> x) int_cmp s in
  compare s s' = 0

let test_monoid_left_unit () =
  let test =
    Test.make ~name:"monoid left unit" ~count:1000 arb_set prop_monoid_left_unit
  in
  let result = QCheck_runner.run_tests [ test ] in
  if result <> 0 then Alcotest.fail "monoid left unit property failed"

let test_monoid_right_unit () =
  let test =
    Test.make ~name:"monoid right unit" ~count:1000 arb_set
      prop_monoid_right_unit
  in
  let result = QCheck_runner.run_tests [ test ] in
  if result <> 0 then Alcotest.fail "monoid right unit property failed"

let test_monoid_associativity () =
  let test =
    Test.make ~name:"monoid associativity" ~count:1000
      (QCheck.make (Gen.triple gen_set gen_set gen_set))
      (fun (s1, s2, s3) -> prop_monoid_assoc s1 s2 s3)
  in
  let result = QCheck_runner.run_tests [ test ] in
  if result <> 0 then Alcotest.fail "monoid associativity property failed"

let test_add_idempotent () =
  let test =
    Test.make ~name:"add idempotent" ~count:1000
      (QCheck.make (Gen.pair (Gen.int_range (-100) 100) gen_set))
      (fun (x, s) -> prop_add_idempotent x s)
  in
  let result = QCheck_runner.run_tests [ test ] in
  if result <> 0 then Alcotest.fail "add idempotent property failed"

let test_append_commutative () =
  let test =
    Test.make ~name:"append commutative" ~count:1000
      (QCheck.make (Gen.pair gen_set gen_set))
      (fun (s1, s2) -> prop_append_commutative s1 s2)
  in
  let result = QCheck_runner.run_tests [ test ] in
  if result <> 0 then Alcotest.fail "append commutative property failed"

let test_filter_true () =
  let test =
    Test.make ~name:"filter true" ~count:1000 arb_set prop_filter_true
  in
  let result = QCheck_runner.run_tests [ test ] in
  if result <> 0 then Alcotest.fail "filter true property failed"

let test_filter_false () =
  let test =
    Test.make ~name:"filter false" ~count:1000 arb_set prop_filter_false
  in
  let result = QCheck_runner.run_tests [ test ] in
  if result <> 0 then Alcotest.fail "filter false property failed"

let test_map_identity () =
  let test =
    Test.make ~name:"map identity" ~count:1000 arb_set prop_map_identity
  in
  let result = QCheck_runner.run_tests [ test ] in
  if result <> 0 then Alcotest.fail "map identity property failed"

let property_tests =
  [
    ("monoid left unit", `Quick, test_monoid_left_unit);
    ("monoid right unit", `Quick, test_monoid_right_unit);
    ("monoid associativity", `Quick, test_monoid_associativity);
    ("add idempotent", `Quick, test_add_idempotent);
    ("append commutative", `Quick, test_append_commutative);
    ("filter true", `Quick, test_filter_true);
    ("filter false", `Quick, test_filter_false);
    ("map identity", `Quick, test_map_identity);
  ]

let () =
  run "Lab2 Property-Based Tests" [ ("rb-set properties", property_tests) ]
