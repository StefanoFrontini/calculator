open OUnit2
open Product

let tests = "test suite for product" >::: [
  "empty" >:: (fun _ -> assert_equal 1 (product []));
  "singleton" >:: (fun _ -> assert_equal 1 (product [1]));
  "three_elements" >:: (fun _ -> assert_equal 6 (product [1; 2; 3]));
]

let _ = run_test_tt_main tests

