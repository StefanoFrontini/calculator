open OUnit2
open Lib
open Maps

(** [cmp_set_like_lists lst1 lst2] compares two lists to see weather they are equivalent set-like lists. That means checking two things. First, they must both be {ì set-like}, meaning that they do not contain any duplicates. Second, they must contain the same elements, though not necessarily in the same order. *)
let cmp_set_like_lists lst1 lst2 =
  let uniq1 = List.sort_uniq compare lst1 in
  let uniq2 = List.sort_uniq compare lst2 in
  List.length lst1 = List.length uniq1
  && List.length lst2 = List.length uniq2
  && uniq1 = uniq2

(** [pp_string s] pretty-prints string [s]. *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (_ :: _ as t) ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(** [pp_pair pp1 pp2 (a, b)] pretty-prints [(a, b)] using [pp1] for [a] and [pp2] for [b]. *)
let pp_pair pp1 pp2 (a, b) = "(" ^ pp1 a ^ ", " ^ pp2 b ^ ")"

let bindings_test name output input =
  name >:: fun _ ->
  assert_equal output
    (AssocListMap.bindings input)
    ~printer:(pp_list (pp_pair string_of_int pp_string))
    ~cmp:cmp_set_like_lists

let lst1 = [ (3110, "fun") ]
let lst2 = [ (3110, "fun"); (2110, "OO") ]

let assoc_tests =
  let open AssocListMap in
  [
    bindings_test "empty list has no bindings" [] empty;
    bindings_test "singleton list has 1 binding" lst1 (of_list lst1);
    bindings_test "list with 2 bindings" lst2 (of_list lst2);
  ]

let suite = "maps suite" >::: assoc_tests
let _ = run_test_tt_main suite
