[@@@ocaml.warnings "-34-32"]

let num = 7 * (1 + 2 + 3);;

num |> string_of_int |> print_endline;

print_endline ("CS " ^ string_of_int 3110)
;;

42 * 10 |> string_of_int |> print_endline;;
3.14 /. 2.0 |> string_of_float |> print_endline;;
4.2 ** 7. |> string_of_float |> print_endline;;
42 = 42 |> string_of_bool |> print_endline;;
"hi" = "hi" |> string_of_bool |> print_endline;;
"hi" == "hi" |> string_of_bool |> print_endline;;
assert (42 == 42);;
assert (2110 <> 3110);;
if 2 > 1 then print_endline "42" else print_endline "7";;

let double x = x * 2 in
7 |> double |> string_of_int |> print_endline;
assert (double 7 = 14)
;;

let cube x = x ** 3. in
7. |> cube |> string_of_float |> print_endline;
assert (cube 7. = 343.)
;;

let sign x = if x > 0 then 1 else if x < 0 then -1 else 0 in
4 |> sign |> string_of_int |> print_endline
;;

let circleArea r = 3.14 *. (r ** 2.) in
2. |> circleArea |> string_of_float |> print_endline;
assert (circleArea 2. = 12.56)

let rootMeanSquare x y = sqrt (((x ** 2.) +. (y ** 2.)) /. 2.);;

rootMeanSquare 3. 4. |> string_of_float |> print_endline

let () =
  assert (
    rootMeanSquare 3. 4. < 3.53553390593 +. 0.00001
    || rootMeanSquare 3. 4. > 3.53553390593 -. 0.00001)

let validDate d m =
  if
    m = "Jan" || m = "Mar" || m = "May" || m = "Jul" || m = "Aug" || m = "Oct"
    || m = "Dec"
  then 1 <= d && d <= 31
  else if m = "Apr" || m = "Jun" || m = "Sep" || m = "Nov" then
    1 <= d && d <= 30
  else if m = "Feb" then 1 <= d && d <= 28
  else false
;;

validDate 28 "Feb" |> string_of_bool |> print_endline

let rec fib n = if n <= 1 then 1 else fib (n - 1) + fib (n - 2)
let rec h n pp p = if n = 0 then p else h (n - 1) p (pp + p)
let fib_fast n = h n 0 1;;

fib 1 |> string_of_int |> print_endline;;
fib_fast 4 |> string_of_int |> print_endline

let divide (numerator : float) (denominator : float) = numerator /. denominator
;;

divide 3.0 2.0 |> string_of_float |> print_endline

let ( +/. ) (x : float) (y : float) : float = (x +. y) /. 2.;;

16.0 +/. 2.0 |> string_of_float |> print_endline

let rec sum lst = match lst with [] -> 0 | h :: t -> h + sum t;;

sum [ 1; 2; 3 ] |> string_of_int |> print_endline

let get_val o = match o with None -> failwith "??" | Some x -> x

let rec list_max (lst : 'a list) : 'a option =
  match lst with
  | [] -> None
  | h :: t -> (
      match list_max t with None -> Some h | Some m -> Some (max h m))

(* #trace list_max;; *)
let () = list_max [ 1; 2; 30 ] |> get_val |> string_of_int |> print_endline

(* let aList = [1;2;3;4;5] *)
(* let aList2 = 1 :: 2 :: 3 :: 4 :: 5 :: [] *)
let aList3 = [ 1 ] @ [ 2; 3; 4 ] @ [ 5 ]

(* alist3 |> string_of_int |> print_endline *)
let rec print_list = function
  | [] -> ()
  | h :: t ->
      print_int h;
      print_string " ";
      print_list t
;;

print_list aList3;;
print_endline ""

let rec print_int_list = function
  | [] -> ()
  | h :: t ->
      print_endline (string_of_int h);
      print_int_list t

let rec product = function [] -> 1 | h :: t -> h * product t
let () = product [ 1; 2; 3 ] |> string_of_int |> print_endline
let rec concat = function [] -> "" | h :: t -> h ^ concat t
let () = concat [ "a"; "b"; "c" ] |> print_endline
let isBigRed = function h :: _ -> h = "bigred" | _ -> false
let () = isBigRed [ "bigred" ] |> string_of_bool |> print_endline
let rec list_length = function [] -> 0 | _ :: t -> 1 + list_length t

(* let twoOrFour lst list_length = match list_length lst with
   | m -> m = 2 || m = 4 *)

let twoOrFour lst list_length = list_length lst = 2 || list_length lst = 4
let () = twoOrFour [ "a"; "b" ] list_length |> string_of_bool |> print_endline

let twoOrFourB = function
  | [ _; _ ] -> true
  | [ _; _; _; _ ] -> true
  | _ -> false

let concatStr str1 str2 = str1 ^ ":" ^ " " ^ str2
(* let concatTwoOrFourB = concatStr "twoOrFourB" *)

let () =
  twoOrFourB [ "a"; "b"; "c"; "d" ]
  |> string_of_bool |> concatStr "twoOrFourB" |> print_endline

let firstTwoEqual = function h1 :: h2 :: _ -> h1 = h2 | _ -> false
let () = firstTwoEqual [ "a"; "a" ] |> string_of_bool |> print_endline
(* let getFifthElement lst = match List.length lst with
   | m -> if m >= 5 then List.nth lst 4 else 0 *)

let getFifthElement lst = if List.length lst >= 5 then List.nth lst 4 else 0

let () =
  getFifthElement [ 1; 2; 3; 4; 20; 30 ] |> string_of_int |> print_endline

let sortDescendingOrder lst = List.sort Stdlib.compare lst |> List.rev
let () = sortDescendingOrder [ 10; 7; 33; 400 ] |> print_list;;

print_endline ""

let getLastElement lst = List.nth lst (List.length lst - 1)
let () = getLastElement [ 1; 2; 3; 4; 20; 30 ] |> string_of_int |> print_endline
let any_zeros lst = List.exists (fun x -> x = 0) lst
let () = any_zeros [ 0; 1; 2; 3 ] |> string_of_bool |> print_endline

let rec take n lst =
  match List.length lst with
  | m -> (
      if m < n then lst
      else
        match n with
        | 0 -> []
        | num -> ( match lst with h :: t -> h :: take (num - 1) t | [] -> []))

let () = take 10 [ 10; 2; 3; 4; 5 ] |> print_list;;

print_endline ""

let rec drop n lst =
  match List.length lst with
  | m -> (
      if m < n then []
      else
        match n with
        | 0 -> lst
        | num -> ( match lst with _ :: t -> drop (num - 1) t | [] -> []))

let () = drop 2 [ 10; 2; 3; 4; 5 ] |> print_list;;

print_endline ""

(* let rec from i j l = if i > j then l else from i (j - 1) (j :: l) *)

(** [i -- j] is the list containing the integers from [i] to [j], inclusive. *)
(* let ( -- ) i j = from i j [] *)

(* let long_list = 0 -- 1_000_000 *)

(* let () = drop 20000 long_list |> print_list *)
(* let () = take 200000 long_list |> print_list *)

let rec list_max2 = function [] -> 0 | h :: t -> max h (list_max2 t)
let () = list_max2 [ 1; 2; 3; 40; 27 ] |> string_of_int |> print_endline

let rec isIncreasing = function
  | [] -> true
  | [ _ ] -> true
  | h1 :: h2 :: t -> h1 <= h2 && isIncreasing (h2 :: t)

let () = isIncreasing [ 1; 2; 3; 40 ] |> string_of_bool |> print_endline

let rec isDecreasing = function
  | [] -> true
  | [ _ ] -> true
  | h1 :: h2 :: t -> h1 >= h2 && isDecreasing (h2 :: t)

let () = isDecreasing [ 27 ] |> string_of_bool |> print_endline

let findIndex f lst =
  List.find_index f lst |> fun x -> match x with Some x -> x | None -> -1

(* let () = findIndex (fun x -> x = 40) [1;2;3; 40; 27] |> print_endline;; *)

let rec divideListInTwo index = function
  | [] -> ([], [])
  | h :: t ->
      if index = 0 then ([], h :: t)
      else
        let a, b = divideListInTwo (index - 1) t in
        (h :: a, b)

let () =
  divideListInTwo 2 [ 1; 2; 3; 40; 27 ] |> fun tuple ->
  match tuple with a, _ -> print_int_list a

let is_unimodal lst =
  match lst with
  | [] -> true
  | _ :: [] -> true
  | _ :: _ :: _ -> (
      match list_max2 lst with
      | m ->
          findIndex (fun x -> x = m) lst |> fun i ->
          divideListInTwo i lst |> fun (a, b) ->
          isIncreasing a && isDecreasing b)

let () = is_unimodal [ 1; 2; 3; 40; 27; 26 ] |> string_of_bool |> print_endline

let rec powerset = function
  | [] -> [ [] ]
  | x :: s ->
      let p = powerset s in
      List.map (List.cons x) p @ p

(* let rec powerset: int list -> int list list = function
   | [] -> [[]]
   | [h] -> [[h]]
   | h :: t -> let p = powerset t in List.map( fun x -> h :: x) p @ [h] :: p *)

let printListOfLists lst =
  List.iter
    (fun x ->
      print_list x;
      print_endline "")
    lst

let () = powerset [ 1; 2; 3 ] |> printListOfLists

type student = { first_name : string; last_name : string; gpa : float }

let aStudent = { first_name = "John"; last_name = "Doe"; gpa = 3.0 }
let studentName student = (student.first_name, student.last_name)
let () = studentName aStudent |> fst |> print_endline
let () = studentName aStudent |> snd |> print_endline
let () = aStudent.gpa |> string_of_float |> print_endline
let createStudent first_name last_name gpa = { first_name; last_name; gpa }
let () = createStudent "Jane" "Dow" 4.0 |> studentName |> fst |> print_endline

type poketype = Normal | Fire | Water
type pokemon = { name : string; hp : int; ptype : poketype }

let charizard = { name = "Charizard"; hp = 78; ptype = Fire }

let () =
  charizard.ptype |> fun x ->
  (match x with Normal -> "Normal" | Fire -> "Fire" | Water -> "Water")
  |> print_endline

let squirtle = { name = "Squirtle"; hp = 44; ptype = Water }
let normalPokemon = { name = "Pikachu"; hp = 35; ptype = Normal }

let () =
  normalPokemon.ptype |> fun x ->
  (match x with Normal -> "Normal" | Fire -> "Fire" | Water -> "Water")
  |> print_endline

let safe_hd = function [] -> None | h :: _ -> Some h
let safe_tl = function [] -> None | _ :: t -> Some t
let () = safe_tl [ 1; 2; 3 ] |> get_val |> print_int_list;;

print_endline "-----------------"

let () = safe_hd [ 1; 2; 3 ] |> get_val |> string_of_int |> print_endline

(* Write a function max_hp : pokemon list -> pokemon option that, given a list of pokemon, finds the Pokémon with the highest HP. *)

(* let max_hp = function
   | [] -> None
   | h :: t -> Some (List.fold_left (fun a b -> if a.hp > b.hp then a else b) h t) *)
let rec max_hp = function
  | [] -> None
  | poke1 :: t -> (
      match max_hp t with
      | None -> Some poke1
      | Some poke2 -> Some (if poke1.hp >= poke2.hp then poke1 else poke2))

(* let max_hp = function
   | [] -> None
   | h :: t -> Some (List.fold_left max h t) *)

let pokemons = [ charizard; squirtle; normalPokemon ]
let () = max_hp pokemons |> get_val |> fun x -> x.name |> print_endline
(* let () = max_hp pokemons |> get_val |>  print_endline *)

let is_before ((year, month, day) : int * int * int)
    ((year2, month2, day2) : int * int * int) : bool =
  if year < year2 then true
  else if year = year2 && month < month2 then true
  else if year = year2 && month = month2 && day < day2 then true
  else false

let () = is_before (2000, 1, 3) (2000, 1, 2) |> string_of_bool |> print_endline

let rec earliest = function
  | [] -> None
  | h :: t -> (
      match earliest t with
      | None -> Some h
      | Some x -> Some (if is_before h x then h else x))

let () =
  earliest [ (2000, 1, 3); (1999, 1, 2); (2000, 1, 1) ] |> get_val |> fun x ->
  let year, month, day = x in
  Printf.printf "%d-%d-%d\n" year month day

let insert k v lst = (k, v) :: lst

let rec lookup k = function
  | [] -> None
  | (k', v) :: t -> if k = k' then Some v else lookup k t

let () =
  insert 1 "one" [] |> insert 2 "two" |> insert 3 "three"
  |> List.iter (fun (k, v) -> Printf.printf "%d: %s\n" k v)

let () =
  lookup 2 [ (1, "one"); (2, "two"); (3, "three") ] |> get_val |> print_endline

let () =
  lookup 3 [ (1, "one"); (2, "two"); (3, "three") ] |> get_val |> print_endline

(* type suit = Hearts | Diamonds | Clubs | Spades
   type rank = Number of int | Ace | Jack | Queen | King
   type card = {suit : suit; rank : rank} *)

(* let aceofClubs = {suit = Clubs; rank = Ace};;
   let queenOfHearts = {suit = Hearts; rank = Queen}
   let twoOfDiamonds = {suit = Diamonds; rank = Number 2}
   let sevenOfSpades = {suit = Spades; rank = Number 7} *)

let findMatch = function [ Some x; _ ] -> [ Some x ] | _ -> []

let () =
  findMatch [ None; Some 2 ]
  |> List.iter (fun x -> Printf.printf "%d\n" (get_val x))

type quad = I | II | III | IV
type sign = Neg | Zero | Pos

let sign (x : int) : sign = if x < 0 then Neg else if x = 0 then Zero else Pos
(* let sign (x:int): sign = match x with
   | x when x < 0 -> Neg
   | x when x = 0 -> Zero
   | _ -> Pos *)

(* let sign (x:int): sign = match x with
   | x -> if x < 0 then Neg else if x = 0 then Zero else Pos *)

let quadrant : int * int -> quad option =
 fun (x, y) ->
  match (sign x, sign y) with
  | Pos, Pos -> Some I
  | Neg, Pos -> Some II
  | Neg, Neg -> Some III
  | Pos, Neg -> Some IV
  | _ -> None

let print_quad = function
  | Some x ->
      Printf.printf "%s\n"
        (match x with I -> "I" | II -> "II" | III -> "III" | IV -> "IV")
  | None -> failwith "??"

let () = quadrant (-1, 1) |> print_quad

(* let quadrant ((x, y): (int*int)): quad option = match (sign x, sign y) with
   | (Pos, Pos) -> Some I
   | (Neg, Pos) -> Some II
   | (Neg, Neg) -> Some III
   | (Pos, Neg) -> Some IV
   | _ -> None *)

let quadrant_when : int * int -> quad option = function
  | x, y when x > 0 && y > 0 -> Some I
  | x, y when x < 0 && y > 0 -> Some II
  | x, y when x < 0 && y < 0 -> Some III
  | x, y when x > 0 && y < 0 -> Some IV
  | _ -> None

let () = quadrant_when (1, 1) |> print_quad

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let t = Node (2, Node (1, Leaf, Leaf), Node (3, Leaf, Leaf))
let t2 = Node (2, Node (10, Leaf, Node (4, Leaf, Leaf)), Node (3, Leaf, Leaf))
let rec size = function Leaf -> 0 | Node (_, l, r) -> 1 + size l + size r
let () = size t |> string_of_int |> print_endline
let rec sum = function Leaf -> 0 | Node (v, l, r) -> v + sum l + sum r
let () = sum t |> string_of_int |> print_endline

let rec depth = function
  | Leaf -> 0
  | Node (_, l, r) -> 1 + max (depth l) (depth r)

let () = depth t |> string_of_int |> print_endline

let rec same_shape t1 t2 =
  match (t1, t2) with
  | Leaf, Leaf -> true
  | Node (_, l1, r1), Node (_, l2, r2) -> same_shape l1 l2 && same_shape r1 r2
  | _ -> false

let () = same_shape t t2 |> string_of_bool |> print_endline

(* let rec list_max3 = function
     | [] -> failwith "list_max"
     | h :: t -> begin match t with
      | [] -> 0
      | _ -> max h (list_max3 t)
   end *)

(* let list_max3 lst = if lst = [] then failwith "list_max" else let rec list_max4 = function
     | [] -> 0
     | h :: t -> max h (list_max4 t)
   in list_max4 lst *)
let rec get_list_max x = function [] -> x | h :: t -> get_list_max (max x h) t

let list_max3 = function
  | [] -> failwith "list_max"
  | h :: t -> get_list_max h t

let () = list_max3 [ 1; 23; 3 ] |> string_of_int |> print_endline

let list_max_string = function
  | [] -> "empty"
  | h :: t -> string_of_int (get_list_max h t)

let () = list_max_string [ 1; 23; 3 ] |> print_endline

let rec helper_max : int tree -> int = function
  | Leaf -> 0
  | Node (v, l, r) -> max (max v (helper_max l)) (helper_max r)

let () = helper_max t2 |> string_of_int |> print_endline

let rec helper_min (min_value : int) : int tree -> int = function
  | Leaf -> min_value
  | Node (v, l, r) -> min (min v (helper_min v l)) (helper_min v r)

let is_bst_invariant : int tree -> bool = function
  | Node (v, l, r) ->
      if v > helper_max l && v < helper_min v r then true else false
  | _ -> true

let () = is_bst_invariant t |> string_of_bool |> print_endline

(* let sign_poly x: [> `Pos | `Zero | `Neg ]  = if x > 0 then `Pos else if x < 0 then `Neg else `Zero

   let quadrant_poly (x,y): [> `I | `II | `III | `IV ] option = match sign_poly x, sign_poly y with
     | `Pos, `Pos -> Some `I
     | `Neg, `Pos -> Some `II
     | `Neg, `Neg -> Some `III
     | `Pos, `Neg -> Some `IV
     | _ -> None *)

(* High-Order Functions *)

(* let double x = 2 * x

   let quad x = double (double x)
   let fourth x = square (square x)

   let twice f x = f (f x) *)
let square x = x * x
let twice f x = f (f x)
let () = twice square 3 |> string_of_int |> print_endline

(* let repeat f n x =
   let rec loop i acc = if i > n then acc else loop (i + 1) (f acc) in loop 1  x;; *)
let rec repeat f n x = if n = 0 then x else repeat f (n - 1) (f x)
let () = repeat square 2 3 |> string_of_int |> print_endline
let product_left = List.fold_left ( *. ) 1.0
let () = product_left [ 1.; 2.; 30. ] |> string_of_float |> print_endline

(* let product_right lst = List.fold_right ( *. ) lst 1.0 *)

let product_right = ListLabels.fold_right ~f:( *. ) ~init:1.0
let () = product_right [ 1.; 2.; 40. ] |> string_of_float |> print_endline
let rec ( -- ) i j = if i > j then [] else i :: (i + 1 -- j)

(* let sum_cube_odd n =
   let l = 0 -- n in
   let odds_only = List.filter (fun x -> x mod 2 = 1) l in
   let odd_cubes = List.map (fun x -> x * x * x) odds_only in
   List.fold_left ( + ) 0 odd_cubes *)

let sum_cube_odd n =
  0 -- n
  |> List.filter (fun x -> x mod 2 = 1)
  |> List.map (fun x -> x * x * x)
  |> List.fold_left ( + ) 0

let () = sum_cube_odd 5 |> string_of_int |> print_endline
let rec exists_rec p = function [] -> false | h :: t -> p h || exists_rec p t

let () =
  exists_rec (fun x -> x = 40) [ 1; 2; 3; 40; 27 ]
  |> string_of_bool |> print_endline

(* let exists_fold p lst = List.fold_right (fun x acc -> if p x then true else acc) lst false *)

let exists_fold p lst = List.fold_left (fun acc el -> acc || p el) false lst

let () =
  exists_fold (fun x -> x = 40) [ 1; 2; 3; 40; 27 ]
  |> string_of_bool |> print_endline

let exists_lib p lst =
  if List.filter (fun x -> p x = true) lst = [] then false else true

let () =
  exists_lib (fun x -> x = 40) [ 1; 2; 3; 41; 27 ]
  |> string_of_bool |> print_endline

let exists_lib2 = List.exists

let () =
  exists_lib2 (fun x -> x = 40) [ 1; 2; 3; 41; 27 ]
  |> string_of_bool |> print_endline

let rec account_balance_rec balance = function
  | [] -> balance
  | h :: t -> account_balance_rec (balance - h) t

let () = account_balance_rec 100 [ 40; 20 ] |> string_of_int |> print_endline

(* let account_balance_left  = List.fold_left (fun acc el -> acc - el) *)
let account_balance_left = List.fold_left ( - )
let () = account_balance_left 100 [ 40; 20 ] |> string_of_int |> print_endline
let account_balance_right = List.fold_right (fun el acc -> acc - el)
(* let account_balance_right balance debits = List.fold_right ( - ) debits balance
   wrong: (40 - (20 - 100))
*)

let () = account_balance_right [ 40; 20 ] 100 |> string_of_int |> print_endline
let uncurried_nth (lst, n) = List.nth lst n
let () = uncurried_nth ([ 10; 20; 30 ], 0) |> string_of_int |> print_endline
let uncurried_append (lst1, lst2) = List.append lst1 lst2
let () = uncurried_append ([ 1; 2; 3 ], [ 4; 5; 6 ]) |> print_list;;

print_endline ""

let uncurried_char_compare (c1, c2) = Char.compare c1 c2
let () = uncurried_char_compare ('a', 'a') |> string_of_int |> print_endline
let uncurried_max (x, y) = Stdlib.max x y
let () = uncurried_max (40, 20) |> string_of_int |> print_endline
let map_composition f g lst = List.map (fun x -> f (g x)) lst
(* let map_composition f g lst = List.map (fun x -> x |> g |> f) lst *)

let () =
  map_composition (fun x -> x + 1) (fun x -> x * 2) [ 1; 2; 3 ] |> print_list
;;

print_endline ""

let elementsGreaterThanThree = List.filter (fun x -> String.length x > 3)
let printListOfStrings = List.iter (fun x -> print_endline x)

let () =
  elementsGreaterThanThree [ "hello"; "world"; "ab" ] |> printListOfStrings
;;

print_endline ""

let addOneToListOfFLoats = List.map (fun x -> x +. 1.0)

let printListOfFloats =
  List.iter (fun x ->
      print_float x;
      print_endline "")

let () = addOneToListOfFLoats [ 1.0; 2.0; 3.0 ] |> printListOfFloats

(* let concatListofStrWithSep strs sep = List.fold_left (fun acc str -> if acc = "" then str else acc ^ sep ^ str) "" strs *)

let concatListOfStrWithSep strs sep =
  match strs with
  | [] -> ""
  | x :: xs -> List.fold_left (fun acc str -> acc ^ sep ^ str) x xs

let () = concatListOfStrWithSep [ "hello"; "world" ] "-" |> print_endline

let uniqueKeys (lst : ('a * 'b) list) : 'a list =
  lst |> List.rev_map fst |> List.sort_uniq Stdlib.compare

(* let printAssociationList (lst: (string * 'b) list) = List.iter (fun x -> print_string (fst x)) lst;; *)

let () =
  uniqueKeys [ ("a", 1); ("b", 2); ("c", 3); ("a", 2) ] |> printListOfStrings

(* let is_valid_matrix (lst: int list list) = match lst with
   | [] -> false
   | h :: t -> let head_length = List.length h in
     begin
       match t with
       | [] -> false
       | _ -> List.for_all (fun x -> List.length x = head_length) t
     end *)

let is_valid_matrix (lst : int list list) =
  match lst with
  | [] -> false
  | h :: t ->
      h |> List.length |> fun head_length ->
      head_length > 0 && List.for_all (fun x -> List.length x = head_length) t

let () = is_valid_matrix [ []; [] ] |> string_of_bool |> print_endline

(* let add_row_vectors v1 v2 = List.map2 (fun x y -> x + y) v1 v2 *)
let add_row_vectors = List.map2 ( + )
let () = add_row_vectors [ 1; 2; 3 ] [ 4; 5; 6 ] |> print_list;;

print_endline ""

let add_matrices = List.map2 add_row_vectors

let print_matrix =
  List.iter (fun x ->
      print_list x;
      print_endline "")

let () =
  add_matrices [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ] [ [ 7; 8; 9 ]; [ 10; 11; 12 ] ]
  |> print_matrix

let dot_product v1 v2 = List.fold_left ( + ) 0 (List.map2 ( * ) v1 v2)
let () = dot_product [ 1; 2; 3 ] [ 4; 5; 6 ] |> string_of_int |> print_endline

let rec matrix_to_list (lst : int list list) index =
  match lst with
  | [] -> []
  | h :: t -> (
      match h with [] -> [] | _ -> List.nth h index :: matrix_to_list t index)

let () = matrix_to_list [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ] 0 |> print_list;;

print_endline ""

(* let matrix_transposition (lst: int list list) : int list list = match lst with
     | [] -> []
     | h :: _ -> match List.length h with
       | 0 -> []
       | _ -> let rec loop i acc = if i > (List.length h) - 1 then List.rev acc else loop (i+1) (  matrix_to_list lst i :: acc) in loop 0 [];;

   let () = matrix_transposition [[1; 2; 3]; [4; 5; 6]] |> print_matrix

   let multiply_matrices m1 m2 =
     List.map(fun row -> List.map( dot_product row ) (matrix_transposition m2)) m1 *)

(* let transpose ls =
     let rec transpose' acc = function
       | [] | [] :: _ -> List.rev acc
       | ls -> transpose' (List.map List.hd ls :: acc) (List.map List.tl ls)
     in transpose' [] ls

   let dot = List.fold_left2 (fun acc x y -> acc + x * y) 0

   let multiply_matrices m1 m2 =
     List.map (fun row -> List.map (dot row) (transpose m2)) m1

     let () = multiply_matrices [[1; 2; 3]; [4; 5; 6]] [[7; 8; 9]; [10; 11; 12]] |> print_matrix *)

(* Turn off unused value declaration warnings *)

module MyList = struct
  type 'a myList = Nil | Cons of 'a * 'a myList

  let rec map f = function Nil -> Nil | Cons (h, t) -> Cons (f h, map f t)

  let rec print_list = function
    | Nil -> ()
    | Cons (h, t) ->
        print_int h;
        print_string " ";
        print_list t
end
[@@warning "-34"] [@@warning "-32"]

module Tree = struct
  type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

  let rec map f = function
    | Leaf -> Leaf
    | Node (v, l, r) -> Node (f v, map f l, map f r)
end
[@@warning "-34"] [@@warning "-32"]

let lst = MyList.map succ (Cons (1, Nil))
let () = lst |> MyList.print_list

module MyStack = struct
  type 'a stack = Empty | Entry of 'a * 'a stack

  let empty = Empty
  let push x s = Entry (x, s)
  let peek = function Empty -> failwith "Empty" | Entry (x, _) -> x
  let pop = function Empty -> failwith "Empty" | Entry (_, s) -> s
end
[@@warning "-34"] [@@warning "-32"]

module type LIST_STACK = sig
  type 'a stack = 'a list

  val empty : 'a list
  val push : 'a -> 'a stack -> 'a stack
  val peek : 'a stack -> 'a
  val pop : 'a stack -> 'a stack

  val pp :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a stack -> unit
end
[@@warning "-34"] [@@warning "-32"]

module ListStack : LIST_STACK = struct
  type 'a stack = 'a list

  let empty = []
  let push x s = x :: s
  let peek = function [] -> failwith "Empty" | x :: _ -> x
  let pop = function [] -> failwith "Empty" | _ :: s -> s

  let pp pp_val fmt s =
    let open Format in
    let pp_break fmt () = fprintf fmt "@," in
    fprintf fmt "@[<v 0>top of stack";
    if s <> [] then fprintf fmt "@,";
    pp_print_list ~pp_sep:pp_break pp_val fmt s;
    fprintf fmt "@,bottom of stack@]"
end
[@@warning "-34"] [@@warning "-32"]

let s = ListStack.empty

(* let () = s |> ListStack.(pp Format.fprintf Format.std_formatter) *)
let s' = ListStack.push 33 s
let () = s' |> print_list
let x = ListStack.peek s'
let () = x |> print_int;;

print_endline ""

let s'' = ListStack.(empty |> push 1 |> push 2)
let () = s'' |> print_list;;

print_endline ""

module type Queue = sig
  type 'a t

  exception Empty

  val empty : 'a t
  val is_empty : 'a t -> bool
  val enqueue : 'a -> 'a t -> 'a t
  val front : 'a t -> 'a
  val dequeue : 'a t -> 'a t
  val size : 'a t -> int
  val to_list : 'a t -> 'a list
end
[@@warning "-34"] [@@warning "-32"]

module ListQueue2 : Queue = struct
  type 'a t = 'a list

  exception Empty

  let empty = []
  let is_empty = function [] -> true | _ -> false
  let enqueue x q = q @ [ x ]
  let front = function [] -> raise Empty | x :: _ -> x
  let dequeue = function [] -> raise Empty | _ :: q -> q
  let size = List.length
  let to_list = Fun.id
end
[@@warning "-34"] [@@warning "-32"]

let () =
  ListQueue2.(empty |> enqueue 1 |> enqueue 2 |> enqueue 3 |> to_list)
  |> print_list
;;

print_endline ""

let queue =
  ListQueue2.(empty |> enqueue 5 |> enqueue 6 |> enqueue 7 |> enqueue 8)

let front = ListQueue2.(queue |> front);;

front |> print_int;;
print_endline ""

let () = ListQueue2.(queue |> to_list) |> print_list;;

print_endline "";;

(* let dequeue2 = ListQueue2.(queue |> dequeue);; *)

ListQueue2.(queue |> dequeue |> to_list) |> print_list

module BatchedQueue : Queue = struct
  type 'a t = { o : 'a list; i : 'a list }

  exception Empty

  let empty = { o = []; i = [] }
  let is_empty = function { o = []; _ } -> true | _ -> false

  let enqueue x = function
    | { o = []; _ } -> { o = [ x ]; i = [] }
    | { o; i } -> { o; i = x :: i }
  (* | q -> {q with i = x :: q.i} *)

  let front = function { o = []; _ } -> raise Empty | { o = h :: _; _ } -> h

  let dequeue = function
    | { o = []; _ } -> raise Empty
    | { o = [ _ ]; i } -> { o = List.rev i; i = [] }
    | { o = _ :: t; i } -> { o = t; i }

  let size { o; i } = List.(length o + length i)
  let to_list { o; i } = o @ List.rev i
end

module type ComplexSig = sig
  type t = float * float

  val zero : t
  val add : t -> t -> t
end
[@@warning "-32"]

module Complex : ComplexSig = struct
  type t = float * float

  let zero = (0., 0.)
  let add (r1, i1) (r2, i2) = (r1 +. r2, i1 +. i2)
end

module ListQueue = struct
  type 'a queue = 'a list

  let empty = []
  let is_empty q = q = []
  let enqueue x q = q @ [ x ]
  let peek = function [] -> None | x :: _ -> Some x
  let dequeue = function [] -> None | _ :: q -> Some q
end
[@@warning "-34"] [@@warning "-32"]

(** Creates a ListQueue filled with [n] elements. *)
let fill_listqueue n =
  let rec loop n q =
    if n = 0 then q else loop (n - 1) (ListQueue.enqueue n q)
  in
  loop n ListQueue.empty

let () = fill_listqueue 10 |> print_int_list;;

print_endline ""

(* module type BST = sig
     type ('k, 'v) t
     val add : 'k -> 'v -> ('k, 'v) t
   end *)

module BstMap = struct
  type ('k, 'v) tree =
    | Leaf
    | Node of ('k * 'v) * ('k, 'v) tree * ('k, 'v) tree

  let empty = Leaf

  let rec insert k v = function
    | Leaf -> Node ((k, v), Leaf, Leaf)
    | Node ((k', v'), l, r) ->
        if k = k' then Node ((k, v), l, r)
        else if k < k' then Node ((k', v'), insert k v l, r)
        else Node ((k', v'), l, insert k v r)

  let rec lookup k = function
    | Leaf -> failwith "Not_found"
    | Node ((k', v'), l, r) ->
        if k = k' then v' else if k < k' then lookup k l else lookup k r

  let rec to_list = function
    | Leaf -> []
    | Node ((k, v), l, r) -> to_list l @ [ (k, v) ] @ to_list r
end
[@@warning "-34"] [@@warning "-32"]
;;

print_endline "BST"

let bst1 =
  BstMap.(
    empty |> insert 5 "five" |> insert 6 "six" |> insert 7 "seven"
    |> insert 8 "eight" |> insert 9 "nine" |> insert 10 "ten" |> to_list)

let print_association_list =
  List.iter (fun (k, v) -> Printf.printf "%d: %s\n" k v)

let () = print_association_list bst1

(* let print_tuple (k, v) = Printf.printf "%d: %s\n" k v *)

let () =
  BstMap.(
    empty |> insert 5 "five" |> insert 6 "six" |> insert 7 "seven"
    |> insert 8 "eight" |> insert 9 "nine" |> insert 10 "ten" |> lookup 5
    |> print_string)

module type Fraction = sig
  (* A fraction is a rational number p/q, where q != 0. *)
  type t

  val make : int -> int -> t
  (** [make n d] is n/d. Requires d != 0. *)

  val numerator : t -> int
  val denominator : t -> int
  val to_string : t -> string
  val to_float : t -> float
  val add : t -> t -> t
  val mul : t -> t -> t
end
[@@warning "-34"] [@@warning "-32"]

module Fraction : Fraction = struct
  type t = int * int

  let make n d =
    assert (d != 0);
    (n, d)

  let numerator (n, _) = n
  let denominator (_, d) = d
  let to_string f = string_of_int (fst f) ^ " / " ^ string_of_int (snd f)
  let to_float f = float_of_int (fst f) /. float_of_int (snd f)

  let add (n1, d1) (n2, d2) =
    let d' = d1 * d2 in
    ((n1 * d2) + (n2 * d1), d')

  let mul a b = (fst a * fst b, snd a * snd b)
end
[@@warning "-34"] [@@warning "-32"]

module FractionReduced : Fraction = struct
  type t = int * int

  (** [gcd x y] is the greatest common divisor of [x] and [y].
  Requires: [x] and [y] are positive. *)
  let rec gcd x y =
    if x = 0 then y else if x < y then gcd (y - x) x else gcd y (x - y)

  let make n d =
    assert (d > 0);
    let d' = gcd n d in
    (n / d', d / d')

  let numerator (n, _) = n
  let denominator (_, d) = d
  let to_string f = string_of_int (fst f) ^ " / " ^ string_of_int (snd f)
  let to_float f = float_of_int (fst f) /. float_of_int (snd f)

  let add (n1, d1) (n2, d2) =
    assert (d1 > 0 && d2 > 0);
    let n' = (n1 * d2) + (n2 * d1) in
    let d' = d1 * d2 in
    let d'' = gcd n' d' in
    (n' / d'', d' / d'')
  (* let d' = d1 * d2 in
     (n1 * d2 + n2 * d1, d')
     let (n, d) = d' in
     let d'' = gcd n d in
     (n / d'', d / d'');; *)

  let mul a b =
    let n1, d1 = a in
    let n2, d2 = b in
    let d' = gcd n1 d1 in
    let d'' = gcd n2 d2 in

    (fst a * fst b / d', snd a * snd b / d'')
end
[@@warning "-34"] [@@warning "-32"]
;;

print_endline " "

module CharMap = Map.Make (Char)

let aMap =
  CharMap.(
    empty |> add 'A' "Alpha" |> add 'E' "Echo" |> add 'S' "Sierra"
    |> add 'V' "Victor")

(* let findE = CharMap.find 'E' aMap;; *)

let removeA = CharMap.remove 'A' aMap;;

assert (CharMap.mem 'A' removeA = false)

let convertMapToAssocList = CharMap.bindings removeA

let print_char_association_list =
  List.iter (fun x ->
      print_char (fst x);
      print_string " ";
      print_endline (snd x))

let () = print_char_association_list convertMapToAssocList;;

print_endline " "

type date = { month : int; day : int }

module Date = struct
  type t = date

  let compare a b =
    if a.month = b.month then a.day - b.day else a.month - b.month
end

module DateMap = Map.Make (Date)

type calendar = string DateMap.t

let my_cal : calendar =
  DateMap.(
    empty
    |> add { month = 1; day = 2 } "Birthday"
    |> add { month = 2; day = 3 } "anniversary")

(* let f key value = print_int key.month; print_int key.day; print_string value *)
let print_calendar cal =
  DateMap.(
    cal
    |> iter (fun date event ->
           Printf.printf "%d/%d: %s\n" date.month date.day event))

let () = print_calendar my_cal;;

print_endline ""

let is_for (m : string CharMap.t) : string CharMap.t =
  CharMap.(m |> mapi (fun key value -> Printf.sprintf "%c is for %s" key value))

let () = is_for aMap |> CharMap.bindings |> print_char_association_list

let first_after (d : Date.t) (c : calendar) =
  DateMap.(
    let right (_, _, r) = r in
    c |> split d |> right |> min_binding |> snd)

let () = my_cal |> first_after { month = 2; day = 1 } |> print_string;;

print_endline ""

module StringSet = struct
  type t = string

  let compare s1 s2 =
    String.compare (String.lowercase_ascii s1) (String.lowercase_ascii s2)
end

module CisSet = Set.Make (StringSet)

let () =
  CisSet.(equal (of_list [ "grr"; "argh" ]) (of_list [ "GRR"; "aRgh" ]))
  |> string_of_bool |> print_string
;;

print_endline ""

module type ToString = sig
  type t

  val to_string : t -> string
end

module Print =
functor
  (M : ToString)
  ->
  struct
    let print x = print_string (M.to_string x)
  end

module MyInt = struct
  type t = int

  let to_string = string_of_int
end

module PrintInt = Print (MyInt)

let () = PrintInt.(print 1);;

print_endline ""

module MyString = struct
  type t = string

  let to_string x = x
end

module PrintString = Print (MyString)

let () = PrintString.print "stefano";;

print_endline ""

module StringWithPrint = struct
  include String
  include Print (MyString)
end

let () = StringWithPrint.(sub "stefano" 3 4 |> print);;

(* open Lib

   let d = Date.make_date 1 15 *)
(* open Lib.Algebra *)

(* let d = Date.make_date 1 15 |> Date.get_day *)

(* let () = Math.add 1 2 |> print_int *)

(* let a = FloatField.( zero + one) *)

(* mutable counter*)
print_endline ""

let next_val =
  let counter = ref 0 in
  fun () ->
    incr counter;
    !counter
;;

next_val () |> print_int;
print_endline ""
;;

next_val () |> print_int;
print_endline ""

type 'a node = { next : 'a mlist; value : 'a }
(** An ['a node] is a node of a mutable singly-linked list. It contains a value
    of type ['a] and a link to the [next] node. *)

and 'a mlist = 'a node option ref
(** An ['a mlist] is a mutable singly-linked list with elements of type ['a].
    The [option] represents the possibility that the list is empty.
    RI: The list does not contain any cycles. *)

(** [empty ()] is an empty singly-linked list. *)
let empty () : 'a mlist = ref None

(** [insert_first lst v] mutates mlist [lst] by inserting value [v] as the
    first value in the list. *)
let insert_first (lst : 'a mlist) (v : 'a) : unit =
  lst := Some { next = ref !lst; value = v }

(** [to_list lst] is an OCaml list containing the same values as [lst]
    in the same order. Not tail recursive. *)
let rec to_list (lst : 'a mlist) : 'a list =
  match !lst with None -> [] | Some { next; value } -> value :: to_list next
;;

print_endline "printing list: "

let lst0 = empty ()
let lst1 = lst0;;

insert_first lst0 1;;
to_list lst1 |> print_int_list

type student7 = { name : string; mutable gpa : float }

let alice = { name = "Alice"; gpa = 3.7 }
let () = alice.gpa <- 4.0

let () =
  print_string ("name: " ^ alice.name ^ " gpa: " ^ string_of_float alice.gpa)
;;

print_endline ""

let x = ref true
let y = ref [ 1; 2 ]
let z = [ ref 10 ]

let rec print_int_ref_list = function
  | [] -> print_string ""
  | h :: t ->
      print_int !h;
      print_int_ref_list t

let () =
  print_string (string_of_bool !x);
  print_int_list !y;
  print_int_ref_list z;
  print_endline ""

let inc = ref (fun x -> x + 1)

let () =
  print_int (!inc 3109);
  print_endline ""

let ( +:= ) x y = x := !x + y
let x = ref 0;;

x +:= 3110

let () =
  print_int !x;
  print_endline ""

let x = ref 0
let y = x
let z = ref 0

let () =
  x == y |> string_of_bool |> print_string;
  print_endline ""

let () =
  x == z |> string_of_bool |> print_string;
  print_endline ""

let () =
  x = y |> string_of_bool |> print_string;
  print_endline ""

let () =
  x = z |> string_of_bool |> print_string;
  print_endline ""

let () =
  x := 1;
  !x |> print_int;
  print_endline ""

let () =
  x = y |> string_of_bool |> print_string;
  print_endline ""

let () =
  x = z |> string_of_bool |> print_string;
  print_endline ""
;;

print_endline "norm"

(* AF: the float array [| x1; ...; xn |] represents the
 *     vector (x1, ..., xn)
 * RI: the array is non-empty *)
type vector = float array

(* let norm (v : vector) : float =
   Array.map (fun x -> x *. x) v |> Array.fold_left ( +. ) 0. |> sqrt *)

let norm (v : vector) : float =
  Array.fold_left (fun acc item -> acc +. (item ** 2.)) 0. v |> sqrt

let normLoop (v : vector) : float =
  let n = ref 0.0 in
  for i = 0 to Array.length v - 1 do
    n := !n +. (v.(i) ** 2.)
  done;
  sqrt !n

let avector = [| 3.; 4. |]

let () =
  avector |> norm |> print_float;
  print_endline ""

let () =
  avector |> normLoop |> print_float;
  print_endline ""

let () = assert (avector |> norm = 5.)
let () = assert (avector |> normLoop = 5.)

let normalize (v : vector) : unit =
  let normValue = norm v in
  Array.iteri (fun i x -> v.(i) <- x /. normValue) v

let normalizeLoop (v : vector) : unit =
  let normValue = norm v in
  for i = 0 to Array.length v - 1 do
    v.(i) <- v.(i) /. normValue
  done

let a = [| 1.; 1. |]

let print_float_array arr =
  Array.iter
    (fun x ->
      print_float x;
      print_endline "")
    arr

let () =
  a |> normalize;
  print_float_array a;
  print_endline ""

let () =
  a |> normalizeLoop;
  print_float_array a;
  print_endline ""

(* let init_matrix2 n o f =
   let m = Array.make_matrix n o None in
   for i = 0 to n - 1 do
     for j = 0 to o - 1 do
       m.(i).(j) <- f i j
     done
   done;
   m *)

let init_matrix n o f = Array.init n (fun i -> Array.init o (fun j -> f i j))

let () =
  init_matrix 2 2 (fun x y -> float_of_int x +. float_of_int y)
  |> Array.iter print_float_array;
  print_endline ""

open Lib

let () =
  let result = Math.add 2 3 in
  print_endline (string_of_int result)

let t = Node (2, Node (1, Leaf, Leaf), Node (3, Leaf, Leaf))

let () =
  print_endline "hash function";
  Hashtbl.hash () |> print_int;
  print_endline "";
  Hashtbl.hash 1 |> print_int;
  print_endline "";
  Hashtbl.hash 0 |> print_int;
  print_endline "";
  Hashtbl.hash "" |> print_int;
  print_endline "";
  Hashtbl.hash [] |> print_int;
  print_endline "";
  Hashtbl.hash true |> print_int;
  print_endline "";
  Hashtbl.hash false |> print_int;
  print_endline "";
  Hashtbl.hash t |> print_int;
  print_endline ""

let ( -- ) i j =
  let rec from i j l = if i > j then l else from i (j - 1) (j :: l) in
  from i j []

let tab = Hashtbl.create 16
let ints = 1 -- 33 |> List.map (fun x -> (x, string_of_int x))
let () = ints |> List.iter (fun (k, v) -> Hashtbl.add tab k v)
let () = Hashtbl.find tab 1 |> print_endline

(* let () = Hashtbl.stats tab *)

let bindings h = Hashtbl.fold (fun k v acc -> (k, v) :: acc) h []

let () =
  print_endline "Hashtabl bindings: ";
  bindings tab |> print_association_list

let load_factor h =
  let stats = Hashtbl.stats h in
  float_of_int stats.num_bindings /. float_of_int stats.num_buckets

let () =
  load_factor tab |> print_float;
  print_endline ""

module CaseInsensitive = struct
  type t = string

  let equal s1 s2 = String.lowercase_ascii s1 = String.lowercase_ascii s2
  let hash s = Hashtbl.hash (String.lowercase_ascii s)
end

module HashtblCaseInsensitive = Hashtbl.Make (CaseInsensitive)

module BadHash = Hashtbl.Make (struct
  type t = int

  let equal = ( = )
  let hash _ = 0
end)

let badTab = BadHash.create 16
let () = ints |> List.iter (fun (k, v) -> BadHash.add badTab k v)

(* The bucket histogram is an array a in which a.(i) is the number of buckets whose size is i. *)
let () = assert ((BadHash.stats badTab).bucket_histogram.(33) = 1)

module type SetAbstract = sig
  (* [elt] is the type of the set elements. *)
  type elt

  (* [t] is the type of sets whose elements have type [elt]. *)
  type t

  (* [empty] is the empty set *)
  val empty : t

  (* [insert x s] is the set ${x} \union s$. *)
  val insert : elt -> t -> t

  (* [mem x s] is whether $x \in s$. *)
  val mem : elt -> t -> bool

  (* [of_list lst] is the smallest set containing all the elements of [lst]. *)
  val of_list : elt list -> t

  (* [elements s] is the list containing the same elements as [s]. *)
  val elements : t -> elt list
end
[@@warning "-34"] [@@warning "-32"]

module type Ordered = sig
  type t

  val compare : t -> t -> int
end

module BstSetAbstract (Ord : Ordered) : SetAbstract with type elt = Ord.t =
struct
  (* AF:  [Leaf] represents the empty set.  [Node (l, v, r)] represents
   *   the set $AF(l) \union {v} \union AF(r)$. *)
  (* RI:  for every [Node (l, v, r)], all the values in [l] are strictly
   *   less than [v], and all the values in [r] are strictly greater
   *   than [v]. *)

  type elt = Ord.t
  type t = Leaf | Node of t * elt * t

  let empty = Leaf

  let rec mem x = function
    | Leaf -> false
    | Node (l, v, r) -> (
        match Ord.compare x v with
        | ord when ord < 0 -> mem x l
        | ord when ord > 0 -> mem x r
        | _ -> true)

  let rec insert x = function
    | Leaf -> Node (Leaf, x, Leaf)
    | Node (l, v, r) -> (
        match Ord.compare x v with
        | ord when ord < 0 -> Node (insert x l, v, r)
        | ord when ord > 0 -> Node (l, v, insert x r)
        | _ -> Node (l, x, r))

  let of_list lst = List.fold_left (fun s x -> insert x s) empty lst

  let rec elements = function
    | Leaf -> []
    | Node (l, v, r) -> elements l @ [ v ] @ elements r
end
[@@warning "-34"] [@@warning "-32"]

(* An example usage of the functor: *)

module IntSetAbstract = BstSetAbstract (Int)

let () =
  IntSetAbstract.(empty |> insert 1 |> insert 20 |> elements |> print_int_list)

type 'a tree2 = Leaf | Node of 'a tree2 * 'a * 'a tree2

let rec preorder = function
  | Leaf -> []
  | Node (l, v, r) -> [ v ] @ preorder l @ preorder r

let preorderLinear t =
  let rec go acc = function
    | Leaf -> acc
    | Node (l, v, r) -> go (go (v :: acc) l) r
  in
  List.rev (go [] t)

let rec inorder = function
  | Leaf -> []
  | Node (l, v, r) -> inorder l @ [ v ] @ inorder r

let inorderLinear t =
  let rec go acc = function
    | Leaf -> acc
    | Node (l, v, r) -> go (v :: go acc l) r
  in
  List.rev (go [] t)

let rec postorder = function
  | Leaf -> []
  | Node (l, v, r) -> postorder l @ postorder r @ [ v ]

let postorderLinear t =
  let rec go acc = function
    | Leaf -> acc
    | Node (l, v, r) -> v :: go (go acc l) r
  in
  List.rev (go [] t)

let t =
  Node
    ( Node (Node (Leaf, 1, Leaf), 2, Node (Leaf, 3, Leaf)),
      4,
      Node (Node (Leaf, 5, Leaf), 6, Node (Leaf, 7, Leaf)) )

(*
    t is
          4
        /   \
       2     6
      / \   / \
     1   3 5   7
  *)

let () = assert (preorderLinear t = [ 4; 2; 1; 3; 6; 5; 7 ])
let () = assert (preorder t = [ 4; 2; 1; 3; 6; 5; 7 ])
let () = assert (inorder t = [ 1; 2; 3; 4; 5; 6; 7 ])
let () = assert (inorderLinear t = [ 1; 2; 3; 4; 5; 6; 7 ])
let () = assert (postorder t = [ 1; 3; 2; 5; 7; 6; 4 ])
let () = assert (postorderLinear t = [ 1; 3; 2; 5; 7; 6; 4 ])

(*
Black height 4:
            8b
        /         \
       4b         12b
      / \      /      \
    2b    6b  10b     14b
   /  \  / \  /  \     / \
  1b 3b 5b 7b 9b 11b 13b 15b
  *)
(*
Black height 3:
            8b
        /         \
       4r         12r
      / \      /      \
    2b    6b  10b     14b
   /  \  / \  /  \     / \
  1b 3b 5b 7b 9b 11b 13b 15b
  *)
(*
Black height 2:
            8b
        /         \
       4r         12r
      / \      /      \
    2b    6b  10b     14b
   /  \  / \  /  \     / \
  1r 3r 5r 7r 9r 11r 13r 15r
  *)
(*
RB draw insert:
            Db
        /         \
       Ar         Tr
      / \      /      \
    xx    xx  Sr      xx
   /  \  / \  /  \     / \
  xx xx xx xx xx xx xx xx

rotate:
            Sr
        /         \
       Db         Tb
      / \      /      \
    Ar    xx  xx      xx
   /  \  / \  /  \     / \
  xx xx xx xx xx xx xx xx

insert R:
            Sr
        /         \
       Db         Tb
      / \      /      \
    Ar    Rr  xx      xx
   /  \  / \  /  \     / \
  xx xx xx xx xx xx xx xx

insert U and C:
            Sr
        /         \
       Db         Tb
      / \      /      \
    Ar    Rr  xx      Ur
   /  \  / \  /  \     / \
  xx Cr xx xx xx xx xx xx

rotate:
            Sr
        /         \
       Cr         Tb
      / \      /      \
    Ab    Db  xx      Ur
   /  \  / \  /  \     / \
  xx xx xx Rr xx xx xx xx

insert E:
            Sr
        /         \
       Cr         Tb
      / \      /      \
    Ab    Db  xx      Ur
   /  \  / \  /  \     / \
  xx xx xx Rr xx xx xx xx
          /
         Er

rotate:
            Sr
        /         \
       Cr         Tb
      / \      /      \
    Ab    Er  xx      Ur
   /  \  / \  /  \     / \
  xx xx Db Rb xx xx xx xx
          /
         xx
rotate and change color root node:
            Eb
        /         \
       Cr         Sr
      / \      /      \
    Ab    Db  Rb      Tb
   /  \  / \  /  \     / \
  xx xx xx xx xx xx xx xx Ur
  *)

(* let rec ones = 1 :: ones
   let () = print_int_list ones *)

(** An ['a sequence] is an infinite list of values of type ['a].
AF: [Cons (x, f)] is the sequence whose head is [x] and tail is [f ()].
RI: none.  *)
type 'a sequence = Cons of 'a * (unit -> 'a sequence)

let rec from n = Cons (n, fun () -> from (n + 1))
let nats = from 0

(** [hd s] is the head of [s] *)
let hd (Cons (h, _)) = h

let zero = hd nats

let () =
  print_int zero;
  print_endline ""

(** [tl s] is the tail of [s] *)
let tl (Cons (_, t)) = t ()

let rec take n s = if n = 0 then [] else hd s :: take (n - 1) (tl s)
let () = print_int_list (take 10 nats)

(** [drop n s] is all but the first [n] elements of [s]. *)
let rec drop n s = if n = 0 then s else drop (n - 1) (tl s)

(* let rec filter acc p s = if p (hd s) then Cons (hd s, acc) else tl s *)

(** [square <a;b;c; ...>] is [<a*a; b*b; c*c; ...>] *)
let rec square (Cons (h, t)) = Cons (h * h, fun () -> square (t ()))
[@@warning "-34"] [@@warning "-32"]

(** [sum <a1; a2; a3; ...> <b1; b2; b3; ...>] is [<a1 + b1; a2 + b2; a3 + b3; ...>] *)
let rec sum (Cons (h1, t1)) (Cons (h2, t2)) =
  Cons (h1 + h2, fun () -> sum (t1 ()) (t2 ()))

(** [map f <a; b; c; ...>] is [<f a; f b; f c; ...>]. *)
let rec map f (Cons (h, t)) = Cons (f h, fun () -> map f (t ()))

(** [map2 f <a1; b1; c1; ...> <a2; b2; c2; ...>] is [<f a1 b1; f a2 b2; f a3 b3; ...>] *)
let rec map2 f (Cons (h1, t1)) (Cons (h2, t2)) =
  Cons (f h1 h2, fun () -> map2 f (t1 ()) (t2 ()))
[@@warning "-34"] [@@warning "-32"]

let rec nats = Cons (0, fun () -> map (fun x -> x + 1) nats)
[@@warning "-34"] [@@warning "-32"]

let rec fibs = Cons (1, fun () -> Cons (1, fun () -> sum fibs (tl fibs)))

let () =
  print_endline "Fibonacci";
  print_int (take 10 fibs |> List.rev |> List.hd);
  print_endline ""

let fib30lazy = lazy (take 30 fibs |> List.rev |> List.hd)
[@@warning "-34"] [@@warning "-32"]
(* let fib30 = Lazy.force fib30lazy *)

(* pow2 : int sequence *)
let rec power x n = if n = 0 then 1 else x * power x (n - 1)
let power2 = power 2
let () = print_int (power2 4)

(* let rec pow2sequence (Cons (h, t)) =
   Cons (power2 h, fun () -> pow2sequence (t ())) *)

(* let pow2 = pow2sequence nats *)

(**
 * [pow2from n] is the sequence of powers of 2 starting with [n].
 * example:  [pow2from 4] is <4;8;16;32;...>.
 * requires: [n] is a power of 2
*)
let rec pow2from n = Cons (n, fun () -> pow2from (2 * n))
(* let rec from n = Cons (n, fun () -> from (n + 1)) *)

(**
 * [pow2] is the sequence <1; 2; 4; 8; 16; ...>.
*)
let pow2 = pow2from 1

let () =
  print_endline "pow2:";
  print_int_list (take 10 pow2)

let () =
  print_endline "modulo: ";
  print_int (97 + ((1122 - 97) mod 25));
  print_endline ""

let evenNaturals = map (fun x -> 2 * x) nats

let () =
  print_endline "even naturals";
  print_int_list (take 10 evenNaturals)

(** [from_skip n k] is the sequence <n; n + 1*k, n + 2*k;...> *)
let rec from_skip n k = Cons (n, fun () -> from_skip (n + k) k)

let evens = from_skip 0 2 [@@warning "-34"] [@@warning "-32"]
let dropped_sequence = drop 97 nats [@@warning "-34"] [@@warning "-32"]

let alphabet = map (fun x -> Char.chr (97 + ((x - 97) mod 26))) dropped_sequence
[@@warning "-34"] [@@warning "-32"]

let rec alphabet_gen n =
  Cons (Char.chr ((n mod 26) + Char.code 'a'), fun () -> alphabet_gen (n + 1))

let alphabet2 = alphabet_gen 0

let () =
  print_endline "alphabet:";
  take 30 alphabet2 |> List.iter print_char

let rec random_coin_flips () =
  Cons (Random.bool (), fun () -> random_coin_flips ())

let () =
  print_endline "random_coin_flip";
  take 10 (random_coin_flips ())
  |> List.iter (fun x -> x |> string_of_bool |> print_string)

let rec nth s n = if n = 0 then hd s else nth (tl s) (n - 1)

let () =
  print_endline "nth: ";
  print_int (nth pow2 0)

let rec filter p s =
  if p (hd s) then Cons (hd s, fun () -> filter p (tl s)) else filter p (tl s)

let even3 = filter (fun n -> n mod 2 = 0) nats

let () =
  print_endline "even3: ";
  print_int_list (take 10 even3)

let rec interleave (Cons (h1, t1)) (Cons (h2, t2)) =
  Cons (h1, fun () -> Cons (h2, fun () -> interleave (t1 ()) (t2 ())))

let natsPow2 = interleave nats pow2

let () =
  print_endline "natsPow2";
  print_int_list (take 10 natsPow2)

let dropped_zero_one = drop 2 nats [@@warning "-34"] [@@warning "-32"]
let sift n s = filter (fun x -> x mod n <> 0) s
let sift_2 = sift 2 dropped_zero_one

(* let (Cons(h,t)) = sift_2 in  *)

let () =
  print_endline "sift:";
  print_int_list (take 10 sift_2)

(* let rec sieve s = sieve (sift (hd s) s) *)
let rec sieve s = Cons (hd s, fun () -> sieve (sift (hd s) (tl s)))
let prime = sieve dropped_zero_one

let () =
  print_endline "prime: ";
  print_int_list (take 20 prime)
;;

(* open Lwt_io

   let (p : string Lwt.t), r = Lwt.wait ()
   let p2 = Lwt.bind p (fun i -> Lwt.return (String.length i)) *)
(* let p =
     Lwt.bind (read_line stdin) (fun s1 ->
         Lwt.bind (read_line stdin) (fun s2 -> Lwt_io.printf "%s\n" (s1 ^ s2)))

   let _ = Lwt_main.run p *)
print_endline "Promises:"

(* open Promise


   let p, r = Promise.make () [@@warning "-34"] [@@warning "-32"]

   let print_content x =
     print_int x;
     Promise.return ()

   let p2 = Promise.( >>= ) p print_content [@@warning "-34"] [@@warning "-32"]
   let () = Promise.fulfill r 1 *)
(* let () =
   match Promise.state p2 with
   | Pending -> print_string "Pending"
   | Rejected _ -> failwith "Rejected"
   | Fulfilled _ -> print_string "Fulfilled" *)
(* open Promise

   let () =
     let p, r = Promise.make () in
     let _ =
       Promise.( >>= ) p (fun i ->
           Printf.printf "%i\n" i;
           Promise.return ())
     in
     Promise.fulfill r 42 *)

module LwtExercises = struct
  (* let () =
       let p, r = Lwt.wait () in
       let _ = Lwt.bind p (fun i -> Lwt_io.printf "%i\n" i) in
       Lwt.wakeup_later r 42

     let delay (sec : float) : unit Lwt.t = Lwt_unix.sleep sec
     let delay_then_print () = Lwt.bind (delay 3.0) (fun _ -> Lwt_io.printl "done")
     let _ = Lwt_main.run (delay_then_print ()) *)

  (* open Lwt.Infix

     let delay (sec : float) : unit Lwt.t = Lwt_unix.sleep sec *)

  (* let timing2 () =
       let _t1 = delay 1. >>= fun () -> Lwt_io.printl "1" in
       let _t2 = delay 10. >>= fun () -> Lwt_io.printl "2" in
       let _t3 = delay 20. >>= fun () -> Lwt_io.printl "3" in
       Lwt_io.printl "all done"
     [@@warning "-34"] [@@warning "-32"]

     let _ = Lwt_main.run (timing2 ()) *)

  (* let timing3 () =
       delay 1. >>= fun () ->
       Lwt_io.printl "1" >>= fun () ->
       delay 10. >>= fun () ->
       Lwt_io.printl "2" >>= fun () ->
       delay 20. >>= fun () ->
       Lwt_io.printl "3" >>= fun () -> Lwt_io.printl "all done"

     let _ = Lwt_main.run (timing3 ()) *)

  (* let timing4 () =
       let t1 = delay 1. >>= fun () -> Lwt_io.printl "1" in
       let t2 = delay 10. >>= fun () -> Lwt_io.printl "2" in
       let t3 = delay 20. >>= fun () -> Lwt_io.printl "3" in
       Lwt.join [ t1; t2; t3 ] >>= fun () -> Lwt_io.printl "all done"

     let _ = Lwt_main.run (timing4 ()) *)

  open Lwt.Infix
  open Lwt_io
  open Lwt_unix

  (** [log ()] is a promise for an [input_channel] that reads from
    the file named "log". *)
  let log () : input_channel Lwt.t =
    openfile "log" [ O_RDONLY ] 0 >>= fun fd ->
    Lwt.return (of_fd ~mode:input fd)

  (** [loop ic] reads one line from [ic], prints it to stdout,
    then calls itself recursively. It is an infinite loop. *)
  let rec loop (ic : input_channel) =
    read_line ic >>= fun str ->
    printlf "%s" str >>= fun () -> loop ic
  (* hint: use [Lwt_io.read_line] and [Lwt_io.printlf] *)

  (** [monitor ()] monitors the file named "log". *)
  let monitor () : unit Lwt.t = log () >>= loop

  (** [handler] is a helper function for [main]. If its input is
    [End_of_file], it handles cleanly exiting the program by
    returning the unit promise. Any other input is re-raised
    with [Lwt.fail]. *)
  let handler : exn -> unit Lwt.t = function
    | End_of_file -> Lwt.return ()
    | exc -> Lwt.fail exc

  let main () : unit Lwt.t = Lwt.catch monitor handler
  let _ = Lwt_main.run (main ())
end

let inc_log x = (x + 1, Printf.sprintf "Called inc on %i; " x)

let dec_log x = (x - 1, Printf.sprintf "Called dec on %i; " x)
[@@warning "-34"] [@@warning "-32"]
;;

inc_log 1
