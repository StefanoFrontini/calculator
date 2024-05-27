let num = 7 * (1 + 2 + 3);;
num |> string_of_int |> print_endline;

print_endline("CS " ^ string_of_int 3110);;
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
assert (double 7 = 14);;
let cube x = x ** 3. in
7. |> cube |> string_of_float |> print_endline;
assert (cube 7. = 343.);;
let sign x = if x > 0 then 1 else if x < 0 then -1 else 0 in
4 |> sign |> string_of_int |> print_endline;;
let circleArea r = 3.14 *. r ** 2. in
2. |> circleArea |> string_of_float |> print_endline;
assert (circleArea 2. = 12.56);;
let rootMeanSquare x y = sqrt((x ** 2. +. y ** 2.) /. 2.);;
rootMeanSquare 3. 4. |> string_of_float |> print_endline;;
let () = assert (rootMeanSquare 3. 4. < 3.53553390593 +. 0.00001 || rootMeanSquare 3. 4. > 3.53553390593 -. 0.00001);;
let validDate d m = if( (m = "Jan" || m = "Mar" || m = "May" || m = "Jul" || m = "Aug" ||  m = "Oct" || m = "Dec")) then 1 <= d && d <= 31 else if ( (m = "Apr" || m = "Jun" || m = "Sep" || m = "Nov")) then 1 <= d && d <= 30  else if ( m = "Feb") then 1 <= d && d <= 28  else false;;
validDate 28 "Feb" |> string_of_bool |> print_endline;;
let rec fib n = if n <= 1 then 1 else fib (n - 1) + fib (n - 2);;
let rec h n pp p = if n = 0 then p else h (n - 1) p (pp + p);;
let fib_fast n = h n 0 1;;
fib 1 |> string_of_int |> print_endline;;
fib_fast 4 |> string_of_int |> print_endline;;
let divide (numerator: float) (denominator: float) = numerator /. denominator;;
divide 3.0 2.0 |> string_of_float |> print_endline
let ( +/. ) (x: float) (y: float): float = (x +. y) /. 2.;;
16.0 +/. 2.0 |> string_of_float |> print_endline
let rec sum lst =
  match lst with
  | [] -> 0
  | h :: t -> h + sum t;;
sum [1; 2; 3] |> string_of_int |> print_endline;;

let get_val o = match o with
| None -> failwith "??"
| Some x -> x;;


let rec list_max (lst: 'a list): 'a option =
  match lst with
  | [] -> None
  | h :: t -> begin
   match list_max t with
   | None -> Some h
   | Some m -> Some (max h m )
  end;;

(* #trace list_max;; *)
let () = list_max [1; 2; 30] |> get_val |> string_of_int |> print_endline;;
(* let aList = [1;2;3;4;5] *)
(* let aList2 = 1 :: 2 :: 3 :: 4 :: 5 :: [] *)
let aList3 = [1] @ [2;3;4] @ [5]
(* alist3 |> string_of_int |> print_endline *)
let rec print_list = function
  | [] -> ()
  | h :: t -> print_int h; print_string " "; print_list t;;


print_list aList3;;
print_endline "";;

let rec print_int_list = function
| [] -> ()
| h :: t -> print_endline (string_of_int h); print_int_list t


let rec product = function
  | [] -> 1
  | h :: t -> h * product t

 let () = product [1;2;3] |> string_of_int |> print_endline;;

let rec concat = function
  | [] -> ""
  | h :: t -> h ^ concat t

let () = concat ["a";"b";"c"] |> print_endline;;

let isBigRed = function
  | h :: _ -> h = "bigred"
  | _ -> false

let () = isBigRed ["bigred"] |> string_of_bool |> print_endline

let rec list_length = function
  | [] -> 0
  | _ :: t -> 1 + list_length t;;
(* let twoOrFour lst list_length = match list_length lst with
  | m -> m = 2 || m = 4 *)

let twoOrFour lst list_length = list_length lst = 2 || list_length lst = 4
let () = twoOrFour ["a";"b";] list_length |> string_of_bool |> print_endline

let twoOrFourB = function
  | _ :: _ :: [] -> true
  | _ :: _ :: _ :: _ :: [] -> true
  | _ -> false;;

let concatStr str1 str2 = str1 ^ ":" ^ " " ^ str2
(* let concatTwoOrFourB = concatStr "twoOrFourB" *)

let () = twoOrFourB ["a";"b"; "c"; "d"] |> string_of_bool |> concatStr "twoOrFourB" |> print_endline ;;

let firstTwoEqual = function
  | h1 :: h2 :: _ -> h1 = h2
  | _ -> false;;

let () = firstTwoEqual ["a";"a"] |> string_of_bool |> print_endline
(* let getFifthElement lst = match List.length lst with
  | m -> if m >= 5 then List.nth lst 4 else 0 *)

  let getFifthElement lst = if List.length lst >= 5 then List.nth lst 4 else 0

let () = getFifthElement [1;2;3;4;20;30] |> string_of_int |> print_endline

let sortDescendingOrder lst = List.sort Stdlib.compare lst |> List.rev

let () = sortDescendingOrder [10;7;33;400] |> print_list;;
print_endline "";;

let getLastElement lst = List.nth lst (List.length lst - 1)
let () = getLastElement [1;2;3;4;20;30] |> string_of_int |> print_endline

let any_zeros lst = List.exists (fun x -> x = 0) lst
let () = any_zeros [0;1;2;3] |> string_of_bool |> print_endline

let rec take n lst = match List.length lst with
  | m -> if m < n then lst else begin
    match n with
    | 0 -> []
    | num -> begin
      match lst with
      | h :: t -> h :: take (num - 1) t
      | [] -> []
    end
  end;;

let () = take 10 [10;2;3;4;5] |> print_list;;
print_endline "";;


let rec drop n lst = match List.length lst with
  | m -> if m < n then [] else begin
    match n with
    | 0 -> lst
    | num -> begin
      match lst with
      | _ :: t -> drop (num - 1) t
      | [] -> []
    end
  end;;

let () = drop 2 [10;2;3;4;5] |> print_list;;
print_endline "";;

(* let rec from i j l = if i > j then l else from i (j - 1) (j :: l) *)

(** [i -- j] is the list containing the integers from [i] to [j], inclusive. *)
(* let ( -- ) i j = from i j [] *)

(* let long_list = 0 -- 1_000_000 *)

(* let () = drop 20000 long_list |> print_list *)
(* let () = take 200000 long_list |> print_list *)

let rec list_max2 = function
| [] -> 0
| h :: t -> max h (list_max2 t);;

let () = list_max2 [1;2;3; 40; 27] |> string_of_int |> print_endline;;


let rec isIncreasing = function
  | [] -> true
  | [_] -> true
  | h1 :: h2 :: t -> h1 <= h2 && isIncreasing (h2 :: t)

  let () = isIncreasing [1;2;3; 40] |> string_of_bool |> print_endline;;

let rec isDecreasing = function
  | [] -> true
  | [_] -> true
  | h1 :: h2 :: t -> h1 >= h2 && isDecreasing (h2 :: t)

  let () = isDecreasing [ 27] |> string_of_bool |> print_endline;;


 let findIndex f lst = List.find_index f lst |> fun x -> match x with
  | Some x ->  x
  | None -> -1;;

  (* let () = findIndex (fun x -> x = 40) [1;2;3; 40; 27] |> print_endline;; *)

  let rec divideListInTwo index = function
    | [] -> ([], [])
    | h :: t -> if index = 0 then ([], h :: t) else
      let (a, b) = divideListInTwo (index - 1) t  in
      (h :: a, b);;

let () = divideListInTwo 2 [1;2;3; 40; 27] |> fun(tuple) -> match tuple with (a, _) -> print_int_list a;;


let is_unimodal lst = match lst with
  | [] -> true
  | _ :: [] -> true
  | _ :: _ :: _ -> begin
    match list_max2 lst with
    | m -> findIndex (fun x -> x = m) lst |> fun(i) -> divideListInTwo i lst |> fun (a, b) -> isIncreasing a && isDecreasing b
  end;;

let () = is_unimodal [1;2;3; 40; 27; 26] |> string_of_bool |> print_endline;;

let rec powerset = function
  | [] -> [ [] ]
  | x :: s -> let p = powerset s in
    List.map (List.cons x) p @ p

(* let rec powerset: int list -> int list list = function
  | [] -> [[]]
  | [h] -> [[h]]
  | h :: t -> let p = powerset t in List.map( fun x -> h :: x) p @ [h] :: p *)

  let printListOfLists lst = List.iter (fun x -> print_list x; print_endline "") lst

let () = powerset [1;2; 3] |> printListOfLists

type student = {first_name : string; last_name : string; gpa : float}

let aStudent = {first_name = "John"; last_name = "Doe"; gpa = 3.0}

let studentName student = (student.first_name, student.last_name)

let () = studentName aStudent |> fst |> print_endline;;
let () = studentName aStudent |> snd |> print_endline;;
let () = aStudent.gpa |> string_of_float |> print_endline;;

let createStudent first_name last_name gpa = {first_name; last_name; gpa}

let () = createStudent "Jane" "Dow" 4.0 |> studentName |> fst |> print_endline

type poketype = Normal | Fire | Water
type pokemon = {name : string; hp : int; ptype : poketype}

let charizard = {name = "Charizard"; hp = 78; ptype = Fire}
let () = charizard.ptype |> fun(x) -> begin match x with Normal -> "Normal" | Fire -> "Fire" | Water -> "Water" end |> print_endline;;


let squirtle = {name = "Squirtle"; hp = 44; ptype = Water}
let normalPokemon = {name = "Pikachu"; hp = 35; ptype = Normal}

let () = normalPokemon.ptype |> fun(x) -> begin match x with Normal -> "Normal" | Fire -> "Fire" | Water -> "Water" end |> print_endline;;



let safe_hd = function
  | [] -> None
  | h :: _ -> Some h

let safe_tl = function
  | [] -> None
  | _ :: t -> Some t

  let () = safe_tl [1;2;3] |> get_val |> print_int_list;;
  print_endline "-----------------";;
  let () = safe_hd [1;2;3] |> get_val |> string_of_int |> print_endline;;

  (* Write a function max_hp : pokemon list -> pokemon option that, given a list of pokemon, finds the Pokémon with the highest HP. *)

(* let max_hp = function
  | [] -> None
  | h :: t -> Some (List.fold_left (fun a b -> if a.hp > b.hp then a else b) h t) *)
let rec max_hp = function
  | [] -> None
  | poke1::t -> begin
      match max_hp t with
      | None -> Some poke1
      | Some poke2 -> Some (if poke1.hp >= poke2.hp then poke1 else poke2)
    end

(* let max_hp = function
  | [] -> None
  | h :: t -> Some (List.fold_left max h t) *)

let pokemons = [charizard; squirtle; normalPokemon]

let () = max_hp pokemons |> get_val |> fun x -> x.name |>  print_endline
(* let () = max_hp pokemons |> get_val |>  print_endline *)

let is_before ((year, month, day): int * int * int) ((year2, month2, day2): int * int * int): bool = if year < year2 then true else if year = year2 && month < month2 then true else if year = year2 && month = month2 && day < day2 then true else false

let () = is_before (2000, 1, 3) (2000, 1, 2) |> string_of_bool |> print_endline

let rec earliest  = function
  | [] -> None
  | h :: t -> begin
    match earliest t with
    | None -> Some h
    | Some x -> Some ( if is_before h x then h else x)
  end

let () = earliest [(2000, 1, 3); (1999, 1, 2); (2000, 1, 1)] |> get_val |> fun x -> let (year, month, day) = x in Printf.printf "%d-%d-%d\n" year month day

let insert k v lst = (k, v) :: lst

let rec lookup k = function
| [] -> None
| (k', v) :: t -> if k = k' then Some v else lookup k t

let () = insert 1 "one" [] |> insert 2 "two" |> insert 3 "three" |> List.iter (fun (k, v) -> Printf.printf "%d: %s\n" k v)

let () = lookup 2 [(1, "one"); (2, "two"); (3, "three")] |> get_val |> print_endline;;

let () = lookup 3 [(1, "one"); (2, "two"); (3, "three")] |> get_val |> print_endline;;

(* type suit = Hearts | Diamonds | Clubs | Spades
type rank = Number of int | Ace | Jack | Queen | King
type card = {suit : suit; rank : rank} *)

(* let aceofClubs = {suit = Clubs; rank = Ace};;
let queenOfHearts = {suit = Hearts; rank = Queen}
let twoOfDiamonds = {suit = Diamonds; rank = Number 2}
let sevenOfSpades = {suit = Spades; rank = Number 7} *)


let findMatch = function
  | [Some x; _] -> [Some(x)]
  | _ -> []

let () = findMatch [None; Some(2)] |> List.iter (fun x -> Printf.printf "%d\n" (get_val x))

type quad = I | II | III | IV
type sign = Neg | Zero | Pos

let sign (x: int): sign = if x < 0 then Neg else if x = 0 then Zero else Pos
(* let sign (x:int): sign = match x with
  | x when x < 0 -> Neg
  | x when x = 0 -> Zero
  | _ -> Pos *)

(* let sign (x:int): sign = match x with
  | x -> if x < 0 then Neg else if x = 0 then Zero else Pos *)

let quadrant: int*int -> quad option = fun (x,y) -> match (sign x, sign y) with
  | (Pos, Pos) -> Some I
  | (Neg, Pos) -> Some II
  | (Neg, Neg) -> Some III
  | (Pos, Neg) -> Some IV
  | _ -> None

 let print_quad = function
   | Some x -> Printf.printf "%s\n" (match x with
     | I -> "I"
     | II -> "II"
     | III -> "III"
     | IV -> "IV")
   | None -> failwith "??"
   let () = quadrant (-1,1) |> print_quad


  (* let quadrant ((x, y): (int*int)): quad option = match (sign x, sign y) with
    | (Pos, Pos) -> Some I
    | (Neg, Pos) -> Some II
    | (Neg, Neg) -> Some III
    | (Pos, Neg) -> Some IV
    | _ -> None *)

let quadrant_when: int*int -> quad option = function
      | (x, y) when x > 0 && y > 0 -> Some I
      | (x, y) when x < 0 && y > 0 -> Some II
      | (x, y) when x < 0 && y < 0 -> Some III
      | (x, y) when x > 0 && y < 0 -> Some IV
      | _ -> None

let () = quadrant_when (1,1) |> print_quad

type 'a tree =
 | Leaf
 | Node of 'a * 'a tree * 'a tree

let t =
  Node(2, Node(1, Leaf, Leaf), Node(3, Leaf, Leaf))

  let t2 = Node(2, Node(10, Leaf, Node(4, Leaf, Leaf)), Node(3, Leaf, Leaf))

let rec size = function
  | Leaf -> 0
  | Node(_, l, r) -> 1 + size l + size r

let () = size t |> string_of_int |> print_endline

let rec sum = function
  | Leaf -> 0
  | Node(v, l, r) -> v + sum l + sum r

let () = sum t |> string_of_int |> print_endline

let rec depth = function
  | Leaf -> 0
  | Node(_, l, r) -> 1 + max (depth l) (depth r)

let () = depth t |> string_of_int |> print_endline

let rec same_shape t1 t2 = match t1, t2 with
  | Leaf, Leaf -> true
  | Node(_, l1, r1), Node(_, l2, r2) -> (same_shape l1 l2) && (same_shape r1 r2)
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
 let rec get_list_max x = function
 | [] -> x
 | h :: t -> get_list_max (max x h) t

let list_max3 = function
  | [] -> failwith "list_max"
  | h :: t -> get_list_max h t

let () = list_max3 [1;23;3] |> string_of_int |> print_endline

let list_max_string = function
  | [] -> "empty"
  | h :: t -> string_of_int (get_list_max h t)

let () = list_max_string [1;23;3] |> print_endline

let rec helper_max : int tree -> int = function
 | Leaf -> 0
 | Node(v, l, r) -> max (max v (helper_max l)) (helper_max r)

 let () = helper_max t2 |> string_of_int |> print_endline

 let rec helper_min (min_value:int) : int tree -> int = function
 | Leaf -> min_value
 | Node(v, l, r) -> min (min v (helper_min v l)) (helper_min v r)

 let is_bst_invariant : int tree -> bool = function
  | Node(v,l,r) -> if v > helper_max l && v < helper_min v r then true else false
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

    let () = product_left [1.;2.;30.] |> string_of_float |> print_endline

    (* let product_right lst = List.fold_right ( *. ) lst 1.0 *)


    let product_right = ListLabels.fold_right ~f:( *. ) ~init:1.0

    let () = product_right [1.;2.;40.] |> string_of_float |> print_endline

    let rec ( -- ) i j = if i > j then [] else i :: i + 1 -- j

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

    let rec exists_rec p = function
      | [] -> false
      | h :: t ->  p h || exists_rec p t

    let () = exists_rec (fun x -> x = 40) [1;2;3; 40; 27] |> string_of_bool |> print_endline

    (* let exists_fold p lst = List.fold_right (fun x acc -> if p x then true else acc) lst false *)

    let exists_fold p lst = List.fold_left (fun acc el -> acc || p el) false lst

    let () = exists_fold (fun x -> x = 40) [1;2;3; 40; 27] |> string_of_bool |> print_endline

    let exists_lib p lst = if List.filter (fun x -> p x = true) lst = [] then false else true

    let () = exists_lib (fun x -> x = 40) [1;2;3; 41; 27] |> string_of_bool |> print_endline

    let exists_lib2 = List.exists

    let () = exists_lib2 (fun x -> x = 40) [1;2;3; 41; 27] |> string_of_bool |> print_endline

    let rec account_balance_rec balance = function
      | [] -> balance
      | h :: t ->  account_balance_rec (balance - h) t

      let () = account_balance_rec 100 [40; 20] |> string_of_int |> print_endline

    (* let account_balance_left  = List.fold_left (fun acc el -> acc - el) *)
    let account_balance_left  = List.fold_left ( - )

    let () = account_balance_left 100 [40; 20] |> string_of_int |> print_endline

    let account_balance_right = List.fold_right (fun el acc -> acc - el)
    (* let account_balance_right balance debits = List.fold_right ( - ) debits balance
    wrong: (40 - (20 - 100))

    *)

    let () = account_balance_right [40; 20] 100 |> string_of_int |> print_endline

    let uncurried_nth (lst, n) = List.nth lst n

    let () = uncurried_nth ([10;20;30], 0) |> string_of_int |> print_endline

    let uncurried_append (lst1, lst2) = List.append lst1 lst2

    let () = uncurried_append ([1;2;3], [4;5;6]) |> print_list;;
    print_endline "";;

    let uncurried_char_compare (c1, c2) = Char.compare c1 c2

    let () = uncurried_char_compare ('a', 'a') |> string_of_int |> print_endline

    let uncurried_max (x, y) = Stdlib.max x y

    let () = uncurried_max (40, 20) |> string_of_int |> print_endline

    let map_composition f g lst = List.map (fun x -> f (g x)) lst
    (* let map_composition f g lst = List.map (fun x -> x |> g |> f) lst *)

    let () = map_composition (fun x -> x + 1) (fun x -> x * 2) [1;2;3] |> print_list;;

    print_endline ""
    let elementsGreaterThanThree = List.filter (fun x -> String.length x > 3)
    let printListOfStrings = List.iter (fun x -> print_endline x)

    let () = elementsGreaterThanThree ["hello"; "world"; "ab"] |> printListOfStrings;;
    print_endline ""

    let addOneToListOfFLoats = List.map (fun x -> x +. 1.0)

    let printListOfFloats = List.iter (fun x -> print_float x; print_endline "")

    let () = addOneToListOfFLoats [1.0; 2.0; 3.0] |> printListOfFloats;;

    (* let concatListofStrWithSep strs sep = List.fold_left (fun acc str -> if acc = "" then str else acc ^ sep ^ str) "" strs *)

    let concatListOfStrWithSep strs sep =
      match strs with
      | [] -> ""
      | x :: xs -> List.fold_left (fun acc str ->  acc ^ sep ^ str) x xs

    let () = concatListOfStrWithSep ["hello"; "world"] "-" |> print_endline

    let uniqueKeys (lst: ('a * 'b) list) : 'a list = lst |> List.rev_map (fst) |> List.sort_uniq Stdlib.compare

    (* let printAssociationList (lst: (string * 'b) list) = List.iter (fun x -> print_string (fst x)) lst;; *)


    let () = uniqueKeys [("a", 1); ("b", 2); ("c", 3); ("a", 2)] |> printListOfStrings

    (* let is_valid_matrix (lst: int list list) = match lst with
      | [] -> false
      | h :: t -> let head_length = List.length h in
        begin
          match t with
          | [] -> false
          | _ -> List.for_all (fun x -> List.length x = head_length) t
        end *)

let is_valid_matrix (lst: int list list) = match lst with
  | [] -> false
  | h :: t -> h |> List.length |> fun head_length ->
      head_length > 0 &&
      List.for_all (fun x -> List.length x = head_length) t

let () = is_valid_matrix [[]; []] |> string_of_bool |> print_endline

(* let add_row_vectors v1 v2 = List.map2 (fun x y -> x + y) v1 v2 *)
let add_row_vectors  = List.map2 ( + )

let () = add_row_vectors [1; 2; 3] [4; 5; 6] |> print_list;;
print_endline ""

let add_matrices = List.map2 add_row_vectors

let print_matrix = List.iter (fun x -> print_list x; print_endline "")

let () = add_matrices [[1; 2; 3]; [4; 5; 6]] [[7; 8; 9]; [10; 11; 12]] |> print_matrix

let dot_product v1 v2 = List.fold_left ( + ) 0 (List.map2 ( * ) v1 v2)

let () = dot_product [1; 2; 3] [4; 5; 6] |> string_of_int |> print_endline

let rec matrix_to_list (lst: int list list) index = match lst with
  | [] -> []
  | h :: t -> match h with
    | [] -> []
    | _ -> List.nth h index :: matrix_to_list t index

let () = matrix_to_list [[1; 2; 3]; [4; 5; 6]] 0 |> print_list;;
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



  module [@warning "-34"; ] [@warning "-32"] MyList = struct

  type 'a myList =
     | Nil
     | Cons of 'a * 'a myList

  let rec map f = function
     | Nil -> Nil
     | Cons (h, t) -> Cons (f h, map f t)

  let rec print_list = function
     | Nil -> ()
     | Cons (h, t) -> print_int h; print_string " "; print_list t;;

  end

  module [@warning "-34"; ] [@warning "-32"] Tree = struct

  type 'a tree =
     | Leaf
     | Node of 'a * 'a tree * 'a tree

  let rec map f = function
     | Leaf -> Leaf
     | Node (v, l, r) -> Node (f v, map f l, map f r)


  end


  let lst = MyList.map succ (Cons (1, Nil))

  let () = lst |> MyList.print_list

module [@warning "-34"; ] [@warning "-32"] MyStack = struct
  type 'a stack =
     | Empty
     | Entry of 'a * 'a stack

  let empty = Empty

  let push x s =
    Entry (x, s)

  let peek = function
     | Empty -> failwith "Empty"
     | Entry (x,_) -> x

  let pop = function
     | Empty -> failwith "Empty"
     | Entry (_, s) -> s
end

module type [@warning "-34"; ] [@warning "-32"] LIST_STACK = sig
  type 'a stack = 'a list
  val empty: 'a list
  val push: 'a -> 'a stack -> 'a stack

  val peek: 'a stack -> 'a

  val pop: 'a stack -> 'a stack

  val pp :
    (Format.formatter -> 'a -> unit) -> Format.formatter -> 'a stack -> unit
end
module [@warning "-34"; ] [@warning "-32"] ListStack: LIST_STACK = struct
  type 'a stack = 'a list

  let empty = []

  let push x s =
    x :: s

  let peek = function
    | [] -> failwith "Empty"
    | x :: _ -> x

  let pop = function
    | [] -> failwith "Empty"
    | _ :: s -> s

  let pp pp_val fmt s =
    let open Format in
    let pp_break fmt () = fprintf fmt "@," in
    fprintf fmt "@[<v 0>top of stack";
    if s <> [] then fprintf fmt "@,";
    pp_print_list ~pp_sep:pp_break pp_val fmt s;
    fprintf fmt "@,bottom of stack@]"

end

let s = ListStack.empty

(* let () = s |> ListStack.(pp Format.fprintf Format.std_formatter) *)
let s' = ListStack.push 33 s

let () = s' |> print_list

let x = ListStack.peek s'

let () = x |> print_int;;

print_endline "";;

let s'' = ListStack.(empty |> push 1 |> push 2 )

let () = s'' |> print_list;;

print_endline "";;

module type [@warning "-34"; ] [@warning "-32"] Queue = sig
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

module [@warning "-34"; ] [@warning "-32"] ListQueue2 : Queue = struct
  type 'a t = 'a list

  exception Empty

  let empty = []

  let is_empty = function
    | [] -> true
    | _ -> false

  let enqueue x q = q @ [x]

  let front = function
    | [] -> raise Empty
    | x :: _ -> x

  let dequeue = function
    | [] -> raise Empty
    | _ :: q -> q

  let size = List.length

  let to_list = Fun.id
end

let () = ListQueue2.(empty |> enqueue 1 |> enqueue 2 |> enqueue 3 |> to_list) |> print_list;;

print_endline ""

let queue = ListQueue2.(empty |> enqueue 5 |> enqueue 6 |> enqueue 7 |> enqueue 8);;
let front = ListQueue2.(queue |> front);;
front |> print_int;;

print_endline "";;

let () = ListQueue2.(queue |> to_list) |> print_list;;

print_endline "";;

(* let dequeue2 = ListQueue2.(queue |> dequeue);; *)

ListQueue2.(queue |> dequeue |> to_list) |> print_list;;

module BatchedQueue : Queue = struct
  type 'a t = {o : 'a list; i : 'a list}

  exception Empty

  let empty = {o = []; i = []}

  let is_empty = function
    | {o = []; _} -> true
    | _ -> false

  let enqueue x = function
    | {o = []; _} -> {o = [x]; i = []}
    | {o; i} -> {o; i = x :: i}
    (* | q -> {q with i = x :: q.i} *)

  let front = function
    | {o = []; _} -> raise Empty
    | {o = h :: _; _} -> h

  let dequeue = function
    | {o = []; _} -> raise Empty
    | {o = [_]; i} -> {o = List.rev i; i = []}
    | {o = _ :: t; i} -> {o = t; i}

  let size {o; i} = List.(length o + length i)

  let to_list {o; i} = o @ List.rev i
end



  module type [@warning "-32"] ComplexSig = sig
    type t = float * float
    val zero : t
    val add : t -> t -> t
  end


  module Complex : ComplexSig = struct
    type t = float * float
    let zero = (0., 0.)
    let add (r1, i1) (r2, i2) = r1 +. r2, i1 +. i2
  end

  module [@warning "-34"; ] [@warning "-32"] ListQueue = struct
    type 'a queue = 'a list

    let empty = []

    let is_empty q = q = []

    let enqueue x q = q @ [x]

    let peek = function
    | [] -> None
    | x :: _ -> Some x

    let dequeue = function
    | [] -> None
    | _ :: q -> Some q
  end


  (** Creates a ListQueue filled with [n] elements. *)
let fill_listqueue n =
  let rec loop n q =
    if n = 0 then q
    else loop (n - 1) (ListQueue.enqueue n q) in
  loop n ListQueue.empty


  let () = fill_listqueue 10 |> print_int_list;;
  print_endline "";;

(* module type BST = sig
  type ('k, 'v) t
  val add : 'k -> 'v -> ('k, 'v) t
end *)

module [@warning "-34"; ] [@warning "-32"] BstMap = struct
 type ('k, 'v) tree =
  | Leaf
  | Node of ('k * 'v) * ('k, 'v) tree * ('k, 'v) tree

  let empty = Leaf

  let rec insert k v = function
   | Leaf -> Node((k, v), Leaf, Leaf)
   | Node((k', v'), l, r) -> if (k = k') then Node((k, v), l, r)
      else if (k < k') then Node((k', v'), insert k v l, r) else Node((k',v'), l, insert k v r)

  let rec lookup k = function
    | Leaf -> failwith "Not_found"
    | Node((k', v'), l, r) -> if (k = k') then v' else if (k < k') then lookup k l else lookup k r

  let rec to_list = function
    | Leaf -> []
    | Node((k, v), l, r) ->  to_list l @ [(k, v)] @ to_list r

end;;

print_endline "BST";;
let bst1 = BstMap.(empty |> insert 5 "five" |> insert 6 "six" |> insert 7 "seven" |> insert 8 "eight" |> insert 9 "nine" |> insert 10 "ten" |> to_list);;

let print_association_list = List.iter (fun (k, v) -> Printf.printf "%d: %s\n" k v)


let () = print_association_list bst1

(* let print_tuple (k, v) = Printf.printf "%d: %s\n" k v *)

let () = BstMap.(empty |> insert 5 "five" |> insert 6 "six" |> insert 7 "seven" |> insert 8 "eight" |> insert 9 "nine" |> insert 10 "ten" |> lookup 5 |> print_string)

module type Fraction = sig
  (* A fraction is a rational number p/q, where q != 0. *)
  type t

  (** [make n d] is n/d. Requires d != 0. *)
  val make : int -> int -> t

  val numerator : t -> int
  val denominator : t -> int
  val to_string : t -> string
  val to_float : t -> float

  val add : t -> t -> t
  val mul : t -> t -> t
end

module Fraction: Fraction = struct
  type t = int * int

  (* let make t  *)
end
