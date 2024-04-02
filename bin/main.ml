
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
let getFifthElement lst = match List.length lst with
  | m -> if m >= 5 then List.nth lst 4 else 0

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

let () = divideListInTwo 0 [1;2;3; 40; 27] |> fun(tuple) -> match tuple with (a, _) -> print_list a;;


let is_unimodal lst = match lst with
  | [] -> true
  | _ :: [] -> true
  | _ :: _ :: _ -> begin
    match list_max2 lst with
    | m -> findIndex (fun x -> x = m) lst |> fun(i) -> divideListInTwo i lst |> fun (a, b) -> isIncreasing a && isDecreasing b
  end;;

let () = is_unimodal [1;2;3; 40; 27; 26] |> string_of_bool |> print_endline;;