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