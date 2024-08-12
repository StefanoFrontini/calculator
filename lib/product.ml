let rec product = function
  | [] -> 1
  | h :: t -> h * product t

 let () = product [1;2;3] |> string_of_int |> print_endline;;

 let rec print_list = function
  | [] -> ()
  | h :: t -> print_int h; print_string " "; print_list t;;
let sortDescendingOrder lst = List.sort Stdlib.compare lst |> List.rev

let () = sortDescendingOrder [10;7;33;400] |> print_list