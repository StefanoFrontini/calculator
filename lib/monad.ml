module type Monad = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

let plus_opt (x : int option) (y : int option) : int option =
  match (x, y) with
  | None, _ | _, None -> None
  | Some a, Some b -> Some (Stdlib.( + ) a b)

let ( + ) = plus_opt [@@warning "-34"] [@@warning "-32"]

let minus_opt (x : int option) (y : int option) : int option =
  match (x, y) with
  | None, _ | _, None -> None
  | Some a, Some b -> Some (Stdlib.( - ) a b)

let ( - ) = minus_opt [@@warning "-34"] [@@warning "-32"]

let mult_opt (x : int option) (y : int option) : int option =
  match (x, y) with
  | None, _ | _, None -> None
  | Some a, Some b -> Some (Stdlib.( * ) a b)

let ( * ) = mult_opt [@@warning "-34"] [@@warning "-32"]

let div_opt (x : int option) (y : int option) : int option =
  match (x, y) with
  | None, _ | _, None -> None
  | Some a, Some b -> if b = 0 then None else Some (Stdlib.( / ) a b)

let ( / ) = div_opt [@@warning "-34"] [@@warning "-32"]

let propagate_none (op : int -> int -> int option) (x : int option)
    (y : int option) =
  match (x, y) with None, _ | _, None -> None | Some a, Some b -> op a b

let wrap_output (op : int -> int -> int) (x : int) (y : int) : int option =
  Some (op x y)

let ( + ) = propagate_none (wrap_output Stdlib.( + ))
[@@warning "-34"] [@@warning "-32"]

let ( - ) = propagate_none (wrap_output Stdlib.( - ))
[@@warning "-34"] [@@warning "-32"]

let ( * ) = propagate_none (wrap_output Stdlib.( * ))
[@@warning "-34"] [@@warning "-32"]

let div (x : int) (y : int) : int option =
  if y = 0 then None else wrap_output Stdlib.( / ) x y

let ( / ) = propagate_none div [@@warning "-34"] [@@warning "-32"]
let return (x : int) : int option = Some x

let bind (x : int option) (op : int -> int option) : int option =
  match x with None -> None | Some a -> op a

let ( >>= ) = bind

(* let upgrade : (int -> int option) -> int option -> int option =
   fun (op : int -> int option) (x : int option) -> x >>= op *)

(* let upgrade op x = x >>= op *)

let ( + ) (x : int option) (y : int option) : int option =
  x >>= fun a ->
  y >>= fun b -> return (Stdlib.( + ) a b)
[@@warning "-34"] [@@warning "-32"]

let ( - ) (x : int option) (y : int option) : int option =
  x >>= fun a ->
  y >>= fun b -> return (Stdlib.( - ) a b)
[@@warning "-34"] [@@warning "-32"]

let ( * ) (x : int option) (y : int option) : int option =
  x >>= fun a ->
  y >>= fun b -> return (Stdlib.( * ) a b)
[@@warning "-34"] [@@warning "-32"]

let ( / ) (x : int option) (y : int option) : int option =
  x >>= fun a ->
  y >>= fun b -> if b = 0 then None else return (Stdlib.( / ) a b)
[@@warning "-34"] [@@warning "-32"]

let upgrade_binary op x y =
  x >>= fun a ->
  y >>= fun b -> op a b

let return_binary op x y = return (op x y)
let ( + ) = upgrade_binary (return_binary Stdlib.( + ))

let ( - ) = upgrade_binary (return_binary Stdlib.( - ))
[@@warning "-34"] [@@warning "-32"]

let ( * ) = upgrade_binary (return_binary Stdlib.( * ))
[@@warning "-34"] [@@warning "-32"]

let ( / ) = upgrade_binary div [@@warning "-34"] [@@warning "-32"]
let a = Some 1 + Some 2

module Maybe : Monad = struct
  type 'a t = 'a option

  let return x = Some x
  let ( >>= ) m f = match m with None -> None | Some x -> f x
end

let ( + ) = Stdlib.( + )
let ( - ) = Stdlib.( - )
let ( * ) = Stdlib.( * )
let ( / ) = Stdlib.( / )
let inc_log x = (x + 1, Printf.sprintf "Called inc on %i; " x)
let dec_log x = (x - 1, Printf.sprintf "Called dec on %i; " x)

let log (name : string) (f : int -> int) : int -> int * string =
 fun x -> (f x, Printf.sprintf "Called %s on %i; " name x)

let add (x : int Maybe.t) (y : int Maybe.t) =
  Maybe.( >>= ) x (fun a -> Maybe.( >>= ) y (fun b -> Maybe.return (a + b)))

module type ExtMonad = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val join : 'a t t -> 'a t
end

module MaybeExt : ExtMonad = struct
  type 'a t = 'a option

  let return x = Some x
  let ( >>= ) m f = match m with Some x -> f x | None -> None
  let ( >>| ) m f = match m with Some x -> Some (f x) | None -> None
  let join = function Some m -> m | None -> None
end

module ExtMaybe : ExtMonad = struct
  type 'a t = 'a option

  let return x = Some x
  let ( >>= ) m f = match m with None -> None | Some x -> f x
  (* let ( >>| ) m f = match m with None -> None | Some x -> Some (f x)
     let join = function Some x -> x | None -> None *)

  let ( >>| ) m f = m >>= fun a -> return (f a)
  let join m = m >>= fun a -> a
end

module type FmapJoinMonad = sig
  type 'a t

  val ( >>| ) : 'a t -> ('a -> 'b) -> 'b t
  val join : 'a t t -> 'a t
  val return : 'a -> 'a t
end

module type BindMonad = sig
  type 'a t

  val return : 'a -> 'a t
  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
end

module MakeMonad (M : FmapJoinMonad) : BindMonad = struct
  include M

  let ( >>= ) m f = m >>| f |> join
  (* type 'a t = 'a M.t
     let return x = M.return x
     let ( >>= ) m f = M.join(M.(>>|) m f) *)
end

module ListMonad : ExtMonad = struct
  type 'a t = 'a list

  let return x = [ x ]
  let join = List.flatten
  let ( >>| ) m f = m |> List.map f

  (* let ( >>= ) m f = m >>| f |> join *)
  let ( >>= ) m f = m |> List.map f |> join
end

module Trivial : Monad = struct
  type 'a t = Wrap of 'a

  let return x = Wrap x
  let join (Wrap (Wrap x)) = Wrap x

  (* let ( >>= ) (Wrap x) f = join(Wrap (f x)) *)
  let ( >>= ) (Wrap x) f = f x
end
[@@warning "-34"] [@@warning "-32"]
