module type Set = sig
  type 'a t
  (** ['a t] is the type of sets whose elements have type ['a ]. *)

  val empty : 'a t
  (** [empty] is the empty set *)

  val insert : 'a -> 'a t -> 'a t
  (** [insert x s] is the set containing [x] as well as all the elements of [s]. *)

  val mem : 'a -> 'a t -> bool
  (** [mem x s] is weather [x] is a member of [s]. *)
end

module ListSet : Set = struct
  type 'a t = 'a list
  (** AF: [[x1, ..., xn]] represents the set {x1, ..., xn}.
 RI: the list contains no duplicates *)

  let empty = []

  (** Efficiency: O(n) *)
  let mem = List.mem

  (** Efficiency: O(n) *)
  let insert x s = if mem x s then s else x :: s
end

module BstSet : Set = struct
  (** AF: [Leaf] represents the empty set. [Node (l, v, r)] represents the set containing [v], as well as all the elements of the sets represented by [l] and [r]
 RI: for every [Node (l,v,r)], all the values in [l] are strictly less than [v], and all the values in [r] are stricly greater than [v].
  *)

  type 'a t = Leaf | Node of 'a t * 'a * 'a t

  let empty = Leaf

  let rec mem x = function
    | Leaf -> false
    | Node (l, v, r) ->
        if x < v then mem x l else if x > v then mem x r else true

  let rec insert x = function
    | Leaf -> Node (Leaf, x, Leaf)
    | Node (l, v, r) as n ->
        if x < v then Node (insert x l, v, r)
        else if x > v then Node (l, v, insert x r)
        else n
end
