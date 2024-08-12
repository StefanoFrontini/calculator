module type Map = sig
  type ('k, 'v) t
  (** [('k, 'v) t] is the type of maps that binds keys of type ['k] to values of type ['v]. *)

  val insert : 'k -> 'v -> ('k, 'v) t -> ('k, 'v) t
  (** [insert k v m] is the same map as [m], but with an additional binding from [k] to [v]. If [k] was already bound in [m], that binding is replaced by the binding to [v] in the new map. *)

  val find : 'k -> ('k, 'v) t -> 'v option
  (** [find k m] is [Some v] if [k] is bound to [v] in [m], and [None] otherwise. *)

  val remove : 'k -> ('k, 'v) t -> ('k, 'v) t
  (** [remove k m] is the same map as [m], but without any binding of [k]. If [k] was not bound in [m] then the map is unchanged *)

  val empty : ('k, 'v) t
  (** [empty] is the empty map.*)

  val of_list : ('k * 'v) list -> ('k, 'v) t
  (** [of_list lst] is the map containing the same bindings as association list [lst].
  Requires: [lst] does not contain any duplicate keys. *)

  val bindings : ('k, 'v) t -> ('k * 'v) list
  (** [bindings m] is the association list containing the same bindings as the map [m]. There are no duplicate keys in the list. *)
end
[@@warning "-34"] [@@warning "-32"]

module AssocListMap : Map = struct
  type ('k, 'v) t = ('k * 'v) list
  (** AF: [[(k1, v1); (k2, v2); ...; (kn, vn)]] is the map {
  k1 : v1, k2 : v2, ..., kn : vn }. If a key appears more then once in the list, then in the map it is bound to the left-most occurrence in the list. For example, [[(k, v1); (k, v2)]] represents the map { k : v1 }. The empty list represents the empty map.
  RI: none *)

  (** Efficiency: O(1) *)
  let insert k v m = (k, v) :: m

  (** Efficiency: O(n) *)
  let find = List.assoc_opt

  (** Efficiency: O(n) *)
  let remove k m = List.filter (fun (k', _) -> k <> k') m

  let empty = []

  (** Efficiency: O(1) *)
  let of_list lst = lst

  (** [keys m] is a list of the keys in [m], without any duplicates.
  Efficiency: O(n log n).
  *)
  let keys m = m |> List.map fst |> List.sort_uniq Stdlib.compare

  (** [binding m k] is [(k, v)], where [v] is the value that [k] binds in [m].
  Requires: [k] is a key in [m].
  Efficiency: O(n).
  *)
  let binding m k = (k, List.assoc k m)

  (** Efficiency: O(n log n) + O(n) * O(n), which is O(n^2) -> O(n^2) *)
  let bindings m = m |> keys |> List.map (binding m)
end
[@@warning "-27"]
