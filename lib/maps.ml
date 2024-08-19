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

module type DirectAddressMap = sig
  type 'v t
  (** [t] is the type of maps that binds keys of type int to values of type ['v]. *)

  val insert : int -> 'v -> 'v t -> unit
  (** [insert k v m] mutates map [m] to bind [k] to [v].
  If [k] was already bound in [m], then the binding is replaced by the binding to [v] in the new map.
  Requires: [k] is in bounds for the [m]. *)
  val find : int -> 'v t -> 'v option
  (** [find k m] is [Some v] if [k] is bound to [v] in [m], and [None] otherwise.
  Requires: [k] is in bounds for the [m]. *)

  val remove : int -> 'v t -> unit
  (** [remove k m] mutates map [m] to remove any binding of [k].
  If [k] was not bound in [m] then the map is unchanged.
  Requires: [k] is in bounds for the [m]. *)

  val create : int -> 'v t
  (** [create c] createa a map with capacity [c]. Keys [0] through [c-1] are _in bounds_ for the map. *)
  val of_list : int -> (int * 'v) list -> 'v t
  (** [of_list c lst] is the map containing the same bindings as association list [lst] and capacity [c].
  Requires: [lst] does not contain any duplicate keys and every key in [lst] is in bounds for capacity [c]. *)

  val bindings :  'v t -> (int * 'v) list
  (** [bindings m] is the association list containing the same bindings as the map [m]. There are no duplicate keys in the list. *)
end

module ArrayMap : DirectAddressMap = struct
  (** AF: [[|Some v0; Some v1; ...|]] represents { 0 : v0, 1 : v1, ... }. If element [i] is [None] then [i] is not bound in the map.
  RI: None
  *)
  type 'v t = 'v option array

  (** Efficiency: O(1) *)
  let insert k v a = a.(k) <- Some v

  (** Efficiency: O(1) *)
  let find k a = a.(k)

  (** Efficiency: O(1) *)
  let remove k a = a.(k) <- None

  (** Efficiency: O(c) *)
  let create c = Array.make c None

  (** Efficiency: O(c) *)
  let of_list c lst =
    let a = create c in
    List.iter (fun (k, v) -> insert k v a) lst;
    a

  (** Efficiency: O(c) *)
  let bindings a =
    let b = ref [] in
    for i = 0 to Array.length a - 1 do
      match a.(i) with
      | None -> ()
      | Some v -> b := (i, v) :: !b
    done;
    !b
end

module type TableMap = sig
  (** [('k, 'v) t] is the type of mutable tabled-based maps that binds keys of type ['k] to values of type ['v].  *)
  type ('k, 'v) t

  (** [insert k v m] mutates map [m] to bind [k] to [v]. If [k] was already bound in [m], then the binding is replaced by the binding to [v]. *)
  val insert : 'k -> 'v ->('k, 'v) t -> unit

  (** [find k m] is [Some v] if [m] binds [k] to [v] and [None] otherwise.*)
  val find : 'k -> ('k, 'v) t -> 'v option

  (** [m remove k] mutates map [m] to remove any binding of [k]. If [k] was not bound in [m] then the map is unchanged. *)
  val remove : 'k -> ('k, 'v) t -> unit

  (** [create hash c] creates a new table map with capacity [c] that will use [hash] as the function to cenvert keys to integers.
  Requires: [hash] distributes keys uniformly over integers and the output of [hash] must always be non-negative and [hash] runs in constant time.
  *)
  val create : ('k -> int) -> int -> ('k, 'v) t

  (** [bindings m] is an association list containing the same bindings
      as [m]. *)
  val bindings : ('k, 'v) t -> ('k * 'v) list

  (** [of_list hash lst] creates a map with the same bindings as [lst],
      using [hash] as the hash function. Requires: [lst] does not
      contain any duplicate keys. *)
  val of_list : ('k -> int) -> ('k * 'v) list -> ('k, 'v) t
end

module HashMap : TableMap = struct
(** AF: If [buckets] is
[| [(k11, v11); (k12, v12); ...]; [(k21, v21); (k22, v22); ...]; ...|]
that represetents the map { k11 : v11, k12 : v12, ...; k21 : v21, k22 : v22, ...}.
RI: No key appears more than once in array (so, no duplicates keys in association lists). All keys are in the right buckets: if [k] is in [buckets] at index [b] then [hash(k) = b]. The output of [hash] must always be non-negative. [hash] must run in constant time. *)
type ('k, 'v) t = {
  hash : 'k -> int;
  mutable size : int;
  mutable buckets : ('k * 'v) list array
}
let create h c = {
  hash = h;
  size = 0;
  buckets = Array.make c []
}

(** [capacity tab] is the number of buckets in [tab].
Efficiency: O(1) *)
let capacity tab = Array.length tab.buckets

(** [load_factor tab] is the load factor of [tab], i.e., the number of bindings divided by the number of buckets. *)
let load_factor tab = float_of_int tab.size /. float_of_int(capacity tab)

(** [index k tab] is the index at which the key [k] should be stored in the buckets of [tab].
Efficiency: O(1) *)
let index k tab = (tab.hash k) mod (capacity tab)

(** [insert_no_resize k v tab] inserts a binding from [k] to [v] in [tab] and does not resize the table, regardless of what happens to the load factor. *)
let insert_no_resize k v tab =
  let b = index k tab in
  let old_bucket = tab.buckets.(b) in
  tab.buckets.(b) <- (k, v) :: List.remove_assoc k old_bucket;
  if not (List.mem_assoc k old_bucket) then tab.size <- tab.size + 1;
  ()

(** [rehash tab new_capacity] replaces the buckets array of [tab] with a new array of size [new_capacity], and re-inserts all the bindings of [tab] into new array. The keys are re-hashed, so the bindings are likely to land in new buckets.
Efficiency: expected O(n), where n is the number of bindings.  *)
let rehash tab new_capacity =
  (** insert [(k, v)] into [tab] *)
  let rehash_binding (k, v) =
  insert_no_resize k v tab in
  (** insert all bindings of [b] into [tab] *)
  let rehash_bucket b =
    List.iter rehash_binding b in
  let old_buckets = tab.buckets in
  tab.buckets <- Array.make new_capacity []; (** O(n) *)
  tab.size <- 0;
  Array.iter rehash_bucket old_buckets (** O(n), O(L) -> expected O(n) *)

(** [resize_if_needed tab] resizes and rehashes [tab] if the load factor is greater than 2.0 or less than 0.5. *)
let resize_if_needed tab =
  let lf = load_factor tab in
  if lf > 2.0 then rehash tab (capacity tab * 2)
  else if lf < 0.5 then rehash tab (capacity tab / 2)
  else ()

(** [remove_no_resize k tab] removes [k] from [tab] and does not trigger a resize, regardeless of what happens to the load factor.
Efficiency: expected O(L) *)
let remove_no_resize k tab =
  let b = index k tab in
  let old_bucket = tab.buckets.(b) in
  tab.buckets.(b) <- List.remove_assoc k old_bucket;
  if (List.mem_assoc k old_bucket) then tab.size <- tab.size - 1;
  ()

(** Efficiency: O(n) *)
let insert k v tab =
  insert_no_resize k v tab;
  resize_if_needed tab

(** Efficiency: expected O(L) *)
let find k tab =
  List.assoc_opt k tab.buckets.(index k tab)

(** Efficiency: O(n) *)
let remove k tab =
  remove_no_resize k tab;
  resize_if_needed tab
 (** Efficiency: O(n). *)
let bindings tab =
  Array.fold_left
    (fun acc bucket ->
        List.fold_left
          (* 1 cons for every binding, which is O(n) *)
          (fun acc (k,v) -> (k,v) :: acc)
           acc bucket) [] tab.buckets

  (** Efficiency: O(n^2). *)
let of_list hash lst =
  let m = create hash (List.length lst) in  (* O(n) *)
  List.iter (fun (k, v) -> insert k v m) lst; (* n * O(n) is O(n^2) *)
  m

end
