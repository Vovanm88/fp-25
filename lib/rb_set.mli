type 'a t

val make : ('a -> 'a -> int) -> 'a t

val empty : 'a t

val add : 'a -> 'a t -> 'a t

val remove : 'a -> 'a t -> 'a t

val filter : ('a -> bool) -> 'a t -> 'a t

val map : ('a -> 'b) -> ('b -> 'b -> int) -> 'a t -> 'b t

val fold_left : ('acc -> 'a -> 'acc) -> 'acc -> 'a t -> 'acc

val fold_right : ('a -> 'acc -> 'acc) -> 'a t -> 'acc -> 'acc

val append : 'a t -> 'a t -> 'a t

val compare : 'a t -> 'a t -> int

val to_list : 'a t -> 'a list

val of_list : ('a -> 'a -> int) -> 'a list -> 'a t

