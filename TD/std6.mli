val sum_impairs : int -> int
val iter_f : int -> ('a -> 'a) -> 'a -> 'a
val rev_append : 'a list -> 'a list -> 'a list
val rev : 'a list -> 'a list
val append : 'a list -> 'a list -> 'a list
val map : ('a -> 'b) -> 'a list -> 'b list
val filter : ('a -> bool) -> 'a list -> 'a list
val partition : ('a -> bool) -> 'a list -> 'a list * 'a list
val fold_right : ('a -> int -> int) -> 'a list -> int -> int
