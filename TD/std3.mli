val map_cons : 'a -> 'a list list -> 'a list list
val prefixes : 'a list -> 'a list list
val add_freq : 'a -> ('a * int) list -> ('a * int) list
val freq : 'a list -> ('a * int) list
val exists_freq_above : int -> ('a * int) list -> bool
val sum_left : int list -> int
val sum_right : int list -> int
val add_fst : int * int -> int * int
val add_snd : int * int -> int * int
val nb_of : char list -> int * int
val o_sup_f : char list -> bool
val all_o_sup_f : char list list -> bool
val dyck : char list -> bool
