val is_in : 'a -> 'a list -> bool
val add_elem : 'a -> 'a list -> 'a list
val is_subset_rec : 'a list -> 'a list -> bool
val is_subset : 'a list -> 'a list -> bool
val eq_set : 'a list -> 'a list -> bool
val intersection_rec : 'a list -> 'a list -> 'a list
val intersection : 'a list -> 'a list -> 'a list
val union_rec : 'a list -> 'a list -> 'a list
val union_left : 'a list -> 'a list -> 'a list
val union_right : 'a list -> 'a list -> 'a list
val make_pairs : 'a -> 'b list -> ('a * 'b) list
val product_rec : 'a list -> 'b list -> ('a * 'b) list
val product : 'a list -> 'b list -> ('a * 'b) list
