type 'a btree = Empty | Node of ('a * 'a btree * 'a btree)
val bt : int btree
val lt_btree : 'a btree -> 'a -> bool
val ge_btree : 'a btree -> 'a -> bool
val is_abr : 'a btree -> bool
val mem : 'a btree -> 'a -> bool
val insert : 'a btree -> 'a -> 'a btree
val abr_of_list : 'a list -> 'a btree
val list_of_abr : 'a btree -> 'a list
val abr_sort : 'a list -> 'a list
