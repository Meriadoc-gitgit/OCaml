type 'a htree = Leaf of int * 'a | Branch of int * 'a htree * 'a htree
val t_msg : char htree
val huff_tab : 'a htree -> ('a * int list) list
val code : 'a list -> ('a * int list) list -> int list
val decode1 : int list -> 'a htree -> 'a * int list
val decode : int list -> 'a htree -> 'a list
val freq_ht : 'a htree -> int
val ht_less : 'a htree -> 'a htree -> bool
val min_sauf_min : 'a htree list -> 'a htree * 'a htree list
val ht_branch : 'a htree -> 'a htree -> 'a htree
val leaf_list : ('a * int) list -> 'a htree list
