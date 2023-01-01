type value = B of bool | I of int
exception TYPE_ERROR of int
val not1 : value -> value
val not2 : value -> value
exception DIV_BY_0 of int
val div1 : value -> value -> value
val div2 : value -> value -> value option
type 'a btree = Empty | Node of 'a * 'a btree * 'a btree
val t_ex : 'a
