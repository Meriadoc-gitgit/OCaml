type 'a htree = 
| Leaf of int * 'a
| Branch of int * 'a htree * 'a htree 
let t_msg : 'a htree = 
  Branch (
    17, Leaf (8, 'A'), 
    Branch (
      9, 
      Branch (
        4, 
        Branch (2, Leaf (1, 'C'), Leaf (1, 'G')), 
        Branch (2, Leaf (1, 'H'), Leaf (1, 'F'))
      ), 
      Branch (
        5, 
        Branch (2, Leaf (1, 'E'), Leaf (1, 'D')),
        Leaf (3, 'B')
      )
    )
  )

(* Exo 1 - Codage de Huffman *)
let rec huff_tab (t : 'a htree) : ('a * (int list)) list =
  let rec num (n : int) (l : ('a * (int list)) list) : ('a * (int list)) list =
  match l with
  |[] -> []
  |h :: t -> (fst h, n :: (snd h)) :: (num n t)
  (* them 1 thanh phan vao day !! *)
  in
  match t with
  |Leaf (_, c) -> [(c, [])]
  |Branch(_, g, d) -> (num 0 (huff_tab g)) @ (num 1 (huff_tab d));;

let rec code (m : 'a list) (c : ('a * int list) list) : int list = 
  let rec find (v : 'a) (t : ('a * int list) list) : int list = 
    match t with 
    | [] -> raise Not_found
    | x :: y -> if fst x = v then snd x else find v y
  in 
  match m with 
  | [] -> []
  | e :: r -> (find e c) @ code r c;;

let lc_msg = huff_tab t_msg in 
let msg = ['A';'A';'B';'A';'C';'B';'A';'G';'H';'A';'A';'F';'E';'A';'D';'B';'A']
  in code msg lc_msg;;

let rec decode1 (l : int list) (t : 'a htree) : ('a * int list) = 
  let rec subset (l1 : int list) (l2 : int list) : bool = 
    match l2 with 
    | [] -> if l1 = [] then true else false 
    | e :: r -> 
      match l1 with 
      | [] -> true
      | x :: y -> x = e && subset y r 
  in 
  let rec not (v : int) (l2 : int list) : int list = 
    match l2 with 
    | [] -> if v = 0 then [] else raise (Invalid_argument "Empty")
    | e :: r -> if v = 0 then e :: r else not (v-1) r
  in 
  let rec res (l1 : ('a * int list) list) (l2 : int list) : ('a * int list) = 
    match l1 with 
    | [] -> raise (Invalid_argument "Empty")
    | e :: r -> if subset (snd e) l2 then (fst e, not (List.length (snd e)) l2) else res r l2 
  in res (huff_tab t) l;;

let rec decode (l : int list) (t : 'a htree) : 'a list = 
  match snd (decode1 l t) with 
  | [] -> fst (decode1 l t) :: []
  | e :: r -> fst (decode1 l t) :: decode (snd (decode1 l t)) t;;