
open Printf

type ('a, 'b) t =
| Leaf of 'b
| Node of 'a * ('a, 'b) t * ('a, 'b) t
[@@deriving compare, sexp, bin_io]
(* with compare, sexp, bin_io *)

let rec fold
    f_leaf
    f_node
    t
    =
  match t with 
  | Leaf b -> f_leaf b
  | Node (a, t1, t2) -> 
    f_node
      a 
      (fold f_leaf f_node t1)
      (fold f_leaf f_node t2)

let to_string
    to_string_leaf
    to_string_node
    t
    =
  fold
    to_string_leaf
    to_string_node    
    t
