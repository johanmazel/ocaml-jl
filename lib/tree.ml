
open Printf

open Sexplib.Std
open Bin_prot.Std

type ('a, 'b) t =
  Leaf of 'b
| Node of 'a * (('a, 'b) t list)
with compare, sexp, bin_io

(* [fold]: Fold function for trees. *)
let rec fold
    f_leaf
    f_node
    acc
    (t_ : ('a, 'b) t)
    =
  match t_ with
  | Leaf b -> f_leaf b
  | Node (a, lst) ->
    let new_acc =
      f_node
  a
  lst
  acc
    in
    List.fold_left
      (fold f_leaf f_node)
      new_acc
      lst

(* [fold]: Fold function for trees without accumulator. *)
let rec fold_wo_acc
    f_leaf
    f_node
    t
    =
  (* print_endline "Tree: fold_wo_acc"; *)
  match t with
  | Leaf b -> f_leaf b
  | Node (a, lst) ->
    (* print_endline *)
    (*   (sprintf *)
    (*    "Tree: fold_wo_acc: node %d elements in list" *)
    (*    (List.length lst) *)
    (*   ); *)
    (* let _lst_mapped = *)
    (*   List.map *)
    (*   (fold_wo_acc f_leaf f_node) *)
    (*   lst *)
    (* in *)
    (* print_endline "Tree: fold_wo_acc: list mapped"; *)
    f_node
      a
      (List.map
   (fold_wo_acc f_leaf f_node)
   lst)
