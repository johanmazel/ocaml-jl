
open Printf

open Sexplib.Std
open Bin_prot.Std
       
(* Matt McDonnell: https://www.matt-mcdonnell.com/code/code_ocaml/ocaml_ptree/ocaml_ptree.html with improvments *)

(* \section{Higher order functions}  *)
(* Some useful higher order functions: *)

(* [compose] : function composition *)
let compose f g x = f(g x);;

(* [id] : (identity) acts as the base element for function composition
eg if recursively folding compose over a list of functions
identity is the base element (c.f. 0, []). *)
let id x = x;;

(* [complement] : given two lists a and b returns the list of elements
in b and not a (this may be the opposite to the normal
definition of complement - check this!) *)
let complement alst blst = List.fold_right
  (fun e acc -> if not(List.mem e alst) then e:: acc else acc)
  blst [];;

(* [unique] : Union of a single list, removing repeated entries *)
let rec unique = function
  | [] -> []
  | x :: xs -> [x] @ (unique (List.filter (fun y -> y <> x) xs));;

(* \section{Planar Tree Data Type} *)

(* A planar tree consists of nodes of tuples of an element of
type $\alpha$ and a list of sub planar trees. Leaf nodes
have the empty list as their list of sub trees. *)
(* type data = 'a *)

type 'a t =
| Empty
| Node of 'a * ('a t list)
with compare, sexp, bin_io

exception Empty_tree;;
exception Not_found;;

(* \subsection{Planar Tree functions} *)

(* \subsubsection{Insertion and Deletion} *)

(* [create] : create an empty tree *)
let create () = Empty;;

(* [insert] : function to insert an element into a tree as the leaf of
an existing node or leaf. The idea is to first see if the parent node
Node(e0, lst) exists in the tree, and if so then add the new node to
the list of subtrees of that node *)
let rec insert e0 e = function
  | Empty -> Node(e,[])
  | (Node(a, lst) as t) ->
    if a = e0 then Node(a, lst @ [Node(e,[])])
    else match lst with
      | [] -> t
      | x :: xs -> Node(a, List.map (fun y -> insert e0 e y) lst);;

(* [filterNonEmpty] : remove any [Empty] trees from lists of subtrees *)
let filterNonEmpty lst = List.filter (fun x -> x <> Empty) lst;;

(* [delete] : function to remove a node (and any subnodes) from a tree.
*)
let rec delete e = function
  | Empty -> Empty
  | (Node(a, lst) as t) ->
    if a = e then Empty
    else match lst with
      | [] -> t
      | x:: xs -> Node(a, filterNonEmpty
                         (List.map (fun y -> delete e y) lst)
                      );;

(* [is_empty] : test to see if the tree is empty *)
let is_empty = function Empty -> true | _ -> false;;

(* \subsubsection{Map and Fold} *)

(* [fold]: Fold function for trees. *)
let rec fold
    f
    acc
    t
  =
  match t with
  | Empty -> raise Empty_tree
  | Node(a, lst) ->
    let new_acc =
      f a lst acc
    in
    List.fold_left
      (fold f)
      new_acc
      lst

let rec fold_root_distance
    f
    root_distance
    acc
    t
  =
  match t with
  | Empty -> raise Empty_tree
  | Node(a, lst) ->
    let new_acc =
      f a lst root_distance acc
    in
    List.fold_left
      (fold_root_distance f (root_distance + 1))
      new_acc    
      lst

let fold_level 
    f
    acc
    t
  =
  fold_root_distance
    f
    0
    acc
    t

(* [fold]: Fold function for trees without accumulator. *)
let rec fold_wo_acc f =
  function
  | Empty -> raise Empty_tree
  | Node(a, lst) -> f a (List.map (fold_wo_acc f) lst);;

(* [map]: apply a function to each element of a tree. *)
let map f = fold_wo_acc (fun x lst -> Node(f x, lst));;

(* [iter] : applies function f to all elements of a tree *)
let rec iter f t = match t with
  | Empty -> ()
  | Node(a, lst) -> f a; List.iter (fun x -> iter f x) lst;;

(* [mem] : test for membership of a tree *)
let mem e t = fold_wo_acc (fun x lst -> List.fold_left ( || ) x lst)
  (map (fun x -> x = e) t);;

(* [size] : number of elements in the tree *)
let size t = fold_wo_acc (fun x lst -> 1 + (List.fold_left ( + ) 0 lst)) t;;

let rec down n = function Empty -> Empty
  | Node(x, lst) -> Node(n, List.map (down (n + 1)) lst);;

let depths t = down 1 t;;

(* [height] : maximum distance from root node to leaf. *)
let rec height =
  function
  | Empty -> 0
  | Node(x, lst) -> 1 + List.fold_left max 0 (List.map height lst);;

(* [paths] : list of all paths from root node to other nodes in
   tree *)
let paths t = fold_wo_acc
    (fun x lst -> match lst with
       | [] -> [ [x] ]
       | y :: ys -> List.fold_left ( @ ) [[x]]
                      (List.map (fun a -> List.map (fun b -> x :: b) a) lst) ) t;;

(* Rather than constructing a tree from an explicit series of insert
   operations, it is easier to construct the tree by folding over a
   list of (parent node, child node) tuples. The first element of the
   list is a bit special as the parent node value is not used by the
   insert funstion. *)

let of_list ndlst = match ndlst with
  | [] -> Empty
  | x :: xs ->
    let root = fst x in
    List.fold_left (fun acc (e0, e) -> insert e0 e acc) (Node(root,[])) xs;;

let extract_subtree
    element_to_string
    f_element
    t
  =
  let tree_found_option =
    fold
      (fun 
        a 
        list (* 'a t list *)
        tree_found_option_acc 
        ->
          let element_equal = f_element a in

          let tree_found_option =
            if element_equal then
              Some (Node (a, list))
            else
              None
          in

          let tree_found_result_option : 'a t option =
            match tree_found_option_acc with
            | None -> tree_found_option
            | Some tree_found_acc ->
              (
                match tree_found_option with
                | None -> Some tree_found_acc
                | Some other_tree_found_acc ->
                  (
                    let current_node_string = element_to_string a in

                    let acc_node_string =
                      match tree_found_acc with
                      | Empty -> ""
                      | Node (data, tree_list) -> element_to_string data
                    in

                    print_endline 
                      (sprintf
                         "Ptree: extract_subtree: element inside subtree and on current node:\n%s\n%s"
                         current_node_string
                         acc_node_string
                      );
                    assert(false);
                  )
              )
          in

          tree_found_result_option
      )
      None
      t
  in

  match tree_found_option with
  | None -> raise Not_found
  | Some tree_found -> tree_found

(* exception Several_node_found of ('a * 'a) *)
exception Several_node_found of (string * string)

let node_height to_string_node (node_compare : 'a -> bool) t =
  let node_height_tuple_option =
    fold_level
      (fun 
        node 
        _
        height 
        node_height_option_acc
        ->
          match node_height_option_acc with
          | None ->
            if node_compare node then
              Some (node, height)
            else
              None
          | Some (node_found, height_found) ->
            if node_compare node then
              (
                raise (Several_node_found ((to_string_node node_found), (to_string_node node)))
              )
            else
              Some (node_found, height_found)
      )
      None
      t
  in

  Batteries.Option.map (fun (_, height) -> height) node_height_tuple_option
