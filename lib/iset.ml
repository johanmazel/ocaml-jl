(* Copyright 2003 Yamagata Yoriyuki. distributed with LGPL *)
(* Modified by Edgar Friendly <thelema314@gmail.com> *)

include Avltree

open Sexplib.Std
open Bin_prot.Std

module type Type =
sig
  type t
  val compare : t -> t -> int
  val to_string: t -> string

  val min : t
  val max : t

  val pred: t -> t
  val succ: t -> t

  val sub: t -> t -> int

  val lt: t -> t -> bool
  val gt: t -> t -> bool
  val lte: t -> t -> bool
  val gte: t -> t -> bool
  val e: t -> t -> bool
  val ne: t -> t -> bool

  val t_of_sexp : Sexplib.Sexp.t -> t
  val sexp_of_t : t -> Sexplib.Sexp.t

  val bin_t : t Bin_prot.Type_class.t
  val bin_size_t : t -> int
  val bin_write_t : Bin_prot.Common.buf -> pos:Bin_prot.Common.pos -> t -> Bin_prot.Common.pos
  val bin_read_t : Bin_prot.Common.buf -> pos_ref:Bin_prot.Common.pos_ref -> t
  val bin_writer_t : t Bin_prot.Type_class.writer
  val bin_reader_t : t Bin_prot.Type_class.reader
  val __bin_read_t__ : (int -> t) Bin_prot.Read.reader
end

module Make = functor (T: Type) -> struct

  type t = (T.t * T.t) Avltree.tree
  with compare, sexp, bin_io

  type elt = T.t

  let empty = Avltree.Empty

  let is_empty = Avltree.is_empty

  let rec mem (n : T.t) s =
    if is_empty s then false else
      let v1, v2 = root s in
      if T.lt n v1 then mem n (left_branch s) else
  if v1 <= n && n <= v2 then true else
    mem n (right_branch s)

  (*$T mem
    let t = empty |> add_range 1 10 |> add_range 10 20 in \
    mem 1 t && mem 5 t && mem 20 t && not (mem 21 t) && not (mem 0 t)

    let t = Enum.append (1--9) (20 --- 15) |> Enum.map (fun i -> i,i) |> of_enum in \
    mem 1 t && mem 5 t && mem 15 t && not (mem 10 t) && not (mem 14 t)

  *)

  let rec add (n: T.t) s =
    if is_empty s then make_tree empty (n, n) empty else
      let (v1, v2) as v = root s in
      let s0 = left_branch s in
      let s1 = right_branch s in
      if T.ne v1 T.min && T.lt n (T.pred v1) then make_tree (add n s0) v s1 else
  if T.ne v2 T.max && T.gt n (T.succ v2) then make_tree s0 v (add n s1) else
    if T.e (T.succ n) v1 then
      if not (is_empty s0) then
              let (u1, u2), s0' = split_rightmost s0 in
              if T.ne u2 T.max && T.e (T.succ u2) n then
    make_tree s0' (u1, v2) s1
              else
    make_tree s0 (n, v2) s1
      else
              make_tree s0 (n, v2) s1
    else if T.e (T.succ v2) n then
      if not (is_empty s1) then
              let (u1, u2), s1' = split_leftmost s1 in
              if T.ne n T.max && T.e (T.succ n) u1 then
    make_tree s0 (v1, u2) s1'
              else
    make_tree s0 (v1, n) s1
      else
              make_tree s0 (v1, n) s1
    else s

  (*$Q add
    (Q.list Q.small_int) (fun l -> let t = List.fold_left (fun s x -> add x s) empty l in List.for_all (fun i -> mem i t) l)
  *)

  let rec from (n: T.t) s =
    if is_empty s then empty else
      let (v1, v2) as v = root s in
      let s0 = left_branch s in
      let s1 = right_branch s in
      if T.lt n v1 then make_tree (from n s0) v s1 else
  if T.gt n v2 then from n s1 else
    make_tree empty (n, v2) s1

  let after (n: T.t) s = if T.e n T.max then empty else from (T.succ n) s

  let rec until (n: T.t) s =
    if is_empty s then empty else
      let (v1, v2) as v = root s in
      let s0 = left_branch s in
      let s1 = right_branch s in
      if T.gt n v2 then make_tree s0 v (until n s1) else
  if T.lt n v1 then until n s0 else
    make_tree s0 (v1, n) empty

  let before (n: T.t) s = if n = T.min then empty else until (T.pred n) s

  (*$= from & ~cmp:equal ~printer:(IO.to_string print)
    (from 3 (of_list [1,5])) (of_list [3,5])
    empty (from 3 (of_list [1,2]))
  *)

  (*$= until & ~cmp:equal ~printer:(IO.to_string print)
    (until 3 (of_list [1,5])) (of_list [1,3])
    empty (until 3 (of_list [4,5]))
  *)

  let add_range (n1: T.t) (n2: T.t) s =
    if T.gt n1 n2 then invalid_arg (Printf.sprintf "ISet.add_range - %s > %s" (T.to_string n1) (T.to_string n2)) else
      let n1, l =
  if T.e n1 T.min then n1, empty else
          let l = until (T.pred n1) s in
          if is_empty l then n1, empty else
            let (v1, v2), l' = split_rightmost l in
            if T.e (T.succ v2) n1 then v1, l' else n1, l in
      let n2, r =
  if n2 = T.max then n2, empty else
          let r = from (T.succ n2) s in
          if is_empty r then n2, empty else
            let (v1, v2), r' = split_leftmost r in
            if T.e (T.succ n2) v1 then v2, r' else n2, r in
      make_tree l (n1, n2) r

  let singleton (n: T.t) = singleton_tree (n, n)

  (*$T singleton
    singleton 3 |> mem 3
    singleton 3 |> mem 4 |> not
  *)

  let rec remove (n: T.t) s =
    if is_empty s then empty else
      let (v1, v2) as v = root s in
      let s1 = left_branch s in
      let s2 = right_branch s in
      if n < v1 then make_tree (remove n s1) v s2
      else if n = v1 then
  if v1 = v2 then concat s1 s2 else
          make_tree s1 (T.succ v1, v2) s2
      else if n > v1 && n < v2 then
  let s = make_tree s1 (v1, T.pred n) empty in
  make_tree s (T.succ n, v2) s2
      else if n = v2 then make_tree s1 (v1, T.pred v2) s2 else
  make_tree s1 v (remove n s2)

  (*$= remove & ~cmp:equal ~printer:(IO.to_string print)
    empty (remove 3 (singleton 3))
    (of_list [1,5] |> remove 5) (of_list [1,4])
    (of_list [1,5] |> remove 1) (of_list [2,5])
    (of_list [1,5] |> remove 3) (of_list [1,2;4,5])
    (of_list [4,6;1,3;8,10] |> remove 1) (of_list [2,3;4,6;8,10])
    (of_list [4,6;1,3;8,10] |> remove 10) (of_list [1,3;4,6;8,9])
  *)

  let remove_range (n1: T.t) (n2: T.t) s =
    if T.gt n1 n2 then invalid_arg "ISet.remove_range" else
      concat (before n1 s) (after n2 s)

  (*$= remove_range & ~cmp:equal ~printer:(IO.to_string print)
    empty (remove_range 10 15 (of_list [10,15]))
    (of_list [0,20] |> remove_range 3 5) (of_list [0,2;6,20])
    (of_list [0,20] |> remove_range 3 5 |> remove_range 8 10 |> remove_range 5 8) (of_list [0,2;11,20])
  *)

  let rec union s1 s2 =
    if is_empty s1 then s2 else
      if is_empty s2 then s1 else
  let s1, s2 = if height s1 > height s2 then s1, s2 else s2, s1 in
  let n1, n2 = root s1 in
  let l1 = left_branch s1 in
  let r1 = right_branch s1 in
  let l2 = before n1 s2 in
  let r2 = after n2 s2 in
  let n1, l =
    if T.e n1 T.min then n1, empty else
            let l = union l1 l2 in
            if is_empty l then n1, l else
              let (v1, v2), l' = split_rightmost l in (* merge left *)
              if T.e (T.succ v2) n1 then v1, l' else n1, l in
  let n2, r =
    if T.e n1 T.max then n2, empty else
            let r = union r1 r2 in
            if is_empty r then n2, r else
              let (v1, v2), r' = split_leftmost r in (* merge right *)
              if T.e (T.succ n2) v1 then v2, r' else n2, r in
  make_tree l (n1, n2) r

  (*$= union & ~cmp:equal ~printer:(IO.to_string print)
    (union (of_list [3,5]) (of_list [1,3])) (of_list [1,5])
    (union (of_list [3,5]) (of_list [1,2])) (of_list [1,5])
    (union (of_list [3,5]) (of_list [1,5])) (of_list [1,5])
    (union (of_list [1,5]) (of_list [3,5])) (of_list [1,5])
    (union (of_list [1,2]) (of_list [4,5])) (of_list [1,2;4,5])
  *)

  let rec inter s1 s2 =
    if is_empty s1 then empty else
      if is_empty s2 then empty else
  let s1, s2 = if height s1 > height s2 then s1, s2 else s2, s1 in
  let n1, n2 = root s1 in
  let l1 = left_branch s1 in
  let r1 = right_branch s1 in
  let l2 = before n1 s2 in
  let r2 = after n2 s2 in
  let m = until n2 (from n1 s2) in
  concat (concat (inter l1 l2) m) (inter r1 r2)

  (*$= inter & ~cmp:equal ~printer:(IO.to_string print)
    (inter (of_list [1,5]) (of_list [2,3])) (of_list [2,3])
    (inter (of_list [1,4]) (of_list [2,6])) (of_list [2,4])
  *)

  let rec compl_aux (n1: T.t) (n2: T.t) s =
    if is_empty s then add_range n1 n2 empty else
      let v1, v2 = root s in
      let l = left_branch s in
      let r = right_branch s in
      let l = if T.e v1 T.min then empty else compl_aux n1 (T.pred v1) l in
      let r = if T.e v2 T.max then empty else compl_aux (T.succ v2) n2 r in
      concat l r

  let compl s = compl_aux T.min T.max s

  let diff s1 s2 = inter s1 (compl s2)

  (*$= diff & ~cmp:equal ~printer:(IO.to_string print)
    (diff (of_list [1,5]) (of_list [2,3])) (of_list [1,1;4,5])
    (diff (of_list [1,3;6,8]) (of_list [3,6])) (of_list [1,2;7,8])
  *)

  let rec compare_aux x1 x2 =
    match x1, x2 with
      [], [] -> 0
    | `Set s :: rest, x ->
      if is_empty s then compare_aux rest x2 else
  let l = left_branch s in
  let v = root s in
  let r = right_branch s in
  compare_aux (`Set l :: `Range v :: `Set r :: rest) x
    | _x, `Set s :: rest ->
      if is_empty s then compare_aux x1 rest else
  let l = left_branch s in
  let v = root s in
  let r = right_branch s in
  compare_aux x1 (`Set l :: `Range v :: `Set r :: rest)
    | `Range ((v1, v2)) :: rest1, `Range ((v3, v4)) :: rest2 ->
      let sgn = T.compare v1 v3 in
      if sgn <> 0 then sgn else
  let sgn = T.compare v2 v4 in
  if sgn <> 0 then sgn else
          compare_aux rest1 rest2
    | [], _ -> ~-1
    | _, [] -> 1

  let compare s1 s2 = compare_aux [`Set s1] [`Set s2]

  let equal s1 s2 = compare s1 s2 = 0

  (*$T equal
    not (equal (of_list [3,3;5,5]) (of_list [3,3;1,1]))
  *)

  let ord = BatOrd.ord compare

  let rec subset s1 s2 =
    if is_empty s1 then true else
      if is_empty s2 then false else
  let v1, v2 = root s2 in
  let l2 = left_branch s2 in
  let r2 = right_branch s2 in
  let l1 = before v1 s1 in
  let r1 = after v2 s1 in
  (subset l1 l2) && (subset r1 r2)

  (*$T subset
    subset (of_list [1,3]) (of_list [1,5])
    subset (of_list [1,3]) (of_list [1,3])
    subset (of_list []) (of_list [1,5])
    not (subset (of_list [0,3]) (of_list [1,5]))
    not (subset (of_list [0,6]) (of_list [1,5]))
  *)

  let fold_range f s x0 = Avltree.fold (fun (n1, n2) x -> f n1 n2 x) s x0

  let fold f s x0 =
    let rec g n1 n2 a =
      if T.e n1 n2 then f n1 a else
  g (T.succ n1) n2 (f n1 a) in
    fold_range g s x0

  (*$= fold & ~cmp:Int.equal ~printer:string_of_int
    (fold (+) (of_list [1,3]) 0) 6
  *)

  let iter proc s = fold (fun n () -> proc n) s ()

  (*$T iter
    let a = ref 0 in iter (fun _ -> incr a) (of_list [1,3;5,8]); !a = 7
  *)

  let iter_range proc = Avltree.iter (fun (n1, n2) -> proc n1 n2)

  (*$T iter_range
    let a = ref 0 in iter_range (fun _ _ -> incr a) (of_list [1,3;5,8]); !a = 2
  *)

  let for_all p s =
    let rec test_range n1 n2 =
      if T.e n1 n2 then p n1 else
  p n1 && test_range (T.succ n1) n2 in
    let rec test_set s =
      if is_empty s then true else
  let n1, n2 = root s in
  test_range n1 n2 &&
    test_set (left_branch s) &&
    test_set (right_branch s) in
    test_set s

  (*$T for_all
    for_all (fun x -> x < 10) (of_list [1,3;2,7])
    not (for_all (fun x -> x = 5) (of_list [4,5]))
  *)

  let exists p s =
    let rec test_range n1 n2 =
      if n1 = n2 then p n1 else
  p n1 || test_range (n1 + 1) n2 in
    let rec test_set s =
      if is_empty s then false else
  let n1, n2 = root s in
  test_range n1 n2 ||
    test_set (left_branch s) ||
    test_set (right_branch s) in
    test_set s

  (*$T exists
    exists (fun x -> x = 5) (of_list [1,10])
    not (exists (fun x -> x = 5) (of_list [1,3;7,10]))
  *)

  let filter_range p (n1: T.t) (n2: T.t) a =
    let rec loop n1 n2 a = function
    None ->
      if T.e n1 n2 then
  make_tree a (n1, n1) empty
      else
  loop (T.succ n1) n2 a (if p n1 then Some n1 else None)
      | Some v1 as x ->
  if T.e n1 n2 then  make_tree a (v1, n1) empty else
    if p n1 then
            loop (T.succ n1) n2 a x
    else
            loop (T.succ n1) n2 (make_tree a (v1, T.pred n1) empty) None in
    loop n1 n2 a None

  let filter p s = fold_range (filter_range p) empty s

  (*$T filter
    true || equal (filter (fun x -> x <> 5) (of_list [1,10])) (of_list [1,4;6,10])
  *)

  let partition_range p (n1: T.t) (n2: T.t) (a, b) =
    let rec loop n1 n2 acc =
      let acc =
  let a, b, (v, n) = acc in
  if p n1 = v then acc else
    if v then
            (make_tree a (n, n1) empty, b, (not v, n1))
    else
            (a, make_tree b (n, n1) empty, (not v, n1)) in
      if n1 = n2 then
  let a, b, (v, n) = acc in
  if v then  (make_tree a (n, n1) empty, b) else
          (a, make_tree b (n, n1) empty)
      else
  loop (T.succ n1) n2 acc in
    loop n1 n2 (a, b, (p n1, n1))

  let partition p s = fold_range (partition_range p) s (empty, empty)

  let cardinal s =
    fold_range (fun (n1: T.t) n2 c -> c + (T.sub n2 n1) + 1) s 0

  (*$T cardinal
    cardinal (of_list [1,3;5,9]) = 8
  *)

  let rev_ranges s =
    fold_range (fun n1 n2 a -> (n1, n2) :: a) s []

  let rec burst_range n1 n2 a =
    if T.e n1 n2 then n1 :: a else
      burst_range n1 (T.pred n2) (n2 :: a)

  let elements s =
    let f a (n1, n2) = burst_range n1 n2 a in
    List.fold_left f [] (rev_ranges s)

  (*$Q ranges;of_list
    (Q.list (Q.pair Q.int Q.int)) (fun l -> \
    let norml = List.map (fun (x,y) -> if x < y then (x,y) else (y,x)) l in \
    let set = of_list norml in \
    equal set (ranges set |> of_list) \
    )
  *)

  let ranges s = List.rev (rev_ranges s)

  let min_elt (s: (T.t * T.t) tree) =
    let (n, _), _ = split_leftmost s in
    n

  let max_elt (s: (T.t * T.t) tree) =
    let (_, n), _ = split_rightmost s in
    n

  (*$= min_elt & ~printer:string_of_int
    3 (of_list [4,7;8,22;23,23;3,3] |> min_elt)
    1 (of_list [4,7;8,12;23,23;1,3] |> min_elt)
  *)

  (*$T min_elt
    Result.(catch min_elt empty |> is_exn Not_found)
  *)

  (*$= max_elt & ~printer:string_of_int
    23 (of_list [4,7;8,22;23,23;3,3] |> max_elt)
    21 (of_list [4,7;8,12;15,21;1,3] |> max_elt)
  *)

  (*$T max_elt
    Result.(catch max_elt empty |> is_exn Not_found)
  *)

  let choose s = fst (root s)

  let of_list_ranges l = List.fold_left (fun s (lo,hi) -> add_range lo hi s) empty l
  let of_enum_ranges e = BatEnum.fold (fun s (lo,hi) -> add_range lo hi s) empty e

  (* let print oc t = *)
  (*   let print_range oc (lo,hi) = *)
  (*     if lo=hi then BatInt.print oc lo *)
  (*     else BatTuple.Tuple2.printn BatInt.print oc (lo,hi) *)
  (*   in *)
  (*   BatEnum.print print_range oc (enum t) *)

(*$= print & ~printer:(fun x -> x)
  "(1,3) (5,6)" (IO.to_string print (of_list [1,3;5,6]))
*)

  let of_list l =
    List.fold_left (fun s e -> add e s) empty l

  let to_list = elements

  let copy s = of_list (elements s)

  let to_string
      ?first: (first = "")
      ?last: (last = "")
      ?sep: (sep = " ")
      s 
      = 
    List_ext.to_string
      ~first: first
      ~last: last
      ~sep: sep
      T.to_string
      (elements s)

    let to_string_range
        ?first: (first = "")
        ?last: (last = "")
        ?sep: (sep = " ")
        s 
      = 
      List_ext.to_string
        ~first: first
        ~last: last
        ~sep: sep
        (fun (t1, t2) ->
           if t1 = t2 then
             Printf.sprintf
               "%s"
               (T.to_string t1)
           else
             Printf.sprintf
               "[%s-%s]"
               (T.to_string t1)
               (T.to_string t2)
        )
        (ranges s)

end
