(* HashSet
 * Copyright (C) 2006 Mario Pernici
 * This software is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License version 2, as published by the Free Software Foundation.
 * 
 * This software is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 * 
 * See the GNU Library General Public License version 2 for more details
 * (enclosed in the file LGPL).
 *)

(* Version 0.2 *)

(*
  HashSet is a hashed set, fast on integers and floats, and 
  with O(log N) amortized worst case guarantee for the add, mem and
  remove operations.
  
  The buckets are ordered (only for internal use; HashSet does
  not keep all the elements ordered, unlike Set).
  The first element of a bucket is kept in a separate array v_keys, 
  with occupation controlled by a vector v_occ. Since on average
  more than 60% of the elements are in v_keys, this leads
  to a significant speed-up for integers and floats, which are unboxed.
  The rest of the bucket is a sorted list at creation; if the
  bucket grows beyond limit_list it is converted to a balanced binary tree,
  adapted from the one in Set in the OCaml standard library.
*)

(* Generic interface *)

let limit_list = 30
let hash  = Hashtbl.hash

type 'a t =
  { mutable v_keys:'a array;        (* v_keys.(i) is the head of
                                       bucket if v_occ.(i) is
                                       positive *)
    mutable v_occ : int array;      (* bucket occupation number *)
    mutable size: int;              (* number of elements in the set *)
    mutable data: 'a bucket array   (* data for keys in bucket, apart
                                       from the head of the bucket*)
  }
and 'a bucket =
  Empty
| Cons of 'a * 'a bucket                    (* list *)
| Node of 'a bucket * 'a * 'a bucket * int  (* tree *)

(* operations on the buckets in data *)
(* Functions acting on trees end with the suffix _t; 
 * functions acting on a bucket in data end with _b .
 *)

let isList_b s =
  match s with
    Empty -> false
  | Cons(_,_) -> true
  | Node(_,_,_,_) -> false;;
  

let height_t = function
    Empty -> 0
  | Node(_, _, _, h) -> h
  | Cons(_,_) -> invalid_arg "HashSet.height_t"

let create_t l v r =
  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h
             | Cons(_,_) -> invalid_arg "HashSet.create_t" in
  let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h
             | Cons(_,_) -> invalid_arg "HashSet.create_t" in
  Node(l, v, r, (if hl >= hr then hl + 1 else hr + 1))

let bal_t l v r =
  let hl = match l with Empty -> 0 | Node(_,_,_,h) -> h
             | Cons(_,_) -> invalid_arg "HashSet.bal_t" in
  let hr = match r with Empty -> 0 | Node(_,_,_,h) -> h
             | Cons(_,_) -> invalid_arg "HashSet.bal_t" in
  if hl > hr + 2 then 
    begin
    match l with
      Empty -> invalid_arg "HashSet.bal_t"
    | Node(ll, lv, lr, _) ->
        if height_t ll >= height_t lr then
          create_t ll lv (create_t lr v r)
        else
          begin
          match lr with
            Empty -> invalid_arg "HashSet.bal_t"
          | Node(lrl, lrv, lrr, _)->
              create_t (create_t ll lv lrl) lrv (create_t lrr v r)
          | Cons(_,_) -> invalid_arg "HashSet.bal_t"
          end
    | Cons(_,_) -> invalid_arg "HashSet.bal_t"
    end
  else if hr > hl + 2 then
    begin
    match r with
      Empty -> invalid_arg "HashSet.bal_t"
    | Node(rl, rv, rr, _) ->
        if height_t rr >= height_t rl then
          create_t (create_t l v rl) rv rr
        else
          begin
          match rl with
            Empty -> invalid_arg "HashSet.bal_t"
          | Node(rll, rlv, rlr, _) ->
              create_t (create_t l v rll) rlv (create_t rlr rv rr)
          | Cons(_,_) -> invalid_arg "HashSet.bal_t"
          end
    | Cons(_,_) -> invalid_arg "HashSet.bal_t"
    end
  else
    Node(l, v, r, (if hl >= hr then hl + 1 else hr + 1))

(* add_t and add_b are used to add an element x only after checking 
 * that x is not in the set
 *)
let rec add_t x = function
    Empty -> 
      Node(Empty, x, Empty, 1);
  | Node(l, v, r, _) as t ->
      let c = compare x v in
      if c = 0 then t else
      if c < 0 then bal_t (add_t x l) v r else bal_t l v (add_t x r)
  | Cons(_,_) -> invalid_arg "HashSet.add_t"


let rec add_b x = function
  Empty -> 
    Cons(x, Empty)
  | Cons(k, rest) as t ->
      let c = (compare x k) in
      if c < 0 then Cons(x, t) else 
      if c = 0 then t 
      else Cons(k, add_b x rest)
  | Node(l, v, r, _) as t ->
      let c = (compare x v) in
      if c = 0 then t else
      if c < 0 then bal_t (add_t x l) v r 
      else bal_t l v (add_t x r)

let rec min_elt_t = function
    Empty -> raise Not_found
  | Node(Empty, v, r, _) -> v
  | Node(l, v, r, _) -> min_elt_t l
  | Cons(_,_) -> invalid_arg "HashSet.min_elt_t"

let rec remove_min_elt_t = function
    Empty -> invalid_arg "HashSet.remove_min_elt_t"
  | Node(Empty, v, r, _) -> r
  | Node(l, v, r, _) -> bal_t (remove_min_elt_t l) v r
  | Cons(_,_) -> invalid_arg "HashSet.remove_min_elt_t"

let merge_t t1 t2 =
  match (t1, t2) with
    (Empty, t) -> t
  | (t, Empty) -> t
  | (Cons(_,_),_) -> invalid_arg "HashSet.merge_t"
  | (_, Cons(_,_)) -> invalid_arg "HashSet.merge_t"
  | (_, _) -> bal_t t1 (min_elt_t t2) (remove_min_elt_t t2)

let rec mem_b key = function
  | Empty ->
      false
  | Cons(k, rest) ->
      let c = compare key k in
      if c = 0 then true
      else if c < 0 then false
      else
        mem_b key rest
  | Node(l, v, r, _) ->
      let c = compare key v in
      c = 0 || mem_b key (if c < 0 then l else r) 


let rec remove_b key s = match s with
    Empty -> Empty
  | Cons(k, rest) ->
    let c = (compare key k) in
    if c < 0 then s else
      if c = 0 then rest else Cons(k, remove_b key rest )
  | Node(l, v, r, _) ->
      let c = compare key v in
      if c = 0 then merge_t l r else
      if c < 0 then bal_t (remove_b key l) v r else 
                bal_t l v (remove_b key r)

let rec iter_b f = function
    Empty -> ()
  | Cons(k, rest) -> f k; iter_b f rest;
  | Node(l, v, r, _) -> iter_b f l; f v; iter_b f r

let rec fold_b f s accu =
  match s with
    Empty -> accu
  | Cons(k, rest) -> fold_b f rest (f k accu)
  | Node(l, v, r, _) -> fold_b f r (f v (fold_b f l accu))

let rec for_all_b p s = match s with
    Empty -> true
  | Cons(k, rest) ->  p k && for_all_b p rest
  | Node(l, v, r, _) -> p v && for_all_b p l && for_all_b p r

let rec exists_b p = function
    Empty -> false
  | Cons(k, rest) -> p k || exists_b p rest
  | Node(l, v, r, _) -> p v || exists_b p l || exists_b p r



let tree_of_list_b s =
  let get_pair = function
    Empty -> invalid_arg "HashSet.tree_of_list"
  | Cons(k, rest) -> 
      Node(Empty, k, Empty, 1), rest
  | Node(_,_,_,_) -> invalid_arg "HashSet.tree_of_list"
  in
  let n, l = get_pair s in
  let rn = ref n in
  iter_b (fun k -> rn := add_t k  !rn) l;
  !rn

(*   *)

let empty () =
    {v_keys = [| |];
    v_occ = [| 0 |];
     size = 0;
     data = [| Empty |];
    }

(* k0 default key; needed to create v_keys
 *  with the required type. When v_occ.(i) is zero, v_keys.(i)
 *  is a fake key, which is k0 or some other value.
 *)
let create initial_size k0 = 
  let s = min (max 1 initial_size) Sys.max_array_length in
  { v_keys = Array.make s k0;
    v_occ = Array.make s 0;
    size = 0;
    data = Array.make s Empty
  }


let length h = h.size
let capacity h = Array.length h.v_keys

let mem h key =
  let i =
    (hash key) mod (Array.length h.v_keys) 
  in
  print_endline "Found pouet !!";
  if h.v_occ.(i) > 0 then
    begin
      let c = (compare key h.v_keys.(i)) in
      if c < 0 then false
      else if c = 0 then true
      else mem_b key h.data.(i)
    end
  else
    false

(* for efficiency reasons the keys in v_keys are not deleted,
 * but "unlinked" setting the corresponding h.v_occ.(i) is zero
 *)
let clear h =
  if length h <> 0 then
    for i = 0 to Array.length h.v_keys - 1 do
      h.v_occ.(i) <- 0;
      h.data.(i) <- Empty;
    done;
    h.size <- 0

(* make a copy, keeping the same internal structure *)
let copy h =
  if length h = 0 then empty() else
    begin
    let n = Array.length h.data in
    let nv_keys = Array.make n h.v_keys.(0) in
    let nv_occ = Array.make n 0 in
    let ndata = Array.make n Empty in
    for i = 0 to n - 1 do
      if h.v_occ.(i) > 0 then
        begin
        nv_keys.(i) <- h.v_keys.(i);
        nv_occ.(i) <- h.v_occ.(i);
        ndata.(i) <- h.data.(i);
        end
    done;
    { v_keys = nv_keys;
      v_occ = nv_occ;
      size = h.size;
      data = ndata}
    end



(* iterate on the buckets *)    
let iter_buckets f h =
  let d = h.data in
  for i = 0 to Array.length d - 1 do
    iter_b f d.(i)
  done

(* iterate on v_keys *)
let iter_v f h =
  let v_occ = h.v_occ in
  for i = 0 to Array.length h.v_keys - 1 do
    if v_occ.(i) > 0 then 
      f h.v_keys.(i);
  done
  
let iter f h =
  iter_v f h;
  iter_buckets f h


      
(* 
 * It is "unsafe" in the sense that no checking is made that
 * no resizing is made.
 *)
let add_unsafe h key =
  let i = (hash key) mod (Array.length h.v_keys) in
  if h.v_occ.(i) = 0 then 
    begin
    h.v_occ.(i) <- 1; 
    h.v_keys.(i) <- key;
    h.size <- succ h.size
    end
  else 
    begin
    (* convert to tree if the list is too long *)
    if h.v_occ.(i) > limit_list && (isList_b h.data.(i)) then
      h.data.(i) <- tree_of_list_b h.data.(i);
    let c = (compare key h.v_keys.(i)) in
    if (c < 0) then
      begin
      h.size <- succ h.size;
      h.v_occ.(i) <- succ h.v_occ.(i);
      h.data.(i) <- add_b h.v_keys.(i) h.data.(i);
      h.v_keys.(i) <- key;
      end
    else if (c > 0) then
      begin
      if not (mem_b key h.data.(i)) then
        begin
        h.size <- succ h.size;
        h.v_occ.(i) <- succ h.v_occ.(i);
        h.data.(i) <- add_b key h.data.(i)
        end
      end
    end

(* nsize is supposed to be >= (length h) *)
let copy_resize h nsize =
  let h2 = create nsize h.v_keys.(0) in
  iter (fun key -> add_unsafe h2 key) h;
  h2

(* resize to (2 * oldsize + 1)  *)
let resize h key =
  let osize = Array.length h.v_keys in
  let nsize = min (2 * osize + 1) Sys.max_array_length in
  if osize = 0 then
    begin
    h.v_keys <- [| key |];
    h.v_occ <- [| 0 |];
    h.size <- 0;
    h.data <- [| Empty |];
    end
  else if nsize <> osize then
    begin
    let hc = copy_resize h nsize in
    h.v_keys <- hc.v_keys;
    h.v_occ <- hc.v_occ;
    h.size <- hc.size;
    h.data <- hc.data;
    end

let add h key =
  if h.size >= (Array.length h.v_keys) then resize h key;
  add_unsafe h key


(* move the first key in data in place of the one in v_keys *) 
let move_hd_in_bucket h i = match  h.data.(i) with
  Cons(k1, next) ->
    begin
    h.data.(i) <- next;
    h.v_keys.(i) <- k1;
    end
  | Node(l, v, r, n) as t -> 
      let k1 = min_elt_t t in
      h.data.(i) <- remove_b k1 t;
      h.v_keys.(i) <- k1  
  | Empty -> ()


let remove h key =
  let i = (hash key) mod (Array.length h.v_keys) in
  if h.v_occ.(i) > 0  then
    begin
    let c = (compare key h.v_keys.(i)) in
    if (c = 0) then
      begin
      h.size <- pred h.size ;
      h.v_occ.(i) <- pred h.v_occ.(i);
      if h.data.(i) <> Empty then 
        move_hd_in_bucket h i;
      end
    else if c > 0 then
      (* remove key from data *)
      if mem_b key h.data.(i) then
        begin
        h.size <- pred h.size ;
        h.v_occ.(i) <- pred h.v_occ.(i);
        h.data.(i) <- remove_b key  h.data.(i)
        end
    end

let of_list lst =
  let h = empty() in
  List.iter (fun x -> add h x) lst;
  h
  
let bucket_lengths h = 
  let num_bucketr = ref 0 in
  for i = 0 to Array.length h.v_occ -1 do
    if h.v_occ.(i) > 0 then incr num_bucketr;
  done;
  let a = Array.make !num_bucketr 0 in
  let c = ref 0 in
  for i = 0 to Array.length h.v_occ -1 do
    if h.v_occ.(i) > 0 then
      begin
      a.(!c) <- h.v_occ.(i);
      incr c;
      end
  done;
  a

let fold f h init =
  let accu = ref init in
  iter (fun key -> (accu := f key !accu)) h;
  !accu
  
  
let for_all p h =
  let predq key =
    if (not (p key)) then
      raise Not_found
  in
  let res = ref true in
  let _ = try iter predq h with
  | Not_found -> res := false in
  !res

let exists p h =
  let predq key =
    if (p key) then
      raise Not_found  (* Found, really *)
  in
  let res = ref false in
  let _ = try iter predq h with
  | Not_found -> res := true in
  !res
  
  
let keys h =
  let a = Array.make (h.size) h.v_keys.(0) in
  let i = ref 0 in
  iter (fun key -> a.(!i) <- key; incr i) h;
  a
  
let equal h other =
  if h.size <> other.size then
    false
  else
    let memq key =
      if (not (mem other key)) then
        raise Not_found
    in
    let res = ref true in
    let _ = try iter memq h with
    | Not_found -> res := false in
    !res
   

(* set operations, inplace *)
let update h other =
  let add_to key = add h key in
  iter add_to other
  
let diff_update h other =
  iter_buckets (fun x -> remove h x) other;
  iter_v (fun x -> remove h x) other

let symmetric_diff_update h other =
  let xor key =
    if mem h key then
      remove h key
    else
      add h key
  in
  iter xor other
    
let inter_update h other =
  let f key =
        if not (mem other key) then
          begin 
          remove h key;
          end
  in
  (* removing first the keys in data is faster, since removing
   * keys from v_keys.(i) when there are other keys in the bucket is slow
   *)
  iter_buckets f h;
  iter_v f h


(* set operations, non inplace *)
let union h other =
  let r = copy h in
  update r other;
  r

let diff h other =
  let r = copy h in
  diff_update r other;
  r

let symmetric_diff h other =
  let r = copy h in
  symmetric_diff_update r other;
  r


let inter h other =
  let r = copy h in
  inter_update r other;
  r


(* Functorial interface *)

module type HashedType =
  sig
    type t
    val compare: t -> t -> int
    val hash: t -> int
  end

module type S =
  sig
    type elt
    type t

    
    val empty : unit -> t
    val create: int -> elt -> t
    val length : t -> int
    val capacity : t -> int
    val mem : t -> elt -> bool
    val clear: t -> unit
    val copy : t -> t
    val iter_v: (elt -> unit) -> t -> unit
    val iter: (elt -> unit) -> t -> unit
    val add_unsafe :  t -> elt -> unit
    val copy_resize : t -> int -> t
    val resize : t -> elt -> unit 
    val add: t -> elt -> unit
    val remove: t -> elt -> unit
    val of_list : elt list -> t
    val bucket_lengths : t -> int array
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val keys : t -> elt array
    val equal : t -> t -> bool
    val update : t -> t -> unit
    val diff_update : t -> t -> unit
    val symmetric_diff_update : t -> t -> unit
    val inter_update : t -> t -> unit
    val union : t -> t -> t
    val diff : t -> t -> t
    val symmetric_diff : t -> t -> t
    val inter : t -> t -> t
  end

module Make(H: HashedType): (S with type elt = H.t) =
struct
  type elt = H.t
  type set = elt t
  type t = set

  let safehash key = (H.hash key) land max_int

  let isList_b = isList_b
  let height_t = height_t

  let create_t = create_t

  let bal_t = bal_t

  let rec add_t x = function
  Empty -> 
    Node(Empty, x, Empty, 1);
    | Node(l, v, r, _) as t ->
      let c = H.compare x v in
      if c = 0 then t else
        if c < 0 then bal_t (add_t x l) v r else bal_t l v (add_t x r)
    | Cons(_,_) -> invalid_arg "HashSet.add_t"
      
      
  let rec add_b x = function
  Empty -> 
    Cons(x, Empty)
    | Cons(k, rest) as t ->
      let c = (H.compare x k) in
      if c < 0 then Cons(x, t) else 
        if c = 0 then t 
        else Cons(k, add_b x rest)
    | Node(l, v, r, _) as t ->
      let c = (H.compare x v) in
      if c = 0 then t else
        if c < 0 then bal_t (add_t x l) v r 
        else bal_t l v (add_t x r)
    
  let min_elt_t = min_elt_t
  let remove_min_elt_t = remove_min_elt_t
  let merge_t = merge_t

  let rec mem_b key = function
    | Empty ->
      false
    | Cons(k, rest) ->
      let c = H.compare key k in
      if c = 0 then true
      else if c < 0 then false
      else
        mem_b key rest
    | Node(l, v, r, _) ->
      let c = H.compare key v in
      c = 0 || mem_b key (if c < 0 then l else r) 

  let rec remove_b key s = match s with
      Empty -> Empty
    | Cons(k, rest) ->
      let c = (H.compare key k) in
      if c < 0 then s else
  if c = 0 then rest else Cons(k, remove_b key rest )
    | Node(l, v, r, _) ->
      let c = H.compare key v in
      if c = 0 then merge_t l r else
  if c < 0 then bal_t (remove_b key l) v r else 
          bal_t l v (remove_b key r)


  let iter_b = iter_b
  let fold_b = fold_b
  let for_all_b = for_all_b
  let exists_b = exists_b
  let bucket_lengths = bucket_lengths


  let tree_of_list_b s =
    let get_pair = function
    Empty -> invalid_arg "HashSet.tree_of_list"
      | Cons(k, rest) -> 
        Node(Empty, k, Empty, 1), rest
      | Node(_,_,_,_) -> invalid_arg "HashSet.tree_of_list"
    in
    let n, l = get_pair s in
    let rn = ref n in
    iter_b (fun k -> rn := add_t k  !rn) l;
    !rn

      
  let empty = empty
  let create = create
  let length = length
  let capacity = capacity

  let mem h key =
    if Array.length h.v_keys = 0 then
      false
    else
      let i = (safehash key) mod (Array.length h.v_keys) in
      if h.v_occ.(i) > 0 then
  begin
    let c = (H.compare key h.v_keys.(i)) in
    if c < 0 then false
    else if c = 0 then true
    else mem_b key h.data.(i)
  end
      else
  begin
    false
  end
  
  let clear = clear
  let copy = copy
  let iter_buckets = iter_buckets
  let iter_v = iter_v
  let iter = iter

  let add_unsafe h key =
    let i = (safehash key) mod (Array.length h.v_keys) in
    if h.v_occ.(i) = 0 then
      begin
  h.v_occ.(i) <- 1;
  h.v_keys.(i) <- key;
  h.size <- succ h.size
      end
    else
      begin
  (*  convert to tree if the list is too long *)
  if h.v_occ.(i) > limit_list && (isList_b h.data.(i)) then
          h.data.(i) <- tree_of_list_b h.data.(i);
  let c = (H.compare key h.v_keys.(i)) in
  if (c < 0) then
          begin
            h.size <- succ h.size;
            h.v_occ.(i) <- succ h.v_occ.(i);
            h.data.(i) <- add_b h.v_keys.(i) h.data.(i);
            h.v_keys.(i) <- key;
          end
  else if (c > 0) then
          begin
            if not (mem_b key h.data.(i)) then
              begin
    h.size <- succ h.size;
    h.v_occ.(i) <- succ h.v_occ.(i);        
    h.data.(i) <- add_b key h.data.(i)
              end
          end
      end

  let copy_resize h nsize =
    let h2 = create nsize h.v_keys.(0) in
    iter (fun key -> add_unsafe h2 key) h;
    h2
      
  let resize h key =
    let osize = Array.length h.v_keys in
    let nsize = min (2 * osize + 1) Sys.max_array_length in
    if osize = 0 then
      begin
  h.v_keys <- [| key |];
  h.v_occ <- [| 0 |];
  h.size <- 0;
  h.data <- [| Empty |];
      end
    else if nsize <> osize then
      begin
  let hc = copy_resize h nsize in
  h.v_keys <- hc.v_keys;
  h.v_occ <- hc.v_occ;
  h.size <- hc.size;
  h.data <- hc.data;
      end
  
  
  let add h key =
    if h.size >= (Array.length h.v_keys) then resize h key;
    add_unsafe h key
      
  let move_hd_in_bucket h i = match  h.data.(i) with
      Cons(k1, next) ->
  begin
    h.data.(i) <- next;
    h.v_keys.(i) <- k1;
  end
    | Node(l, v, r, n) as t -> 
      let k1 = min_elt_t t in
      h.data.(i) <- remove_b k1 t;
      h.v_keys.(i) <- k1  
    | Empty -> ()

  let remove h key =
    let i = (safehash key) mod (Array.length h.v_keys) in
    if h.v_occ.(i) > 0  then
      begin
  let c = (H.compare key h.v_keys.(i)) in
  if (c = 0) then
          begin
            h.size <- pred h.size ;
            h.v_occ.(i) <- pred h.v_occ.(i);
            if h.data.(i) <> Empty then
              move_hd_in_bucket h i ;
          end
  else if c > 0 then
          (* remove key from data *)
          if mem_b key h.data.(i) then
            begin
              h.size <- pred h.size ;
              h.v_occ.(i) <- pred h.v_occ.(i);
              h.data.(i) <- remove_b key  h.data.(i)
            end
      end
  
  let of_list lst =
    let h = empty() in
    List.iter (fun x -> add h x) lst;
    h
      
      
  let fold = fold
  let for_all = for_all
  let exists = exists
  let keys = keys

  let equal h other =
    if h.size <> other.size then
      false
    else
      let memq key =
        if (not (mem other key)) then
          raise Not_found
      in
      let res = ref true in
      let _ = try iter memq h with
  | Not_found -> res := false in
      !res

  (* set operations, inplace *)
  let update h other =
    let add_to key = add h key in
    iter add_to other
      
  let diff_update h other =
    iter_buckets (fun x -> remove h x) other;
    iter_v (fun x -> remove h x) other
      
  let symmetric_diff_update h other =
    let xor key =
      if mem h key then
        remove h key
      else
        add h key
    in
    iter xor other
      
  let inter_update h other =
    let f key =
      if not (mem other key) then
        begin 
          remove h key;
        end
    in
    iter_buckets f h;
    iter_v f h
      
  (* set operations, non inplace *)
  let union h other =
    let r = copy h in
    update r other;
    r
      
  let diff h other =
    let r = copy h in
    diff_update r other;
    r
      
  let symmetric_diff h other =
    let r = copy h in
    symmetric_diff_update r other;
    r
      
      
  let inter h other =
    let r = copy h in
    inter_update r other;
    r
      
end
