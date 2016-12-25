
open Printf

open Sexplib.Std
open Bin_prot.Std

type t =
| Int of int
| Int32 of int32
| String of string
[@@deriving compare, sexp, bin_io]
(* with compare, sexp, bin_io *)

let to_string
      t
  =
  match t with
  | Int x -> string_of_int x
  | Int32 x -> Int32.to_string x
  | String string -> string

let equal t1 t2 = compare t1 t2 = 0

let get_int_exn t =
  match t with
  | Int int -> int
  | Int32 int32 -> 
    print_endline
      (sprintf
         "Metric_value: get_int_exn: found int32: %s"
         (to_string t)
      );
    raise Not_found
  | String string -> 
    print_endline
      (sprintf
         "Metric_value: get_int_exn: found string: %s"
         (to_string t)
      );
    raise Not_found
  
let get_int32_exn t =
  match t with
  | Int int -> 
    print_endline
      (sprintf
         "Metric_value: get_int32_exn: found int: %s"
         (to_string t)
      );
    raise Not_found
  | Int32 int32 -> int32
  | String string ->  
    print_endline
      (sprintf
         "Metric_value: get_int32_exn: found string: %s"
         (to_string t)
      );
    raise Not_found

let get_string_exn t =
  match t with
  | Int int ->
    print_endline
      (sprintf
         "Metric_value: get_string_exn: found int: %s"
         (to_string t)
      );
    raise Not_found
  | Int32 int32 ->
    print_endline
      (sprintf
         "Metric_value: get_string_exn: found int32: %s"
         (to_string t)
      );
    raise Not_found
  | String string -> string
