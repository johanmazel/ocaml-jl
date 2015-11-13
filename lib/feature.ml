
open Printf

open Sexplib.Std
open Bin_prot.Std

type t =
  {
    indice : int;
    name : string;
  }
with sexp, bin_io

let new_t
    indice
    name
    =
  {
    indice = indice;
    name = name;
  }

let new_empty_t
    ()
    =
  {
    indice = 0;
    name = "";
  }

let to_indice_string t = string_of_int t.indice
                  
let to_name t = t.name
    
let to_string
    t
  =
  sprintf
    "%d-%s"
    t.indice
    t.name
      
let compare t1 t2 = compare t1.indice t2.indice
