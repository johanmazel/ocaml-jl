
open Printf

(*let to_string_dynarray                                                               *)
(*		?first                                                                           *)
(*		?last                                                                            *)
(*		?sep                                                                             *)
(*		to_string_element                                                                *)
(*		dynarray                                                                         *)
(*=                                                                                    *)
(*	let array_of_dynarray = DynArray.to_array dynarray in                              *)
(*	                                                                                   *)
(*	let first_default = "" in                                                          *)
(*	let last_default = "" in                                                           *)
(*	let sep_default = "" in                                                            *)
(*	                                                                                   *)
(*	let first_to_use =                                                                 *)
(*		(                                                                                *)
(*			let ref_first_to_return = ref first_default in                                 *)
(*			Option.may (fun first -> ref_first_to_return := first) (first : string option);*)
(*			!ref_first_to_return                                                           *)
(*		)                                                                                *)
(*	in                                                                                 *)
(*	                                                                                   *)
(*	let last_to_use =                                                                  *)
(*		(                                                                                *)
(*			let ref_last_to_return = ref last_default in                                   *)
(*			Option.may (fun last -> ref_last_to_return := last) (last : string option);    *)
(*			!ref_last_to_return                                                            *)
(*		)                                                                                *)
(*	in                                                                                 *)
(*	                                                                                   *)
(*	let sep_to_use =                                                                   *)
(*		(                                                                                *)
(*			let ref_sep_to_return = ref sep_default in                                     *)
(*			Option.may (fun value -> ref_sep_to_return := value) sep;                      *)
(*			!ref_sep_to_return                                                             *)
(*		)                                                                                *)
(*	in                                                                                 *)
(*	                                                                                   *)
(*	Array.sprint                                                                       *)
(*		~first: first_to_use                                                             *)
(*		~last: last_to_use                                                               *)
(*		~sep_element: sep_to_use                                                                 *)
(*		(fun output value -> IO.nwrite output (to_string_element value))                 *)
(*		array_of_dynarray                                                                *)

let to_string_dynarray
    ?first
    ?last
    ?sep
    to_string
    (dynarray : 'a Batteries.DynArray.t)
    =
  let first_default = "" in
  let last_default = "" in
  let sep_default = "" in
  
  let first_to_use =
    (
      let ref_first_to_return = ref first_default in
      Batteries.Option.may (fun first -> ref_first_to_return := first) (first : string option);
      !ref_first_to_return
    )
  in
  
  let last_to_use =
    (
      let ref_last_to_return = ref last_default in
      Batteries.Option.may (fun last -> ref_last_to_return := last) (last : string option);
      !ref_last_to_return
    )
  in
  
  let sep_to_use =
    (
      let ref_sep_to_return = ref sep_default in
      Batteries.Option.may (fun value -> ref_sep_to_return := value) sep;
      !ref_sep_to_return
    )
  in
  
  let innerIO_output = Batteries.IO.output_string () in
  
  Batteries.DynArray.print
    ~first: first_to_use
    ~last: last_to_use
    ~sep: sep_to_use
    (fun innerIO element ->
      (
	Batteries.IO.nwrite innerIO (to_string element);
      )
    )
    innerIO_output
    dynarray;
  
  Batteries.IO.close_out innerIO_output

(*let to_string_hashtbl                                                                *)
(*		?first                                                                           *)
(*		?last                                                                            *)
(*		?sep                                                                             *)
(*		to_string_element                                                                *)
(*		hashtbl                                                                          *)
(*=                                                                                    *)
(*	let first_default = "" in                                                          *)
(*	let last_default = "" in                                                           *)
(*	let sep_default = "" in                                                            *)
(*	                                                                                   *)
(*	let first_to_use =                                                                 *)
(*		(                                                                                *)
(*			let ref_first_to_return = ref first_default in                                 *)
(*			Option.may (fun first -> ref_first_to_return := first) (first : string option);*)
(*			!ref_first_to_return                                                           *)
(*		)                                                                                *)
(*	in                                                                                 *)
(*	                                                                                   *)
(*	let last_to_use =                                                                  *)
(*		(                                                                                *)
(*			let ref_last_to_return = ref last_default in                                   *)
(*			Option.may (fun last -> ref_last_to_return := last) (last : string option);    *)
(*			!ref_last_to_return                                                            *)
(*		)                                                                                *)
(*	in                                                                                 *)
(*	                                                                                   *)
(*	let sep_to_use =                                                                   *)
(*		(                                                                                *)
(*			let ref_sep_to_return = ref sep_default in                                     *)
(*			Option.may (fun value -> ref_sep_to_return := value) sep;                      *)
(*			!ref_sep_to_return                                                             *)
(*		)                                                                                *)
(*	in                                                                                 *)
(*	                                                                                   *)
(*	(*		let values_enum_of_hastbl = BatHashtbl.values hashtbl in*) *)
(*	                                                                                   *)
(*	let ref_string = ref "" in                                                         *)
(*	                                                                                   *)
(*	ref_string := !ref_string ^ first_to_use;                                          *)
(*	                                                                                   *)
(*	Hashtbl.iter                                                                       *)
(*		(fun _ elt ->                                                                    *)
(*					(                                                                          *)
(*						Utils.append_result_of_function_to_string_to_ref_string                  *)
(*						(*								~first: first_to_use*) *)
(*						(*								~last: last_to_use  *) *)
(*							~sep_element: sep_to_use                                                       *)
(*							ref_string                                                             *)
(*							(fun element -> (to_string_element element))                           *)
(*							elt                                                                    *)
(*					)                                                                          *)
(*		)                                                                                *)
(*		hashtbl;                                                                         *)
(*	                                                                                   *)
(*	ref_string := !ref_string ^ last_to_use;                                           *)
(*	                                                                                   *)
(*	!ref_string                                                                        *)

(* let to_string_hashtbl_old                                                                   *)
(* 		?first                                                                                  *)
(* 		?last                                                                                   *)
(* 		?sep                                                                                    *)
(* 		?to_string_key                                                                          *)
(* 		to_string_element                                                                       *)
(* 		hashtbl                                                                                 *)
(* =                                                                                           *)
(* 	let first_default = "" in                                                                 *)
(* 	let last_default = "" in                                                                  *)
(* 	let sep_default = "" in                                                                   *)
	
(* 	let first_to_use =                                                                        *)
(* 		match first with                                                                        *)
(* 		| None -> ""                                                                            *)
(* 		| Some string -> string                                                                 *)
(* 	(*		(                                                                                *) *)
(* 	(*			let ref_first_to_return = ref first_default in                                 *) *)
(* 	(*			Option.may (fun first -> ref_first_to_return := first) (first : string option);*) *)
(* 	(*			!ref_first_to_return                                                           *) *)
(* 	(*		)                                                                                *) *)
(* 	in                                                                                        *)
	
(* 	let last_to_use =                                                                         *)
(* 		match last with                                                                         *)
(* 		| None -> ""                                                                            *)
(* 		| Some string -> string                                                                 *)
(* 	(*		(                                                                            *)     *)
(* 	(*			let ref_last_to_return = ref last_default in                               *)     *)
(* 	(*			Option.may (fun last -> ref_last_to_return := last) (last : string option);*)     *)
(* 	(*			!ref_last_to_return                                                        *)     *)
(* 	(*		)                                                                            *)     *)
(* 	in                                                                                        *)
	
(* 	let sep_to_use =                                                                          *)
(* 		match sep with                                                                          *)
(* 		| None -> ""                                                                            *)
(* 		| Some string -> string                                                                 *)
(* 	(*		(                                                          *)                       *)
(* 	(*			let ref_sep_to_return = ref sep_default in               *)                       *)
(* 	(*			Option.may (fun value -> ref_sep_to_return := value) sep;*)                       *)
(* 	(*			!ref_sep_to_return                                       *)                       *)
(* 	(*		)                                                          *)                       *)
(* 	in                                                                                        *)
	
(* 	let to_string_key =                                                                       *)
(* 		match to_string_key with                                                                *)
(* 		| None -> (fun _ -> "")                                                                 *)
(* 		| Some to_string_key -> to_string_key                                                   *)
(* 	in                                                                                        *)
	
(* 	let innerIO_output = BatInnerIO.output_string () in                                       *)
	
(* 	Hashtbl.print                                                                             *)
(* 		~first: first_to_use                                                                    *)
(* 		~last: last_to_use                                                                      *)
(* 		~sep: sep_to_use                                                                        *)
(* 		(fun innerIO key -> BatInnerIO.nwrite innerIO (to_string_key key))                      *)
(* 		(fun innerIO value -> BatInnerIO.nwrite innerIO (to_string_element value))              *)
(* 		innerIO_output                                                                          *)
(* 		hashtbl;                                                                                *)
	
(* 	BatInnerIO.close_out innerIO_output                                                       *)

let to_string_hashtbl_conversion
    ?first
    ?last
    ?sep
    ?to_string_key
    to_string_element
    hashtbl
    =
  let first_default = "" in
  let last_default = "" in
  let sep_default = "" in
  
  let first_to_use =
    (
      let ref_first_to_return = ref first_default in
      Batteries.Option.may (fun first -> ref_first_to_return := first) (first : string option);
      !ref_first_to_return
    )
  in
  
  let last_to_use =
    (
      let ref_last_to_return = ref last_default in
      Batteries.Option.may (fun last -> ref_last_to_return := last) (last : string option);
      !ref_last_to_return
    )
  in
  
  let sep_to_use =
    (
      let ref_sep_to_return = ref sep_default in
      Batteries.Option.may (fun value -> ref_sep_to_return := value) sep;
      !ref_sep_to_return
    )
  in
  
  let to_string_key =
    match to_string_key with
    | None -> (fun _ -> "")
    | Some to_string_key -> to_string_key
  in
  
  let enum = Batteries.Hashtbl.enum hashtbl in
  let array = Batteries.Array.of_enum enum in
  let list = Batteries.Array.to_list array in
  
  let innerIO_output = Batteries.IO.output_string () in
  
  Batteries.List.print
    ~first: first_to_use
    ~last: last_to_use
    ~sep: sep_to_use
    (fun output tuple ->
					(* Batteries.IO.nwrite output *)
      Batteries.IO.nwrite output
	(
	  let key = fst tuple in
	  let value = snd tuple in
	  (to_string_key key) ^ (to_string_element value)
	)
    )
    innerIO_output
    list;
  
  Batteries.IO.close_out innerIO_output

let to_string_hashtbl_fold
    ?first
    ?last
    ?sep_element
    ?sep_key_value
    ?to_string_key
    to_string_value
    hashtbl
    =
  let first_to_use =
    match first with
    | None -> ""
    | Some string -> string
  in
  
  let last_to_use =
    match last with
    | None -> ""
    | Some string -> string
  in
  
  let sep_element_to_use =
    match sep_element with
    | None -> ""
    | Some string -> string
  in
  
  let sep_key_value_to_use =
    match sep_key_value with
    | None -> ": "
    | Some string -> string
  in
  
  let to_string_key =
    match to_string_key with
    | None -> (fun _ -> "")
    | Some to_string_key -> to_string_key
  in
  
  let hashtbl_string =
    Hashtbl.fold
      (fun key value result ->
	result ^ Printf.sprintf "%s%s%s%s"
	  (to_string_key key)
	  sep_key_value_to_use
	  (to_string_value value)
	  sep_element_to_use
      )
      hashtbl
      first_to_use
  in
  
  hashtbl_string ^ last_to_use

let print_hashtbl
    ?(first ="{\n")
    ?(last ="\n}")
    ?(sep_element =",\n")
    ?(sep_key_value =": ")
    print_k
    print_v
    out
    t
    =
  Batteries.Enum.print
    ~first
    ~last
    ~sep: sep_element
    (fun out (k, v) ->
      BatPrintf.fprintf
	out
	"%a%s%a"
	print_k k
	sep_key_value
	print_v v)
    out
    (Batteries.Hashtbl.enum t)

let to_string_hashtbl
    ?(first = "")
    ?(last = "")
    ?(sep_element = "\n")
    ?(sep_key_value = ": ")
    ?(to_string_key = fun key -> "")
    to_string_value
    hashtbl
    =
  let innerIO_output = Batteries.IO.output_string () in
  
  print_hashtbl
    ~first
    ~last
    ~sep_element
    ~sep_key_value
    (fun innerIO key -> Batteries.IO.nwrite innerIO (to_string_key key))
    (fun innerIO value -> Batteries.IO.nwrite innerIO (to_string_value value))
    innerIO_output
    hashtbl;
  
  Batteries.IO.close_out innerIO_output

let to_string_list
    ?first: (first = "")
    ?last: (last = "")
    ?sep: (sep = " ")
    to_string_value
    list
    =
  let innerIO_output = Batteries.IO.output_string () in
  
  Batteries.List.print
    ~first: first
    ~last: last
    ~sep: sep
    (fun output value ->
      Batteries.IO.nwrite output
	(to_string_value value)
    )
    innerIO_output
    list;
  
  Batteries.IO.close_out innerIO_output

let to_string_array
    ?first: (first = "")
    ?last: (last = "")
    ?sep: (sep = " ")
    to_string_value
    list
    =
  let innerIO_output = Batteries.IO.output_string () in
  
  Batteries.Array.print
    ~first: first
    ~last: last
    ~sep: sep
    (fun output value ->
      Batteries.IO.nwrite output
	(to_string_value value)
    )
    innerIO_output
    list;
  
  Batteries.IO.close_out innerIO_output

(* let to_string_bitset bitset                             *)
(* =                                                       *)
(* 	let innerIO_output = Batteries.IO.output_string () in *)
	
(* 	BitSet.print innerIO_output bitset;                   *)
	
(* 	Batteries.IO.close_out innerIO_output                 *)

let to_string_indice_bitset bitset
    =
  let enum = Batteries.BitSet.enum bitset in
  let list = Batteries.List.of_enum enum in
  (* let indice_value_list = List.mapi (fun indice value -> (indice , value)) list in *)
  
  List.fold_left
    (fun acc indice ->
      (* match value with                                                                                           *)
      (* | 0 -> acc                                                                                                 *)
      (* | 1 -> acc ^ " " ^ (string_of_int indice)                                                                  *)
      (* | _ -> failwith (sprintf "Utils_batteries: to_string_indice_bitset: invalid value at %d: %d" indice value) *)
      acc ^ " " ^ (string_of_int indice)
    )
    ""
    list

let to_string_set
    ?first: (first = "")
    ?last: (last = "")
    ?sep: (sep = "")
    to_string_value
    set
    =
  let innerIO_output = Batteries.IO.output_string () in
  
  Batteries.Set.print
    ~first: first
    ~last: last
    ~sep: sep
    (fun output value ->
      (* Batteries.IO.nwrite output *)
      Batteries.IO.nwrite output
	(to_string_value value)
    )
    innerIO_output
    set;
  
  Batteries.IO.close_out innerIO_output

(* let to_string_imap                                                                      *)
(* 		?first: (first = "")                                                                *)
(* 		?last: (last = "")                                                                  *)
(* 		?sep: (sep = "")                                                                    *)
(* 		?(sep_key_value =": ")                                                              *)
(* 		to_string_key                                                                       *)
(* 		to_string_value                                                                     *)
(* 		map                                                                                 *)
(* =                                                                                       *)
(* 	let list = Maps.Int_batmap.bindings map in                                            *)
	
(* 	to_string_list                                                                        *)
(* 		~first: first                                                                       *)
(* 		~last: last                                                                         *)
(* 		~sep: sep                                                                           *)
(* 		(fun (key, value) -> (to_string_key key) ^ sep_key_value ^ (to_string_value value)) *)
(* 		list                                                                                *)

(* let to_string_imap_old                                                                        *)
(* 		?first: (first = "")                                                                      *)
(* 		?last: (last = "")                                                                        *)
(* 		?sep: (sep = "")                                                                          *)
(* 		?(sep_key_value =": ")                                                                    *)
(* 		to_string_key                                                                             *)
(* 		to_string_value                                                                           *)
(* 		map                                                                                       *)
(* =                                                                                             *)
(* 	let result = first in                                                                       *)

(* 	let result =                                                                                *)
(* 		Batteries.IMap.fold                                                                       *)
(* 			(fun key value string ->                                                                *)
(* 						match result with                                                                 *)
(* 						| first ->                                                                        *)
(* 						(* print_endline "Utils_batteries: to_string_imap: first"; *) *)
(* 								result ^ (to_string_key key) ^ sep_key_value ^ (to_string_value value)        *)
(* 						| _ ->                                                                            *)
(* 						(* print_endline "Utils_batteries: to_string_imap: other"; *) *)
(* 								result ^	sep ^ (to_string_key key) ^ sep_key_value ^ (to_string_value value) *)
(* 			)                                                                                       *)
(* 			map                                                                                     *)
(* 			result                                                                                  *)
(* 	in                                                                                          *)

(* 	result ^ last                                                                               *)

let compare_hashtable
    element_compare
    hashtable1
    hashtable2
    =
  (
    let result12 =
      Hashtbl.fold
	(fun key1 element1 bool ->
	  try
	    (
	      let element2 =
		Hashtbl.find
		  hashtable2
		  key1
	      in

	      bool
	      &&
		(
		  if element_compare element1 element2 = 0 then
		    true
		  else
		    false
		)
	    )
	  with
	  | Not_found ->
	    (
	      false
	    )
	)
	hashtable1
	true
    in

    let result21 =
      Hashtbl.fold
	(fun key2 element2 bool ->
	  try
	    (
	      let element1 =
		Hashtbl.find
		  hashtable1
		  key2
	      in

	      bool
	      &&
		(
		  if element_compare element1 element2 = 0 then
		    true
		  else
		    false
		)
	    )
	  with
	  | Not_found ->
	    (
	      false
	    )
	)
	hashtable2
	true
    in

    if result12 && result21 then
      0
    else
      1
  )

let shuffle_list l =
  let nd = Batteries.List.map (fun c -> (Random.bits (), c)) l in
  let sond = Batteries.List.sort compare nd in
  Batteries.List.map snd sond
    
