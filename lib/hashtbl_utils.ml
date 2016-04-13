
(* module L = List_ext *)

module type HASHTBL_TYPE = sig

  type ('a, 'b) t
        
  val add : ('a, 'b) t -> 'a -> 'b -> unit
    
  val find : ('a, 'b) t -> 'a -> 'b
    
  val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c    
  val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit

  val to_list : ('a, 'b) t -> ('a * 'b) list
    
end

module Make = functor (Hashtbl_type : HASHTBL_TYPE) -> struct

  let compare
      element_compare
      hashtable1
      hashtable2
      =
    (
      let result12 =
        Hashtbl_type.fold
          (fun key element1 bool ->
            try
              (
		let element2 =
		  Hashtbl_type.find
                    hashtable2
                    key
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
        Hashtbl_type.fold
          (fun key element2 bool ->
	    try
	      (
		let element1 =
		  Hashtbl_type.find
		    hashtable1
		    key
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
      
  let compare_diff_key
      element_compare
      hashtable1
      hashtable2
      =
    (
      let result12, option12 =
        Hashtbl_type.fold
          (fun key element1 (bool, option) ->
            try
              (
		let element2 =
		  Hashtbl_type.find
                    hashtable2
                    key
		in

		let b =
		  if element_compare element1 element2 = 0 then
		    true
		  else
		    false
		in
		
		(bool
		 &&
		   b)
		  ,		
		(
		  if bool = false then
		    option
		  else
		    if b then
		      None
		    else
		      Some key
		)
	      )
            with
            | Not_found ->
	       (
		 false, Some key
	       )
          )
	  hashtable1
	  (true, None)
      in

      let result21, option21 =
        Hashtbl_type.fold
          (fun key element2 (bool, option) ->
	    try
	      (
		let element1 =
		  Hashtbl_type.find
		    hashtable1
		    key
		in

		let b =
		  if element_compare element1 element2 = 0 then
		    true
		  else
		    false
		in
		
		(bool
		 &&
		   b)
		  ,		
		(
		  if bool = false then
		    option
		  else
		    if b then
		      None
		    else
		      Some key
		)
	      )
	    with
	    | Not_found ->
	       (
		 false, Some key
	       )
          )
          hashtable2
          (true, None)
      in

      let option =
	if result12 == false then
	  option12
	else
	  if result21 == false then
	    option21
	  else
	    None
      in
      
      if result12 && result21 then
        0, option
      else
        1, option
    )

  let diff
      key_comparator
      element_comparator
      
      t1
      t2
      =
    (
      let m1 =
	Core.Std.Map.of_alist_exn
	  ~comparator: key_comparator
	  (Hashtbl_type.to_list
	     t1
	  )
      in
      let m2 =
	Core.Std.Map.of_alist_exn
	  ~comparator: key_comparator
	  (Hashtbl_type.to_list
	     t2
	  )
      in
      
      let d =
	Core.Std.Map.symmetric_diff
	  ~data_equal: element_comparator
	  m1
	  m2
      in

      (* let diff_t = *)
      (* 	Core.Std.Map.to_alist *)
      (* 	  r *)
      (* in *)

      (* diff_t *)

      d
	
      (* assert(1) *)
    )
      
  let append
      data_compare
      h
      h_to_append
    =
    Hashtbl_type.iter
      (fun key data ->
         try
           (
             let data_found =
               Hashtbl_type.find
                 h
                 key
             in

             assert(data_compare
                      data_found
                      data
                    =
                    0);
           )
         with
         | Not_found ->
           (
             Hashtbl_type.add h key data
           )
      )
      h_to_append;

    h
  
end
