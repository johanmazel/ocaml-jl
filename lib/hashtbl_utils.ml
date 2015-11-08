
module type HASHTBL_TYPE = sig

  type ('a, 'b) t
    
    
  val add : ('a, 'b) t -> 'a -> 'b -> unit
    
  val find : ('a, 'b) t -> 'a -> 'b
    
  val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c    
  val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
    
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
        (fun key1 element1 bool ->
           try
             (
               let element2 =
               Hashtbl_type.find
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
        Hashtbl_type.fold
        (fun key2 element2 bool ->
           try
             (
               let element1 =
               Hashtbl_type.find
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
