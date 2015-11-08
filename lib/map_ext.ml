
open Printf

let debug_enabled = ref true

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
        (fun s -> Format.printf "[Map_ext]: %s@." s)
      else
        ignore
    )
    fmt

module type Key = sig

  type t

  val compare : t -> t -> int

  val to_string : t -> string
end

module Make (Key : Key) = struct

  include Map.Make(Key)

  let of_list l = List_ext.fold_left (fun acc (key, value) -> add key value acc) empty l
  let of_list_no_dup l = 
    List.fold_left 
      (fun acc (key, data) -> 
	if mem key acc then
	  (
	    print_endline (sprintf "Map_ext: %s already in map" (Key.to_string key));
	    assert(false)
	  );
	add key data acc
      ) 
      empty
      l
  let to_list = bindings

  let fold_succ f t acc =
    if is_empty t then
      acc
    else
      let map_without_first =
        remove
          (fst (min_binding t))
          t
      in

      let previous_key_value_tuple_ref = ref (min_binding t) in

      let result =
        fold
          (fun key value acc ->
             let new_acc =
               f
                 !previous_key_value_tuple_ref
                 (key, value)
                 acc
             in

             previous_key_value_tuple_ref := (key, value);

             new_acc
          )
          map_without_first
          acc
      in

      result
        
  let to_string
      ?sep: (sep = " ")
      to_string_key_value
      t
    =
    let list = bindings t in

      List_ext.to_string
        ~sep
        (fun (key, value) ->
           to_string_key_value key value
        )
        list

end
