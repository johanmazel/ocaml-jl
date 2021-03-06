
open Printf

let debug_enabled = ref true

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
        (fun s -> Format.printf "[Set_ext]: %s@." s)
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

  include Set.Make(Key)

  let of_list = List_ext.fold_left (fun acc x -> add x acc) empty
  let to_list = elements

  let fold_succ f s acc =
    if is_empty s then
      acc
    else
      let s_wo_first =
        remove
          (min_elt s)
          s
      in

      let previous_ref = ref (min_elt s) in

      let result =
        fold
          (fun current acc ->
             let new_acc =
               f
                 !previous_ref
                 current
                 acc
             in

             previous_ref := current;

             new_acc
          )
          s_wo_first
          acc
      in

      result

  let to_string
      ?sep: (sep = " ")
      t
    =
    let list = elements t in

    List_ext.to_string
      ~sep
      Key.to_string
      list

end
