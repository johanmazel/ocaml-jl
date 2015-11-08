
open Printf

include BatList

let to_string
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

(* TODO: replace by fold_left *)
let fold_succ f l acc =
  if length l = 0 then
    acc
  else
    let l_wo_first = tl l in

    let previous_ref = ref (hd l) in

    let result =
      fold_left
        (fun acc current ->
           let new_acc =
             f
               acc
               !previous_ref
               current
           in

           previous_ref := current;

           new_acc
        )
        acc
        l_wo_first
    in

    result

(* let group_consecutive_succ f l = *)
(*   let last_group, l_l = *)
(*     fold_succ  *)
(*       (fun previous current acc -> *)
(*          let current_group, l_l = acc in *)

(*          let new_acc = *)
(*            if f previous current then *)
(*              current :: current_group, l_l *)
(*            else *)
(*              [ current ], current_group :: l_l *)
(*          in *)

(*          new_acc *)
(*       )       *)
(*       l *)
(*       ([ hd l ], []) *)
(*   in *)

(*   last_group :: l_l *)
