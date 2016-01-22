
open Printf

include BatArray

let to_string
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

let shuffle a =
  (* Fisher–Yates shuffle *)
  for i = pred (Array.length a) downto 1 do
    let j = Random.int (succ i) in
    if i <> j (* faster to omit this test with arrays of about 100000 elements or more *) then (
      let tmp = Array.unsafe_get a i in
      Array.unsafe_set a i (Array.unsafe_get a j);
      Array.unsafe_set a j tmp
    )
  done

let fold_succ f acc a =
  if length a = 0 then
    acc
  else
    let a_wo_first = sub a 1 (length a - 1) in

    let previous_ref = ref (a.(0)) in

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
        a_wo_first
    in

    result
