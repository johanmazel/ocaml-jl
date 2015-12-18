
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
