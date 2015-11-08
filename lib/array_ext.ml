
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
