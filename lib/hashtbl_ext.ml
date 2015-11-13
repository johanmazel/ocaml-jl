
open Printf

include BatHashtbl

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

let to_string
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
