
open Printf

type t =
| No_parallelization
| Parmap of int * int

let to_string parallelization_mode =
  (match parallelization_mode with
   | No_parallelization -> "No_parallelization"
   | Parmap (number_of_cores, chunk_size) -> (sprintf "Parmap with %d cores and a chank size of %d" number_of_cores chunk_size)
  )
