
open Printf

type t =
  {
    mutable total_nb_elements: int;
    mutable nb_elements_processed : int;
    mutable nb_elements_by_step : int;

    mutable size_total : int;
    mutable size_processed_before : int;
    mutable size_processed_now : int;

    mutable time_start : float;
    mutable time_now : float;
    mutable time_before : float;
  }

let new_t
    total_nb_elements
    nb_elements_processed
    nb_elements_by_step

    size_total
    size_processed_before
    size_processed_now

    time_start
    time_now
    time_before
  =
  {
    total_nb_elements = total_nb_elements ;
    nb_elements_processed = nb_elements_processed;
    nb_elements_by_step = nb_elements_by_step;

    size_total = size_total;
    size_processed_before = size_processed_before;
    size_processed_now = size_processed_now;

    time_start = time_start;
    time_now = time_now;
    time_before = time_before;
  }

(* let new_empty_t () = *)
(*   new_t *)
(*     0 *)
(*     0 *)
(*     0 *)

(*     0 *)
(*     0 *)
(*     0 *)

(*     0.0 *)
(*     0.0 *)
(*     0.0 *)

let of_nb_elements_size total_nb_elements size_total =
  let nb_elements_by_step =
    if total_nb_elements > 100 then
      (
        total_nb_elements / 100;
      )
    else
      1
  in

  new_t
    total_nb_elements
    0
    nb_elements_by_step

    size_total
    0
    0

    (Unix.gettimeofday ())
    (Unix.gettimeofday ())
    (Unix.gettimeofday ())

let display_data
    ?(display_size = false)
    ?(add_new_line = false)
    percentage_processed_float
    throughput
    time_elapsed
    t
  =
  (
    (* assert(percentage_processed_float <= 100.); *)
    if percentage_processed_float > 100. then
      (
        print_endline
          (sprintf  
             "Progress_manager: display_data: percentage_processed_float (%f) > 100"
             percentage_processed_float
          );
        assert(false)
      );

    let total_numbers_of_characters = 20 in
    let number_of_characters = int_of_float ((percentage_processed_float *. (float_of_int total_numbers_of_characters)) /. 100.) in

    let base_string = Bytes.make total_numbers_of_characters ' ' in
    Bytes.fill base_string 0 number_of_characters '#';

    print_string
      (sprintf
         "\rProgress_manager: display_data: [%s] ; %.1f%%%s in %fs%s"
         base_string
         percentage_processed_float
         (if display_size then
            sprintf
              " ; %f Mo/s "
              throughput
          else
            "")
         time_elapsed
         (if add_new_line then
            "\n"
          else
            ""
         )
      );

    flush_all ();
  )

let display_stat ?(display_size = false) t =
  (
    t.time_now <- Unix.gettimeofday ();
    
    let percentage_processed_float = ((float_of_int t.nb_elements_processed) /. (float_of_int t.total_nb_elements)) *. 100.0 in
    
    let time_elapsed = t.time_now -. t.time_before in
    let trace_processed = t.size_processed_now - t.size_processed_before in
    
    let throughput = ((float_of_int trace_processed) /. time_elapsed) /. (1024.0 *. 1024.0) in
    
    display_data
      ~display_size
      percentage_processed_float
      throughput
      time_elapsed
      t;

    t.time_before <- t.time_now;
    t.size_processed_before <- t.size_processed_now;
  )

let update_and_display_stat ?(display_size = false) t element_size =
  (
    t.nb_elements_processed <- t.nb_elements_processed + 1;
    t.size_processed_now <- t.size_processed_now + element_size;

    if ( ((t.nb_elements_processed mod t.nb_elements_by_step ) = 0) && (t.nb_elements_processed <> 0) ) then
      (
        display_stat ~display_size t;
      );
  )

let final_display
    ?(display_size = false)
    t
    =
  (
    t.time_now <- Unix.gettimeofday ();

    let time_elapsed = t.time_now -. t.time_start in

    let throughput = ((float_of_int t.size_total) /. time_elapsed) /. (1024.0 *. 1024.0) in

    display_data
      ~display_size
      ~add_new_line: true
      100.
      throughput
      time_elapsed
      t;
  )
    
