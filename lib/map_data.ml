
open Printf

module A = BatArray
  
(* TODO: factorize with map_my_list *)

let debug_enabled = ref true

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
  (fun s -> Format.printf "[Map_data]: %s@." s)
      else
  ignore
    )
    fmt

let master
    ref_result_list
    (_)
    element
  =  
  ref_result_list := Batteries.List.append [ element ] !ref_result_list;

  []

let map_sequence
    parallelization_mode
    f
    f_data_to_list
    f_data_of_list
    f_data_to_sequence
    f_data_map
    f_data_length
    data
  =
  debug "map_sequence: call";

  debug
    "map_sequence: parallelization_mode: %s"
    (Parallelization_mode.to_string parallelization_mode)
  ;

  let result =
    match parallelization_mode with
    | Parallelization_mode.No_parallelization ->
      (
        let progress_manager =
          Progress_manager.of_nb_elements_size
            (f_data_length data)
            0
        in

        Progress_manager.display_stat progress_manager;

        let result =
          f_data_map
            (fun element ->
               Progress_manager.update_and_display_stat progress_manager 0;
               f element
            )
            data
        in

        Progress_manager.final_display progress_manager;

        result
      )
    | Parallelization_mode.Parmap (number_of_cores, chunk_size) ->
      (
        debug "map_sequence: building sequence";

        Parmap.debugging !debug_enabled;

        let sequence = f_data_to_sequence data in

        debug "map: parmap";

        let sequence =
          Parmap.parmap
            ~ncores: number_of_cores
            ~chunksize: chunk_size
            f
            sequence
        in

        f_data_of_list sequence
      )
  in

  debug "map: end";

  result

let map_list
    parallelization_mode
    (f: ('a -> 'b))
    (l: 'a list)
  =
  (
    debug "map_list: call";

    debug
      "map_list: parallelization_mode: %s"
      (Parallelization_mode.to_string parallelization_mode)
    ;

    let result =
      map_sequence
        parallelization_mode
        f
        (fun list -> list)
        (fun list -> list)
        (fun list -> Parmap.L list)
        Batteries.List.map
        Batteries.List.length
        l
    in

    debug "map_list: end";

    (result: 'b list)
  )

let fold_list
    parallelization_mode
    f
    reduce_f
    acc_init 
    list
  =
  (
    debug "fold_list: call";

    debug
      "fold_list: parallelization_mode: %s"
      (Parallelization_mode.to_string parallelization_mode)
    ;

    let result =
      match parallelization_mode with
      | Parallelization_mode.No_parallelization ->
        (
          let progress_manager =
            Progress_manager.of_nb_elements_size
              (List.length list)
              0
          in

          Progress_manager.display_stat progress_manager;

          let result =
            Batteries.List.fold_left
              (fun acc element ->
                 Progress_manager.update_and_display_stat progress_manager 0;
                 f acc element
              )
              acc_init
              list
          in

          Progress_manager.final_display progress_manager;

          result
        )
      | Parallelization_mode.Parmap (number_of_cores, chunk_size) ->
        (
          debug "fold_list: building sequence";

          Parmap.debugging !debug_enabled;

          let sequence = Parmap.L list in

          debug "fold_list: parmap";

          Parmap.parfold
            ~ncores: number_of_cores
            ~chunksize: chunk_size
            (fun acc element -> f element acc)
            sequence
            acc_init 
            reduce_f 
        )
    in

    debug "fold_list: end";

    result
  )

let fold_array
    parallelization_mode
    f
    reduce_f
    acc_init 
    array
  =
  (
    debug "fold: call";

    debug
      "fold: parallelization_mode: %s"
      (Parallelization_mode.to_string parallelization_mode)
    ;

    let result =
      match parallelization_mode with
      | Parallelization_mode.No_parallelization ->
        (
          Batteries.Array.fold_left
            f
            acc_init
            array
        )
      | Parallelization_mode.Parmap (number_of_cores, chunk_size) ->
        (
          debug "fold: building sequence";

          Parmap.debugging !debug_enabled;

          let sequence = Parmap.A array in

          debug "fold: parmap";

          Parmap.parfold
            ~ncores: number_of_cores
            ~chunksize: chunk_size
            (fun acc element -> f element acc)
            sequence
            acc_init 
            reduce_f 
        )
    in

    debug "fold: end";

    result
  )

let iter_list
    parallelization_mode
    f
    list
  =
  (
    debug "iter: call";

    debug
      "iter: parallelization_mode: %s"
      (Parallelization_mode.to_string parallelization_mode)
    ;

    let result =
      match parallelization_mode with
      | Parallelization_mode.No_parallelization ->
        (
          Batteries.List.iter
            f
            list
        )
      | Parallelization_mode.Parmap (number_of_cores, chunk_size) ->
        (
          debug "iter: building sequence";

          Parmap.debugging !debug_enabled;

          let sequence = Parmap.L list in

          debug "iter: parmap";

          Parmap.pariter
            ~ncores: number_of_cores
            ~chunksize: chunk_size
            f
            sequence
        )
    in

    debug "iter: end";

    result
  )

let map_array
    parallelization_mode
    (f: ('a -> 'b))
    a
  =
  (
    debug "map_array: call";

    debug
      "map_array: parallelization_mode: %s"
      (Parallelization_mode.to_string parallelization_mode)
    ;

    let result =
      map_sequence
        parallelization_mode
        f
        Array.to_list
        Array.of_list
        (fun array -> Parmap.A array)
        Batteries.Array.map
        Batteries.Array.length
        a
    in

    debug "map_array: end";

    (result: 'b array)
  )

let slice_subarray
    subarray_size
    a
  =
  (
    let length = A.length a in

    let slice_number_float = float_of_int length /. float_of_int subarray_size in
    let slice_number = int_of_float (ceil slice_number_float) in

    let last_slice_length = length mod subarray_size in

    let slice_indice_a = A.init slice_number (fun i -> i) in

    debug
      "test_map_subarray: length: %d ; slice_number: %d ; last_slice_length: %d"
      length
      slice_number
      last_slice_length
    ;

    debug
      "map_subarray: slice_indice_a:\n%s"
      (Array_ext.to_string
         ~sep: "  "
         string_of_int
         slice_indice_a
      );

    let a_a =
      A.map
        (fun slice_indice ->
           let slice_start = slice_indice * subarray_size in

           debug "test_map_subarray: slice_indice: %d ; slice_start: %d" slice_indice slice_start;

           if slice_indice < slice_number - 1 then
             A.sub a slice_start subarray_size
           else
             (
               debug "map_subarray: last slice";
               if last_slice_length = 0 then
                 A.sub a slice_start subarray_size
               else
                 A.sub a slice_start last_slice_length                 
             )
        )
        slice_indice_a
    in

    a_a    
  )

let flatten_array_array
    a_a
  =
  let a_l = A.to_list a_a in

  let a = A.concat a_l in

  a

let test_map_subarray
    ()
  =
  (
    let a = A.init 17 (fun i -> i) in

    let a_a = slice_subarray 25 a in

    debug
      "test_map_subarray: sliced_array:\n%s"
      (Array_ext.to_string
         ~sep: "\n"
         (fun a ->
            (Array_ext.to_string
               ~sep: "  "
               string_of_int
               a
            )
         )
         a_a
      )
    ;

    let a_merged = flatten_array_array a_a in

    debug
      "map_subarray: sliced_array:\n%s"
      (Array_ext.to_string
         ~sep: "  "
         string_of_int
         a_merged
      );
  )

let map_subarray_legacy___
    parallelization_mode
    subarray_size
    (f : 'a array -> 'c)
    (g : ('c -> 'a -> 'b))
    (a : 'a array)
  =
  (
    debug "map_subarray: call";

    debug
      "map_subarray: parallelization_mode: %s"
      (Parallelization_mode.to_string parallelization_mode)
    ;

    let a_a = slice_subarray subarray_size a in

    debug "map_subarray: mapping";

    let a_a_mapped =
      A.map
        (fun subarray ->
           debug "map_subarray: subarray length: %d" (A.length subarray);

           let data = f a in

           let result =
             map_sequence
               parallelization_mode
               (g data)
               Array.to_list
               Array.of_list
               (fun array -> Parmap.A array)
               Batteries.Array.map
               Batteries.Array.length
               subarray
           in

           result
        )
        a_a
    in

    let a_mapped =
      flatten_array_array
        a_a_mapped
    in

    debug "map_subarray: end";

    (a_mapped: 'b array)
  )

let map_subarray
    parallelization_mode
    subarray_size
    (f : 'a array -> 'c)
    (g : ('c -> 'a -> 'b))
    (a : 'a array)
  =
  (
    debug "map_subarray: call";

    debug
      "map_subarray: parallelization_mode: %s"
      (Parallelization_mode.to_string parallelization_mode)
    ;

    let data = f a in

    let a_a = slice_subarray subarray_size a in

    debug "map_subarray: mapping";

    let a_a_mapped =
      map_sequence
        parallelization_mode
        (fun subarray ->
           let result =
             A.map
               (g data)               
               subarray
           in

           result
        )
        Array.to_list
        Array.of_list
        (fun array -> Parmap.A array)
        Batteries.Array.map
        Batteries.Array.length
        a_a
    in

    let a_mapped =
      flatten_array_array
        a_a_mapped
    in

    debug "map_subarray: end";

    (a_mapped: 'b array)
  )
