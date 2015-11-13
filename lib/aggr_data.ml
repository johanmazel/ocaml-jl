
open Printf

module type Key = sig

  type simple_t
  val to_string_simple : simple_t -> string
  val to_simple_filename_prefix : simple_t -> string
  val compare_simple :  simple_t ->  simple_t -> int

  type aggr_mode_t
  val to_string_aggr_mode : aggr_mode_t -> string

  type aggr_t
  val to_string_aggr : aggr_t -> string
  val to_aggr_filename_prefix : aggr_t -> string
  val of_aggr_mode_simple_key : aggr_mode_t -> simple_t -> aggr_t
  val of_aggr_mode_aggr_key : aggr_mode_t -> aggr_t -> aggr_t
  val compare_aggr :  aggr_t ->  aggr_t -> int

end

module type Data = sig

  type t
  val copy : t -> t
  val append : t -> t -> unit
  val fusion : t -> t -> t
  val to_string : t -> string
    
end

module Make (Key : Key) (Data : Data) = struct

  let debug_enabled = ref false
    
  let set_debug bool = debug_enabled := bool
    
  let debug fmt =
    Printf.kprintf
      (
        if !debug_enabled then
          (fun s -> Format.printf "%s@." s)
        else
          ignore
      )
      fmt
      
  module Simple_key_data_container = struct
      
    type t =
      {
        simple_key_data_hashtable : (Key.simple_t , Data.t) Hashtbl.t;
      }
    (* with bin_io *)
  
    let new_t
        simple_key_data_hashtable
      =
      {
        simple_key_data_hashtable = simple_key_data_hashtable;
      }
  
    let new_empty_t
        hashtable_size
      =
      {
        simple_key_data_hashtable = Hashtbl.create hashtable_size;
      }
  
    let to_string t =
      Hashtbl_ext.to_string
        ~sep_element: "\n"
        ~sep_key_value: "\n"
        ~to_string_key: Key.to_string_simple
        Data.to_string
        t.simple_key_data_hashtable
  
    let length t = Hashtbl.length t.simple_key_data_hashtable

    let add t date data =
      Hashtbl.add
        t.simple_key_data_hashtable
        date
        data
  
    let find t key_simple =
      Hashtbl.find
        t.simple_key_data_hashtable
        key_simple
  
    let replace t key_simple data =
      Hashtbl.replace
        t.simple_key_data_hashtable
        key_simple
        data
  
    let of_simple_key_data_tuple_list
        tuple_list
      =
      let t = new_empty_t (List.length tuple_list) in

      List.iter
        (fun (date, data) ->
           assert(Hashtbl.mem t.simple_key_data_hashtable date = false);    
           add
             t
             date
             data
        )
        tuple_list;

      t
  
    let of_simple_key_data_hashtable
        hashtable
      =
      new_t
        hashtable
  
    let fusion t_1 t_2 =
      let t_to_return = 
        new_empty_t 
          (max (length t_1) (length t_2))
      in

      Hashtbl.iter
        (fun simple_key timed_data ->
           (
             try(
               let timed_data_found =
                 Hashtbl.find
                   t_2.simple_key_data_hashtable
                   simple_key
               in

               let data_fusionned =
                 Data.fusion
                   timed_data
                   timed_data_found
               in

               add
                 t_to_return
                 simple_key
                 data_fusionned
             )
             with
             | Not_found ->
               (

               )
           )
        )
        t_1.simple_key_data_hashtable;

      t_to_return
  
    let iter f (t : t) =
      Batteries.Hashtbl.iter
        (fun key data ->
           f key data)
        t.simple_key_data_hashtable

    let fold f t acc_init =
      let new_acc =
        Batteries.Hashtbl.fold
          (fun key_simple data acc ->
             f 
               key_simple
               data
               acc
          )
          t.simple_key_data_hashtable
          acc_init
      in

      new_acc
    
    let to_list t =
      let enum = Batteries.Hashtbl.enum t.simple_key_data_hashtable in
      let list = Batteries.List.of_enum enum in
      Batteries.List.map
        (fun (key_simple, key_simple_data) -> (key_simple, key_simple_data))
        list

    let get_prefix date =
      let filename_prefix =
        "date_" ^ Core.Date.to_string_iso8601_basic date
      in

      filename_prefix
  
    let iter_filename_prefix f t =
      Batteries.Hashtbl.iter
        (fun key_simple data ->
           let prefix = Key.to_simple_filename_prefix key_simple in

           f data prefix
        )
        t.simple_key_data_hashtable
  
    let filter
        f
        t
      =
      let hashtable =
        Batteries.Hashtbl.filter
          f
          t.simple_key_data_hashtable
      in

      new_t
        hashtable
  
    let filteri
        f
        t
      =
      let hashtable =
        Batteries.Hashtbl.filteri
          f
          t.simple_key_data_hashtable
      in

      new_t
        hashtable
  
    let map f t =
      let hashtable =
        Batteries.Hashtbl.map
          (fun date data ->
             (
               f date data
             )
          )
          t.simple_key_data_hashtable
      in

      new_t
        hashtable

    let map_to_hashtbl f t =
      Batteries.Hashtbl.map
        (fun key data ->
           (
             f key data
           )
        )
        t.simple_key_data_hashtable
    
  end

  module Aggr_key_data_inter =  struct
      
    type t =
      {
        aggr_key : Key.aggr_t;
        mutable data_list : Data.t list;
      }
  
    let new_t
        aggr_key
        data_list
      =
      {
        aggr_key = aggr_key;
        data_list = data_list;
      }
  
    let new_empty_t
        aggr_key
      =
      {
        aggr_key = aggr_key;
        data_list = [];
      }
  
    let to_string to_string_mode t =
      sprintf
        "%s\n%s"
        (Key.to_string_aggr t.aggr_key)
        (List_ext.to_string
           ~first: ""
           ~last: ""
           ~sep: " "
           Data.to_string
           t.data_list
        )
  
    let add_data
        t
        data
      =
      t.data_list <- data :: t.data_list
  
  
    let add_data
        t
        data
      =
      t.data_list <- data :: t.data_list
    
    let to_aggr_key_data_tuple_append
        t
      =
      (
        debug "Aggr_key_data_inter: to_aggr_key_data_tuple: call";

        assert(List.length t.data_list > 0);

        debug
          "Aggr_key_data_inter: to_aggr_key_data_tuple: date %s with %d elements"
          (Key.to_string_aggr t.aggr_key)
          (List.length t.data_list)
        ;

        let first_data = 
          Data.copy
            (List.hd 
               t.data_list) 
        in

        let remaining_data_list = List.tl t.data_list in

        Batteries.List.iter
          (fun data ->
             let data_to_use =
               (* Data.copy *)
               data
             in

             Data.append
               first_data
               data_to_use
          )
          remaining_data_list;

        let data = first_data in

        debug "Aggr_key_data_inter: to_aggr_key_data_tuple: end";

        (
          t.aggr_key
          ,
          data
        )
      )
  
  end

  module Aggr_key_map = Map.Make(struct
    type t = Key.aggr_t
    let compare t1 t2 = Key.compare_aggr t1 t2
  end);;

  let to_string_aggr_key_map
      ?(first = "")
      ?(last = "")
      ?(sep = "")
      ?(sep_key_value =": ")
      to_string_key
      to_string_value
      map
      =
    let list = Aggr_key_map.bindings map in
    
    List_ext.to_string
      ~first: first
      ~last: last
      ~sep: sep
      (fun (key, value) -> (to_string_key key) ^ sep_key_value ^ (to_string_value value))
      list

  module Aggr_key_data_container = struct

    type t =
      {
        key_aggr_mode : Key.aggr_mode_t;  
        hashtable : (Key.aggr_t , Data.t) Hashtbl.t;
      }
  
    let new_t
        key_aggr_mode
        hashtable
      =
      {
        key_aggr_mode = key_aggr_mode;
        hashtable = hashtable;
      }
  
    let new_empty_t
        key_aggr_mode
      =
      new_t
        key_aggr_mode
        (Hashtbl.create 0)
  
    let to_map t =
      Hashtbl.fold
        (fun aggr_key data map ->
           Aggr_key_map.add
             aggr_key
             data
             map
        )
        t.hashtable
        Aggr_key_map.empty

    let to_list t =
      let map = to_map t in
      Aggr_key_map.bindings map

    let to_string t =
      (Hashtbl_ext.to_string
         ~sep_element: "\n"
         Data.to_string
         t.hashtable)

    let to_string_ordered
        ?(to_string_data = Data.to_string)
        t
        (* : To_string_mode.t -> ?to_string_data: (To_string_mode.t -> Data.t -> string) -> t -> string *)
      =
      let map = to_map t in

      to_string_aggr_key_map
        ~sep: "\n"
        ~sep_key_value: ": "
        Key.to_string_aggr
        to_string_data
        map
  
    let length t = Hashtbl.length t.hashtable

    let get_key_aggr_mode t = t.key_aggr_mode

    let add
        t
        aggr_key
        data
      =
      Hashtbl.add
        t.hashtable
        aggr_key
        data
  
    let of_aggr_key_data_hashtable
        key_aggr_mode
        hashtable
      =
      (
        new_t
          key_aggr_mode
          hashtable
      )

    let of_simple_key_data_container
        key_aggr_mode
        simple_key_data_container
      =
      (
        debug "Aggr_key_data_container: of_simple_key_data_container: call";

        debug
          "Aggr_key_data_container: of_simple_key_data_container: simple_key_data_hashtable length %d"
          (Simple_key_data_container.length simple_key_data_container)
        ;

        let temporary_hashtable = Hashtbl.create 1 in

        Simple_key_data_container.iter
          (fun simple_key data ->
             (
               let aggr_key =
                 Key.of_aggr_mode_simple_key
                   key_aggr_mode
                   simple_key
               in

               try(
                 let trace_evaluation_statistics_inter_found =
                   Hashtbl.find
                     temporary_hashtable
                     aggr_key
                 in

                 Aggr_key_data_inter.add_data
                   trace_evaluation_statistics_inter_found
                   data;
               )
               with
               | Not_found ->
                 (
                   let new_trace_evaluation_statistics_inter =
                     Aggr_key_data_inter.new_empty_t
                       aggr_key
                   in

                   Aggr_key_data_inter.add_data
                     new_trace_evaluation_statistics_inter
                     data;

                   Hashtbl.add
                     temporary_hashtable
                     aggr_key
                     new_trace_evaluation_statistics_inter;
                 )
             )
          )
          simple_key_data_container;

        debug
          "Aggr_key_data_container: of_simple_key_data_container: temporary_hashtable length %d"
          (Hashtbl.length temporary_hashtable)
        ;

        let hashtable =
          Batteries.Hashtbl.map
            (fun aggr_key aggr_key_data_inter ->
               let aggr_key_, data =
                 Aggr_key_data_inter.to_aggr_key_data_tuple_append
                   aggr_key_data_inter
               in
               data
            )
            temporary_hashtable
        in

        debug "Aggr_key_data_container: of_simple_key_data_container: end";

        new_t
          key_aggr_mode
          hashtable
      )
  
    let of_t
        key_aggr_mode
        t
      =
      (
        debug
          "Aggregated_timed_data_container: of_t: call"
        ;

        let hashtable = Hashtbl.copy t.hashtable in

        debug
          "Aggregated_timed_data_container: of_t: hashtable length %d"
          (Hashtbl.length hashtable)
        ;

        (* TODO: assert that aggr_mode is included in t.aggr_mode *)

        let temporary_hashtable = Hashtbl.create 1 in

        Hashtbl.iter
          (fun
            aggr_key_base
            data
            ->
              (
                let aggr_key =
                  Key.of_aggr_mode_aggr_key
                    key_aggr_mode
                    aggr_key_base
                in

                try(
                  let trace_evaluation_statistics_inter_found =
                    Hashtbl.find
                      temporary_hashtable
                      aggr_key
                  in

                  Aggr_key_data_inter.add_data
                    trace_evaluation_statistics_inter_found
                    (* aggregated_timed_data *)
                    data
                  ;
                )
                with
                | Not_found ->
                  (
                    let aggr_key_data_inter =
                      Aggr_key_data_inter.new_empty_t
                        aggr_key
                    in


                    Aggr_key_data_inter.add_data
                      aggr_key_data_inter
                      data
                    ;

                    Hashtbl.add 
                      temporary_hashtable 
                      aggr_key
                      aggr_key_data_inter;
                  )
              )
          )
          hashtable;

        debug
          "Aggregated_timed_data_container: of_t: temporary_hashtable length %d"
          (Hashtbl.length temporary_hashtable)
        ;

        let hashtable =
          Batteries.Hashtbl.map
            (fun aggr_key aggr_key_data_inter ->
               let aggr_key, data =
                 Aggr_key_data_inter.to_aggr_key_data_tuple_append
                   aggr_key_data_inter
               in

               data
            )
            temporary_hashtable
        in

        debug "Aggregated_timed_data_container: of_t: end";

        new_t
          key_aggr_mode
          hashtable
      )
  
    let iter f t =
      Batteries.Hashtbl.iter
        f
        t.hashtable
        
    let iter_filename_prefix f t =
      Batteries.Hashtbl.iter
        (fun aggr_key data ->
           (
             let filename_prefix =
               Key.to_string_aggr aggr_key
             in

             f data filename_prefix
           )
        )
        t.hashtable
        
    (* TODO: add key *)
    let map_to_hashtbl f t =
      Batteries.Hashtbl.map
        (fun _ data ->
           (
             f data
           )
        )
        t.hashtable
        
    let filter f t =
      let hashtbl_filtered =
        Batteries.Hashtbl.filter
          (fun data ->
             (
               f data
             )
          )
          t.hashtable
      in

      new_t
        t.key_aggr_mode
        hashtbl_filtered
      
    let fold
        f
        t
        acc
      =
      Hashtbl.fold
        (fun aggr_key data -> f aggr_key data)
        t.hashtable
        acc

    let map_to_self f t =
      let new_hashtable =
        Batteries.Hashtbl.map
          (fun aggr_key data -> f data)
          t.hashtable
      in

      new_t
        t.key_aggr_mode
        new_hashtable
  
  end
    
end

