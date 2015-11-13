
open Printf
open Bigarray

open Feature_data_structures

exception Incompatible_length

type t =
  {
    length : int;
    feature_array : Feature.t array;
  }

let new_t
    feature_array
    =
  {
    length = Array.length feature_array;
    feature_array = feature_array;
  }

let new_empty_t
    ()
    =
  {
    length = 0;
    feature_array = Array.make 0 (Feature.new_empty_t ());
  }

let to_string t =
  Core.Core_list.to_string
    Feature.to_indice_string
    (Array.to_list t.feature_array)

let length t =
	Array.length t.feature_array

let get t indice = Array.get t.feature_array indice

let of_feature_list
    parameters
    feature_list
    =	
  let feature_container =
    new_t
      (Array.of_list feature_list)
  in
  
  feature_container

let of_feature_indice_list_and_global_feature_container
    parameters
    global_feature_array
    feature_indice_list
    =
  (    
    let feature_list =
      Batteries.List.map
	(fun attribute_indice -> Array.get global_feature_array attribute_indice)
	feature_indice_list
    in
    
    let feature_container =
      new_t
	(Array.of_list feature_list)
    in
    
    feature_container
  )

let of_feature_indice_list_and_global_feature_container
    global_feature_container
    feature_indice_list
  =
  (
    let nb_feature = global_feature_container.length in

    let feature_list =
      Batteries.List.map
	(fun feature_indice ->
	   assert(feature_indice < nb_feature - 1);
	   get global_feature_container feature_indice)
	feature_indice_list
    in

    let feature_container =
      new_t
	(Array.of_list feature_list)
    in

    feature_container
  )

let nth
    t
    position
  =
  Array.get
    t.feature_array
    position

let iter (function_to_apply : Feature.t -> unit) t =
  Array.iter
    function_to_apply
    t.feature_array

let fold_left
    f
    acc
    t
  =
  Array.fold_left
    f
    acc
    t.feature_array

let unique
    t
  =
  let feature_list = Array.to_list t.feature_array in

  let feature_list_unique = Batteries.List.unique feature_list in

  let feature_array_unique = Array.of_list feature_list_unique in

  new_t
    feature_array_unique

let fusion
    t_1
    t_2
  =
  let array = Array.append t_1.feature_array t_2.feature_array in

  let new_t =
    new_t
      array
  in

  unique new_t

let fusion
    t_1
    t_2
  =
  let array = Array.append t_1.feature_array t_2.feature_array in

  new_t
    array

let mapi_to_array
    f
    t
  =
  Array.mapi
    f
    t.feature_array

let of_string_array
    string_array
  =
  let feature_array =
    Batteries.Array.mapi
      (fun indice name -> Feature.new_t indice name)
      string_array
  in

  new_t
    feature_array

let to_bit_set
    t
    global_t
  =
  let bitset = Batteries.BitSet.create global_t.length in

  iter
    (fun feature ->
       (
	 let indice = feature.Feature.indice in

	 Batteries.BitSet.set bitset indice;
       )
    )
    t;

  bitset

let of_bit_set_and_global_feature_container
    bitset
    t
  =
  let enum = Batteries.BitSet.enum bitset in
  let list = Batteries.List.of_enum enum in

  let feature_list =
    Batteries.List.map
      (fun indice ->
	 let feature = get t indice in
	 feature
      )
      list
  in

  new_t
    (Array.of_list feature_list)

let to_feature_map
    t
  =
  Array.fold_left
    (fun feature_map feature ->
       Feature_map.add
	 feature
	 feature
	 feature_map
    )
    Feature_map.empty
    t.feature_array
