
module Feature_map = Map.Make(Feature);;

let to_string_feature_map
    ?first: (first = "")
    ?last: (last = "")
    ?sep_element: (sep = "")
    ?sep_key_value: (sep_key_value = ": ")
    to_string_key
    to_string_value
    map
  =
  let list = Feature_map.bindings map in

  List_ext.to_string
    ~first: first
    ~last: last
    ~sep: sep
    (fun (key, value) -> (to_string_key key) ^ sep_key_value ^ (to_string_value value))
    list
