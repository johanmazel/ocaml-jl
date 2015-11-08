
let fusion
    fusion_function
    t_1_option
    t_2_option
    =
  match t_1_option with
  | None -> t_2_option
  | Some t_1 ->
    (
      match t_2_option with
      | None -> Some t_1
      | Some t_2 ->
	Some
	  (fusion_function
	     t_1
	     t_2)
    )

let append
    append_function
    change_function
    t_option
    t_to_append_option
    =
  match t_to_append_option with
  | None -> ()
  | Some t_to_append ->
    (
      match t_option with
      | None -> 
	change_function
	  t_to_append
      | Some t ->
	append_function
	  t
	  t_to_append
    )


