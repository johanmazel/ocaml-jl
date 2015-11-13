
open Printf
open Thread
  
let rec delay_my_thread time =
  let time_before = Unix.gettimeofday () in
  try
    (
      Thread.delay time;
    )
  with
  | Unix.Unix_error (error, function_name, parameter) ->
    (
      let time_now = Unix.gettimeofday () in
      print_endline
	(sprintf
	   "Utils: delay_my_thread: error \"%s\" in function \"%s\" with parameters \"%s\" !!!!!!!!!!!!!!"
	   (Unix.error_message error)
	   function_name
	   parameter
	);

      let delay_time_remaining = time -. (time_now -. time_before) in

      delay_my_thread delay_time_remaining;
    )

let rec no_error_unix_function f =
  (
    try
      (
	f
      )
    with
    | Unix.Unix_error (error, function_name, parameter) ->
      (
	Format.fprintf
	  Format.std_formatter
	  "@[Utils: no_error_unix_function: Error caught \"%s\" in %s with %s@."
	  (Unix.error_message error)
	  function_name
	  parameter;

	Format.fprintf
	  Format.std_formatter
	  "@[Utils: no_error_unix_function: waiting 0.1 sec before relaunch@.";

	delay_my_thread 0.1;

	no_error_unix_function f
      )
  )

let match_regex pattern str =
  let bool =
    Str.string_match
      (Str.regexp pattern)
      str
      0
  in
  bool
          
let walk_directory_rec dir pattern =
  let rec walk acc = function
    | [] -> (acc)
    | dir::tail ->
       let contents = Array.to_list (Sys.readdir dir) in
       let contents = List.rev_map (Filename.concat dir) contents in
       let dirs, files =
         List.fold_left
	   (fun (dirs,files) f ->
	    let stats =
	      try
		Unix.stat f
	      with
	      | Unix.Unix_error (error, code, command) ->
		 print_endline
		   (sprintf
		      "Utils: walk_directory_tree: unix error with code \"%s\" for command \"%s\":\n%s"
		      code
		      command
		      (Unix.error_message error)
		   );
		 assert(false)
	    in
	    match stats.Unix.st_kind with
	    | Unix.S_REG -> (dirs, f::files)  (* Regular file *)
	    | Unix.S_DIR -> (f::dirs, files)  (* Directory *)
	    | _ -> (dirs, files)
	   ) ([],[]) contents
       in
       let matched = List.filter (match_regex pattern) files in
       walk (matched @ acc) (dirs @ tail)
  in

  try
    (
      if Sys.is_directory dir then
	walk [] [dir]
      else
	(
	  print_endline
	    (sprintf
	       "walk_directory_tree: \"%s\" is not a directory"
	       dir
	    );
	  assert(false)
	)
    )
  with
  | Sys_error message ->
     print_endline
       (sprintf
	  "walk_directory_tree: message \"%s\" for dir: \"%s\" "
	  message
	  dir
       );
     assert(false)

let walk_directory dir pattern =
  let contents = Array.to_list (Sys.readdir dir) in
  let contents = List.rev_map (Filename.concat dir) contents in
  let dirs, files =
    List.fold_left
      (fun (dirs,files) f ->
	 let stats =
	   try
	     Unix.stat f
	   with
	   | Unix.Unix_error (error, code, command) ->
	     print_endline
	       (sprintf
		  "Utils: walk_directory_tree: unix error with code \"%s\" for command \"%s\":\n%s"
		  code
		  command
		  (Unix.error_message error)
	       );
	     assert(false)
	 in
	 match stats.Unix.st_kind with
	 | Unix.S_REG -> (dirs, f::files)  (* Regular file *)
	 | Unix.S_DIR -> (f::dirs, files)  (* Directory *)
	 | _ -> (dirs, files)
      ) ([],[]) contents
  in

  List.filter (match_regex pattern) files
  
