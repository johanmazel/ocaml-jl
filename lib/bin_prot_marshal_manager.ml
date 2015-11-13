
open Printf

let debug_enabled = ref true

let set_debug bool = debug_enabled := bool

let debug fmt =
  Printf.kprintf
    (
      if !debug_enabled then
        (fun s -> Format.printf "[Bin_prot_marshal_manager]: %s@." s)
      else
        ignore
    )
    fmt

let write_to_file
    bin_writer_t
    data
    file_name
  =
  (
    debug "write_to_file: call";

    debug "write_to_file: dumping in buffer";
    let buffer = 
      Bin_prot.Utils.bin_dump 
        ~header: true
        bin_writer_t
        data
    in

    let data_length = Bigarray.Array1.dim buffer in

    debug "write_to_file: creating string";
    let string = Bytes.create data_length in

    debug "write_to_file: blitting from buffer to string";
    Bin_prot.Common.blit_buf_string
      buffer
      string
      data_length;

    debug "write_to_file: length %d" data_length;

    debug
      "write_to_file: marshalling to %s"
      file_name
    ;

    let file = open_out file_name in

    output_string file string;

    flush file;
    close_out file;

    debug "write_to_file: end";
  )

let import_from_file
    bin_reader_t
    file_path
  =
  (
    debug "import_from_file: call";

    debug "import_from_file: file_path: %s" file_path;

    if Sys.file_exists file_path = false then
      (
        print_endline
          (sprintf
             "%s does not exists"
             file_path
          );
        assert(false)
      );

    let file = open_in file_path in

    let data_length = in_channel_length file in

    let string = Bytes.create data_length in
    really_input file string 0 data_length;
    close_in file;

    debug "import_from_file: data_length %d" data_length;

    (* Note: Bin_prot.Utils.bin_read_stream is used in conjunction with
       "~header: true" in Bin_prot.Utils.bin_dump *)
    let string_pos_ref = ref 0 in
    let data =
      try
        (
          Bin_prot.Utils.bin_read_stream
            ~read:
              (fun buf ~pos:pos ~len:len ->
                 (
                   debug "import_from_file: read: call";
                   debug "import_from_file: read: pos %d - len %d" pos len;

                   Bin_prot.Common.blit_string_buf
                     ~src_pos:!string_pos_ref
                     ~dst_pos:0
                     ~len: len
                     string
                     buf;

                   string_pos_ref := !string_pos_ref + len;

                   debug "import_from_file: read: end";

                   debug "import_from_file: unmarshalling";
                 )
              )
            bin_reader_t
        )
      with
      | Bin_prot.Common.Read_error (error, pos) ->
        failwith (sprintf "Parser: import_from_file: error at %d: %s" pos (Bin_prot.Common.ReadError.to_string error))
    in

    debug "import_from_file: end";

    data
  )
