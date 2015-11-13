
open Printf

let execute
    prefix
    f
  =
  print_endline prefix;

  let zone = Core.Time.Zone.local in

  let time_now = Unix_utils.no_error_unix_function (Unix.gettimeofday ()) in
  let tm = Unix.localtime time_now in
  let start_core_date = Core.Date.of_tm tm in
  let start_ofday = Core.Std.Time.Ofday.now zone in
  let start_time = Core.Time.of_date_ofday 
      ~zone: zone
      start_core_date
      start_ofday
  in

  let result = f () in

  let time_now = Unix_utils.no_error_unix_function (Unix.gettimeofday ()) in
  let tm = Unix.localtime time_now in
  let end_core_date = Core.Date.of_tm tm in
  let end_ofday = Core.Std.Time.Ofday.now zone in
  let end_time = Core.Time.of_date_ofday
       ~zone: zone
      end_core_date
      end_ofday
  in

  let time_span = Core.Time.diff end_time start_time in

  let parts = Core.Span.to_parts time_span in

  print_endline
    (sprintf
       "%s %dh:%dm:%ds"
       prefix
       parts.Core.Span.Parts.hr
       parts.Core.Span.Parts.min
       parts.Core.Span.Parts.sec
    );

  result
