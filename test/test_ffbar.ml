let dur_timestamp () =
  let ic = Stdlib.open_in "Test_parsing.input" in
  try Ffbar.read_output ic (Some "00:00:01") None
  with e -> close_in_noerr ic ; raise e

let durseek_timestamp () =
  let ic = Stdlib.open_in "Test_parsing.input" in
  try Ffbar.read_output ic (Some "00:00:01") (Some "00:00:01")
  with e -> close_in_noerr ic ; raise e

let no_timestamp () =
  let ic = Stdlib.open_in "Test_parsing.input" in
  try Ffbar.read_output ic None None with e -> close_in_noerr ic ; raise e

let seconds_timestamp () =
  let ic = Stdlib.open_in "Test_parsing.input" in
  try Ffbar.read_output ic (Some "10") None
  with e -> close_in_noerr ic ; raise e

let milliseconds_timestamp () =
  let ic = Stdlib.open_in "Test_parsing.input" in
  try Ffbar.read_output ic (Some "00:01:00.12345") None
  with e -> close_in_noerr ic ; raise e

let () =
  no_timestamp () ;
  dur_timestamp () ;
  durseek_timestamp () ;
  seconds_timestamp () ;
  milliseconds_timestamp ()
