let dur_timestamp () =
  let ic = Stdlib.open_in "Test_parsing.input" in
  Result.get_ok @@ Ffbar.read_output ic (Some "00:00:01") None

let durseek_timestamp () =
  let ic = Stdlib.open_in "Test_parsing.input" in
  Result.get_ok @@ Ffbar.read_output ic (Some "00:00:01") (Some "00:00:01")

let no_timestamp () =
  let ic = Stdlib.open_in "Test_parsing.input" in
  Result.get_ok @@ Ffbar.read_output ic None None

let seconds_timestamp () =
  let ic = Stdlib.open_in "Test_parsing.input" in
  Result.get_ok @@ Ffbar.read_output ic (Some "10") None

let milliseconds_timestamp () =
  let ic = Stdlib.open_in "Test_parsing.input" in
  Result.get_ok @@ Ffbar.read_output ic (Some "00:01:00.12345") None

let () =
  no_timestamp () ;
  dur_timestamp () ;
  durseek_timestamp () ;
  seconds_timestamp () ;
  milliseconds_timestamp ()
