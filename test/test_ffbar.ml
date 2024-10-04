let () =
  let ic = Stdlib.open_in "Test_parsing.input" in
  try Ffbar.read_output ic None None with e -> close_in_noerr ic ; raise e
