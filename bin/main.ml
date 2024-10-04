let () =
  let usage =
    String.concat "\n"
      [ "Show a progress bar to your run of ffmpeg."
      ; ""
      ; "Examples:"
      ; "$ ffbar -i input.mp4 out.mp4"
      ; "$ ffmpeg -nostdin -stats -progress - -i input.mp4 out.mp4 2>&1 | ffbar"
      ]
  in
  let get_param flag args =
    Option.bind
      (Option.map (( + ) 1) (List.find_index (( = ) flag) args))
      (List.nth_opt args)
  in
  match Sys.argv |> Array.to_list with
  | [] | [_] ->
      if Unix.(isatty stdin) then
        print_endline usage
      else
        Ffbar.read_output stdin None None
  | _ :: args -> (
      let chan =
        Unix.open_process_in
        @@ Filename.quote_command "ffmpeg"
             (["-nostdin"; "-hide_banner"; "-stats"; "-progress"; "-"] @ args)
             ~stdout:"/dev/stdout" ~stderr:"/dev/stdout"
      in
      Ffbar.read_output chan (get_param "-t" args) (get_param "-ss" args) ;
      match Unix.close_process_in chan with
      | Unix.WEXITED 0 -> ()
      | Unix.WEXITED n ->
          Printf.printf "\nabnormal exit code %d\n" n ;
          exit n
      | Unix.WSIGNALED n -> Printf.printf "signaled with %d\n" n
      | Unix.WSTOPPED n -> Printf.printf "stopped with %d\n" n )
