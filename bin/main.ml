type command = Eof | Nop | Timestamp of float

let parse_timestamp t =
  Scanf.sscanf t "%d:%d:%d" (fun hour minute second ->
      Float.of_int ((hour * 60 * 60) + (minute * 60) + second) )

let read_command chan =
  match In_channel.input_line chan with
  | None -> Eof
  | Some line when String.starts_with ~prefix:"out_time=" line ->
      Timestamp
        ( parse_timestamp
        @@ String.(sub line (length "out_time=") (length "00:00:00")) )
  | Some _ -> Nop

let read_commands chan filename total_duration partial_duration seek_to =
  let total =
    Float.to_int
    @@
    match (partial_duration, seek_to) with
    | Some dur, _ -> parse_timestamp dur
    | None, Some seek -> total_duration -. parse_timestamp seek
    | None, None -> total_duration
  in
  let bar =
    let open Progress.Line in
    list
      [ rpad 24 (constf " %s" String.(sub filename 0 (min 23 (length filename))))
      ; percentage_of total
      ; bar ~style:`ASCII total
      ; const "-" ++ eta total ++ const " " ]
  in
  let prev = ref 0.0 in
  let quit = ref false in
  Progress.with_reporter bar (fun f ->
      while not !quit do
        match read_command chan with
        | Eof -> quit := true
        | Nop -> ()
        | Timestamp timestamp ->
            f (Float.to_int (timestamp -. !prev)) ;
            prev := timestamp
      done )

let read_output chan partial_duration seek_to =
  let rec read_duration chan =
    match In_channel.input_line chan with
    | None -> Error "reached EOF before finding a Duration"
    | Some line when String.starts_with ~prefix:"frame=1" line ->
        Error "reached progress updates before finding Duration"
    | Some line when String.starts_with ~prefix:"  Duration:" line ->
        Ok
          ( parse_timestamp
          @@ String.(sub line (length "  Duration: ") (length "00:00:00")) )
    | Some _ -> read_duration chan
  in
  let rec read_filename chan =
    match In_channel.input_line chan with
    | None -> Error "reached EOF before finding Input #0"
    | Some line when String.starts_with ~prefix:"frame=1" line ->
        Error "reached progress updates before finding Input #0"
    | Some line when String.starts_with ~prefix:"Input #0" line ->
        let tmp = String.split_on_char '/' line |> List.rev |> List.hd in
        Ok String.(sub tmp 0 (max 0 (length tmp - 2)))
    | Some _ -> read_filename chan
  in
  match
    Result.bind (read_filename chan) (fun filename ->
        Result.bind (read_duration chan) (fun duration ->
            Ok (read_commands chan filename duration partial_duration seek_to) ) )
  with
  | Error err -> failwith err
  | Ok _ -> ()

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
        read_output stdin None None
  | _ :: args -> (
      let chan =
        Unix.open_process_in
        @@ Filename.quote_command "ffmpeg"
             (["-nostdin"; "-hide_banner"; "-stats"; "-progress"; "-"] @ args)
             ~stdout:"/dev/stdout" ~stderr:"/dev/stdout"
      in
      read_output chan (get_param "-t" args) (get_param "-ss" args) ;
      match Unix.close_process_in chan with
      | Unix.WEXITED 0 -> ()
      | Unix.WEXITED n ->
          Printf.printf "\nabnormal exit code %d\n" n ;
          exit n
      | Unix.WSIGNALED n -> Printf.printf "signaled with %d\n" n
      | Unix.WSTOPPED n -> Printf.printf "stopped with %d\n" n )
