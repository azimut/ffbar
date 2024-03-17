type command = Eof | Nop | Timestamp of float

let parse_timestamp t =
  Scanf.sscanf t "%d:%d:%d" (fun hour minute second ->
      Float.of_int ((hour * 60 * 60) + (minute * 60) + second) )

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

let line2command line =
  match line with
  | line when String.starts_with ~prefix:"out_time=" line ->
      Timestamp
        ( parse_timestamp
        @@ String.(sub line (length "out_time=") (length "00:00:00")) )
  | _ -> Nop

let read_command chan =
  match In_channel.input_line chan with
  | None -> Eof
  | Some line -> line2command line

let read_commands chan duration =
  let total = Float.to_int duration in
  let bar =
    let open Progress.Line in
    list [elapsed (); bar ~style:`UTF8 total; percentage_of total]
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
      done ) ;
  print_newline ()

let read_output chan =
  match read_duration chan with
  | Error err -> failwith err
  | Ok duration -> read_commands chan duration

let print_usage () =
  print_endline "Show a progress bar to your run of ffmpeg." ;
  print_newline () ;
  print_endline "Examples:" ;
  print_endline "$ ffbar -i input.mp4 out.mp4" ;
  print_endline
    "$ ffmpeg -nostdin -stats -progress - -i input.mp4 out.mp4 2>&1 | ffbar"

let () =
  match Sys.argv with
  | [||] | [|_|] ->
      if Unix.(isatty stdin) then
        print_usage ()
      else
        read_output stdin
  | args ->
      args.(0) <-
        "2>&1 /usr/bin/ffmpeg -nostdin -hide_banner -stats -progress -" ;
      read_output @@ Unix.open_process_in
      @@ String.concat " " (Array.to_list args)
