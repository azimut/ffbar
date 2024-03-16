type command = Eof | Nop | Timestamp of float

let parse_timestamp t =
  Scanf.sscanf t "%d:%d:%d.%d" (fun hour minute second _ ->
      Float.of_int ((hour * 60 * 60) + (minute * 60) + second) )

let rec read_duration chan =
  match In_channel.input_line chan with
  | None -> Error "reached EOF before finding a Duration"
  | Some line when String.starts_with ~prefix:"frame=1" line ->
      Error "reached progress updates before finding Duration"
  | Some line when String.starts_with ~prefix:"  Duration:" line ->
      Ok
        ( parse_timestamp
        @@ String.(sub line (length "  Duration: ") (length "00:00:00.0")) )
  | Some _ -> read_duration chan

let line2command line =
  match line with
  | line when String.starts_with ~prefix:"out_time=" line ->
      let start = String.length "out_time=" in
      Timestamp
        (parse_timestamp @@ String.(sub line start (length line - start)))
  | _ -> Nop

let bar perc start_time =
  let duplicate n s = String.concat "" @@ List.init n (fun _ -> s) in
  let tt = Unix.gmtime (Unix.time () -. start_time) in
  Printf.sprintf "%3d|%s%s|%02d:%02d:%02d" perc
    (duplicate perc "\u{2588}")
    (duplicate (100 - perc) " ")
    tt.tm_hour tt.tm_min tt.tm_sec

let progress_bar percentage start_time =
  Tty.Escape_seq.cursor_horizontal_seq 0 () ;
  Tty.Escape_seq.erase_line_seq 200 () ;
  print_string @@ bar percentage start_time

let read_command chan =
  match In_channel.input_line chan with
  | None -> Eof
  | Some line -> line2command line

let rec read_commands chan duration start_time =
  match read_command chan with
  | Eof -> print_newline ()
  | Nop -> read_commands chan duration start_time
  | Timestamp timestamp ->
      let percentage = Int.of_float (100.0 *. timestamp /. duration) in
      progress_bar percentage start_time ;
      read_commands chan duration start_time

let read_output chan =
  match read_duration chan with
  | Error err -> failwith err
  | Ok duration ->
      let start_time = Unix.time () in
      read_commands chan duration start_time

let () =
  match Sys.argv with
  | [||] | [|_|] -> read_output stdin
  | args ->
      args.(0) <-
        "2>&1 /usr/bin/ffmpeg -nostdin -hide_banner -stats -progress -" ;
      read_output @@ Unix.open_process_in
      @@ String.concat " " (Array.to_list args)
