type command =
  | Eof
  | Nop
  | Timestamp of int

let parse_timestamp t =
  Scanf.sscanf t "%d:%d:%d.%d"
    (fun hour minute second _ ->
      hour * 60 * 60 + minute * 60 + second)

let line2command line =
  match line with
  | line when String.starts_with ~prefix:"out_time=" line ->
     let start = String.length "out_time=" in
     Timestamp (
         parse_timestamp @@
           String.(sub line start ((length line) - start)))
  | _ -> Nop

let read_command () =
    match In_channel.input_line stdin with
    | None        -> Eof;
    | Some (line) -> line2command line

let rec read_duration () =
  match In_channel.input_line stdin with
  | None ->
     Error "reached EOF before finding a Duration";
  | Some (line) when String.starts_with ~prefix:"frame=1" line ->
     Error "reached progess updates before finding Duration"
  | Some (line) when String.starts_with ~prefix:"  Duration:" line ->
     Ok (parse_timestamp @@
           String.(sub line (length "  Duration: ") (length "00:00:00.0")) )
  | Some _ ->
     read_duration ()

let progress_bar duration timestamp =
  Tty.Escape_seq.cursor_horizontal_seq 1 ();
  Tty.Escape_seq.erase_line_seq 1 ();
  Printf.printf "!!!!!!!!!!! %d" (duration - timestamp)

let rec read_commands duration =
  match read_command () with
  | Eof ->  ()
  | Nop -> read_commands duration
  | Timestamp timestamp ->
     progress_bar duration timestamp;
     read_commands duration

let () =
  match read_duration () with
  | Error (err) ->
     failwith err
  | Ok (duration) ->
     read_commands duration
