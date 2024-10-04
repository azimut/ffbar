type video = {name: string; duration: float}

type command = Eof | Nop | Timestamp of float | ParsingError

let parse_timestamp s =
  Option.to_result ~none:"failed to parse_timestamp"
    ( if String.contains s ':' then
        Scanf.sscanf_opt s "%d:%d:%f" (fun hour minute second ->
            Float.of_int ((hour * 60 * 60) + (minute * 60)) +. second )
      else
        float_of_string_opt s )

let read_command chan =
  match In_channel.input_line chan with
  | None -> Eof
  | Some line when String.starts_with ~prefix:"out_time=" line -> (
      let raw_timestamp =
        String.(sub line (length "out_time=") (length "00:00:00.000000"))
      in
      match parse_timestamp raw_timestamp with
      | Error _ -> ParsingError
      | Ok timestamp -> Timestamp timestamp )
  | Some _ -> Nop

let calculate_total duration output_duration seek_to =
  Result.map Float.to_int
    ( match (output_duration, seek_to) with
    | Some dur, _ -> parse_timestamp dur
    | None, Some seek ->
        Result.map (fun seek -> duration -. seek) (parse_timestamp seek)
    | None, None -> Ok duration )

let read_commands chan video partial_duration seek_to =
  let total = calculate_total video.duration partial_duration seek_to in
  Result.bind total (fun total ->
      let bar =
        let open Progress.Line in
        list
          [ rpad 24
              (constf " %s"
                 String.(sub video.name 0 (min 23 (length video.name))) )
          ; percentage_of total
          ; bar ~style:`ASCII total
          ; const "-" ++ eta total ++ const " " ]
      in
      let prev = ref 0.0 in
      let quit = ref false in
      Progress.with_reporter bar (fun f ->
          while not !quit do
            match read_command chan with
            | Eof | ParsingError ->
                quit := true (* FIXME: handle parsing error *)
            | Nop -> ()
            | Timestamp timestamp ->
                f (Float.to_int (timestamp -. !prev)) ;
                prev := timestamp
          done ) ;
      Ok () )

let read_output chan partial_duration seek_to =
  let rec read_duration chan =
    match In_channel.input_line chan with
    | None -> Error "reached EOF before finding a Duration"
    | Some line when String.starts_with ~prefix:"frame=" line ->
        Error "reached progress updates before finding Duration"
    | Some line when String.starts_with ~prefix:"  Duration:" line ->
        let raw_timestamp =
          String.(sub line (length "  Duration: ") (length "00:00:00.00"))
        in
        parse_timestamp raw_timestamp
    | Some _ -> read_duration chan
  in
  let rec read_filename chan =
    match In_channel.input_line chan with
    | None -> Error "reached EOF before finding Input #0"
    | Some line when String.starts_with ~prefix:"frame=" line ->
        Error "reached progress updates before finding Input #0"
    | Some line when String.starts_with ~prefix:"Input #0" line ->
        let start = String.index line '\'' + 1 in
        let file = String.(sub line start (length line - start - 2)) in
        Ok (Filename.basename file)
    | Some _ -> read_filename chan
  in
  match
    Result.bind (read_filename chan) (fun name ->
        Result.bind (read_duration chan) (fun duration ->
            read_commands chan {name; duration} partial_duration seek_to ) )
  with
  | Error err -> failwith err
  | Ok _ -> ()
