type video = {name: string; duration: float}

type command = Eof | Nop | Timestamp of float | ParsingError

let parse_timestamp s =
  Option.to_result
    ~none:("could not parse timestamp: " ^ s)
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

(* NOTE: I do NOT handle if seek is outside file. ffmpeg should handle
   that... *)
let calculate_total video_duration user_duration user_seek =
  Result.map Float.to_int
    ( match (user_duration, user_seek) with
    | Some udur, Some useek ->
        Result.bind (parse_timestamp udur) (fun uduration ->
            Result.bind (parse_timestamp useek) (fun seek ->
                if seek +. uduration > video_duration then
                  Ok (video_duration -. seek)
                else
                  Ok uduration ) )
    | Some dur, None -> parse_timestamp dur
    | None, Some seek ->
        Result.map
          (fun seek -> video_duration -. min video_duration seek)
          (parse_timestamp seek)
    | None, None -> Ok video_duration )

let read_commands chan video user_duration user_seek =
  let total = calculate_total video.duration user_duration user_seek in
  Result.bind total (fun total ->
      let bar =
        let open Progress.Line in
        list
          [ rpad 24
              (constf " %s"
                 String.(sub video.name 0 (min 23 (length video.name))) )
          ; percentage_of total
          ; bar ~style:`ASCII ~data:`Sum total
          ; const "-" ++ eta total ++ const " " ]
      in
      let prev = ref 0 in
      let quit = ref false in
      Progress.with_reporter bar (fun f ->
          while not !quit do
            match read_command chan with
            | Eof | ParsingError ->
                quit := true (* FIXME: handle parsing error *)
            | Nop -> ()
            | Timestamp tt ->
                let timestamp = Float.to_int tt in
                f (timestamp - !prev) ;
                prev := timestamp
          done ) ;
      Ok () )

let read_output chan user_duration user_seek =
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
        Option.to_result ~none:"failed to parse Input #0 line"
          ( String.index_opt line '\''
          |> Option.map (( + ) 1)
          |> Option.map (fun start ->
                 String.(sub line start (length line - start - 2)) )
          |> Option.map Filename.basename )
    | Some _ -> read_filename chan
  in
  Result.bind (read_filename chan) (fun name ->
      Result.bind (read_duration chan) (fun duration ->
          read_commands chan {name; duration} user_duration user_seek ) )
