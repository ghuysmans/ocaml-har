let () =
  match Yojson.Safe.from_channel stdin |> Har.of_yojson with
  | Error e ->
    prerr_endline e;
    exit 1
  | Ok {log} ->
    print_endline log.creator.name
