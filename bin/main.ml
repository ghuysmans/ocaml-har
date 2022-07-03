module C = Har_replay.Make (struct
  type t = Uri.t
  let compare = compare
  let sexp_of_t x = Sexplib0.Sexp.Atom (Uri.to_string x)

  let of_har = function
    | {Har.Entry.Request.meth = GET; url; _} -> Some url
    | _ -> None

  let of_cohttp ?body uri = function
    | {Cohttp.Request.meth = `GET; _} -> ignore body; uri
    | _ -> failwith "not a GET"
end)

let () = Lwt_main.run (
  match Yojson.Safe.from_channel stdin |> Har.of_yojson with
  | Error e ->
    print_endline e;
    exit 1
  | Ok har ->
    let ctx = C.index har in
    let%lwt resp, body = C.get ~ctx (Uri.of_string "https://example.com/") in
    let%lwt s = Cohttp_lwt.Body.to_string body in
    match Cohttp.Response.status resp with
    | `OK -> Lwt_io.printl s
    | _ -> Lwt_io.eprintf "not ok: %s\n" s
)
