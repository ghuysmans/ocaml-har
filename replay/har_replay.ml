type response = Cohttp.Response.t * Cohttp_lwt.Body.t

module type S = sig
  include Map.OrderedType
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val of_har : Har.Entry.Request.t -> Har.Entry.Response.t -> (t * response) list
  val of_cohttp : ?body:string -> Uri.t -> Cohttp.Request.t -> t
end

let response_of_har (response : Har.Entry.Response.t) =
  let status = Cohttp.Code.status_of_code response.status in
  let headers =
    List.map (fun (x : Har.Entry.nv) -> x.name, x.value) response.headers |>
    Cohttp.Header.of_list
  in
  Cohttp_lwt.Response.make ~status ~headers ()

let body_of_har (response : Har.Entry.Response.t) =
  match response.content with
  | Some {text = Some x; _} -> Cohttp_lwt.Body.of_string x
  | _ -> Cohttp_lwt.Body.empty

module Make (Indexer : S) = struct
  module M = Map.Make (Indexer)

  type ctx = (Cohttp_lwt.Response.t * Cohttp_lwt.Body.t) M.t ref
  let default_ctx = ref M.empty
  let sexp_of_ctx ctx =
    M.to_seq !ctx |>
    List.of_seq |>
    Sexplib0.Sexp_conv.(sexp_of_list
      Cohttp_lwt.(sexp_of_pair
        Indexer.sexp_of_t
        (sexp_of_pair Response.sexp_of_t Body.sexp_of_t)))

  let index {Har.log = {entries; _}} =
    ref @@ List.fold_left (fun acc {Har.Entry.request; response; _} ->
      Indexer.of_har request response |>
      List.fold_left (fun acc (t, r) -> M.add t r acc) acc
    ) M.empty entries

  let call ?(ctx : ctx = default_ctx) ?headers ?body ?chunked meth uri =
    ignore chunked;
    let req = Cohttp.Request.make ~meth ?headers uri in
    let f body =
      let t = Indexer.of_cohttp ?body uri req in
      match M.find_opt t !ctx with
      | Some x -> x
      | None ->
        Cohttp.Response.make ~status:`Not_found (),
        Cohttp_lwt.Body.of_string @@
          Sexplib0.Sexp.to_string_hum (Indexer.sexp_of_t t)
    in
    match body with
    | None -> Lwt.return (f None)
    | Some b ->
      let open Lwt.Infix in
      Cohttp_lwt.Body.to_string b >|= fun s ->
      f (Some s)

  let callv ?ctx _uri _stream =
    ignore ctx;
    failwith "TODO"

  let get ?ctx ?headers uri =
    call ?ctx ?headers `GET uri

  let post ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?body ?chunked ?headers `POST uri

  (* taken from Cohttp_lwt.Client.Make *)
  let post_form ?ctx ?headers ~params uri =
    let headers =
      Cohttp.Header.add_opt_unless_exists headers "content-type"
        "application/x-www-form-urlencoded"
    in
    let body = Cohttp_lwt.Body.of_string (Uri.encoded_of_query params) in
    post ?ctx ~chunked:false ~headers ~body uri

  let patch ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?body ?chunked ?headers `PATCH uri

  let put ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?body ?chunked ?headers `PUT uri

  let delete ?ctx ?body ?chunked ?headers uri =
    call ?ctx ?body ?chunked ?headers `DELETE uri

  let head ?ctx ?headers uri =
    let open Lwt.Infix in
    call ?ctx ?headers `HEAD uri >|= fst
end
