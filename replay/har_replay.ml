module type S = sig
  include Map.OrderedType
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val of_har : Har.Entry.Request.t -> t option
  val of_cohttp : ?body:Cohttp_lwt.Body.t -> Uri.t -> Cohttp_lwt.Request.t -> t
end

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
      match Indexer.of_har request with
      | None -> acc
      | Some t ->
        let status = Cohttp.Code.status_of_code response.status in
        let headers =
          List.map (fun (x : Har.Entry.nv) -> x.name, x.value) response.headers |>
          Cohttp.Header.of_list
        in
        let resp = Cohttp_lwt.Response.make ~status ~headers () in
        let body =
          match response.content with
          | Some {text = Some x; _} -> Cohttp_lwt.Body.of_string x
          | _ -> Cohttp_lwt.Body.empty
        in
        M.add t (resp, body) acc
    ) M.empty entries

  let call ?(ctx : ctx = default_ctx) ?headers ?body ?chunked meth uri =
    ignore chunked;
    let req = Cohttp.Request.make ~meth ?headers uri in
    let t = Indexer.of_cohttp ?body uri req in
    Lwt.return
      begin match M.find_opt t !ctx with
      | Some x -> x
      | None ->
        Cohttp.Response.make ~status:`Not_found (),
        Cohttp_lwt.Body.of_string @@
          Sexplib0.Sexp.to_string_hum (Indexer.sexp_of_t t)
      end

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
