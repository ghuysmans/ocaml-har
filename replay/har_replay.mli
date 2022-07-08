type response = Cohttp.Response.t * Cohttp_lwt.Body.t

module type S = sig
  include Map.OrderedType
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val of_har : Har.Entry.Request.t -> Har.Entry.Response.t -> (t * response) list
  val of_cohttp : ?body:string -> Uri.t -> Cohttp.Request.t -> t
end

val response_of_har : Har.Entry.Response.t -> Cohttp.Response.t
val body_of_har : Har.Entry.Response.t -> Cohttp_lwt.Body.t

module Make (Indexer : S) : sig
  include Cohttp_lwt.S.Client
  val sexp_of_ctx : ctx -> Sexplib0.Sexp.t
  val index : Har.t -> ctx
end
