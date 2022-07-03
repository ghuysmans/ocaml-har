module type S = sig
  include Map.OrderedType
  val sexp_of_t : t -> Sexplib0.Sexp.t
  val of_har : Har.Entry.Request.t -> t option
  val of_cohttp : ?body:Cohttp_lwt.Body.t -> Uri.t -> Cohttp_lwt.Request.t -> t
end

module Make (Indexer : S) : sig
  include Cohttp_lwt.S.Client
  val sexp_of_ctx : ctx -> Sexplib0.Sexp.t
  val index : Har.t -> ctx
end
