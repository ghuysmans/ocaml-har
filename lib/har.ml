type creator = {
  name: string;
  version: string;
} [@@deriving yojson]

type dt = string [@@deriving yojson] (* TODO *)

type uri = Uri.t
let uri_of_yojson j =
  try
    Ok (Yojson.Safe.Util.to_string j |> Uri.of_string)
  with _ ->
    Error "uri_of_yojson"
let uri_to_yojson u = `String (Uri.to_string u)

type 'a def = 'a option
let def_of_yojson f j =
  match f j with
  | Error e -> Error e
  | Ok x -> Ok (Some x)
let def_to_yojson f = function
  | None -> failwith "def_to_yojson: please specify [@default None]"
  | Some x -> f x

type 'a tag = 'a
let tag_of_yojson f j =
  match f (`List [j]) with
  | Error e -> Error e
  | Ok x -> Ok x
let tag_to_yojson f x =
  match f x with
  | `List (h :: _) -> h
  | _ -> failwith "tag_to_yojson"

module Page = struct
  type page_timings = {
    on_content_load: float [@default -1.] [@key "onContentLoad"];
    on_load: float [@default -1.] [@key "onLoad"];
  } [@@deriving yojson]

  type t = {
    started_date_time: dt [@key "startedDateTime"];
    id: string;
    title: string;
    page_timings: page_timings [@key "pageTimings"];
  } [@@deriving yojson]
end

module Entry = struct
  module Initiator = struct
    type call_frame = {
      function_name: string [@key "functionName"];
      script_id: string [@key "scriptId"];
      url: uri;
      line_number: int [@key "lineNumber"];
      column_number: int [@key "columnNumber"];
    } [@@deriving yojson]

    type description = string [@@deriving yojson] (* TODO Image, etc.? *)

    type stack = {
      description: description def [@default None];
      call_frames: call_frame list [@key "callFrames"];
      parent: stack def [@default None];
    } [@@deriving yojson]

    type script = {
      typ: string [@key "type"];
      stack: stack;
    } [@@deriving yojson]

    type parser = {
      typ: string [@key "type"];
      url: uri;
      line_number: int def [@default None] [@key "lineNumber"];
    } [@@deriving yojson]

    type t =
      | Other
      | Parser of Uri.t * int option
      | Script of stack

    let of_yojson = function
      | `Assoc l as y ->
        begin match List.assoc "type" l with
        | `String "script" ->
          script_of_yojson y |>
          Result.map (fun {stack; _} -> Script stack)
        | `String "parser" ->
          parser_of_yojson y |>
          Result.map (fun {url; line_number; _} -> Parser (url, line_number))
        | `String "other" -> Ok Other
        | _ -> Error "Entry.Initiator.of_yojson"
        end
      | _ -> Error "Entry.Initiator.of_yojson"

    let to_yojson = function
      | Script stack ->
        script_to_yojson {typ = "stack"; stack}
      | Parser (url, line_number) ->
        parser_to_yojson {typ = "parser"; url; line_number}
      | Other ->
        `Assoc ["type", `String "other"]
  end

  type nv = {
    name: string;
    value: string;
  } [@@deriving yojson]

  type mime_type = string [@@deriving yojson]

  module Request = struct
    type meth =
      | GET
      | POST
      (* FIXME *)
      [@@deriving yojson]

    type same_site =
      | Lax
      | Strict
      | No [@name "None"]
      [@@deriving yojson]

    type cookie = {
      name: string;
      value: string;
      path: string def [@default None];
      domain: string def [@default None];
      expires: dt def [@default None];
      http_only: bool def [@default None] [@key "httpOnly"];
      secure: bool def [@default None];
      same_site: same_site tag def [@default None] [@key "sameSite"];
    } [@@deriving yojson]

    type post_data = {
      mime_type: mime_type [@key "mimeType"];
      text: string;
      params: nv list;
    } [@@deriving yojson]

    type t = {
      meth: meth tag [@key "method"];
      url: uri;
      http_version: string [@key "httpVersion"];
      headers: nv list;
      query_string: nv list [@key "queryString"];
      cookies: cookie list;
      headers_size: int [@key "headersSize"];
      body_size: int [@key "bodySize"];
      post_data: post_data def [@default None] [@key "postData"];
    } [@@deriving yojson]
  end

  module Response = struct
    type encoding =
      | Base64 [@name "base64"]
      [@@deriving yojson]

    type content = {
      size: int;
      compression: int def [@default None];
      mime_type: mime_type [@key "mimeType"];
      text: string def [@default None];
      encoding: encoding tag def [@default None];
    } [@@deriving yojson]

    type error = unit [@@deriving yojson] (* FIXME *)

    type t = {
      status: int;
      status_text: string [@key "statusText"];
      http_version: string [@key "httpVersion"];
      headers: nv list;
      cookies: nv list;
      content: content option;
      redirect_url: uri [@key "redirectURL"];
      headers_size: int [@key "headersSize"];
      body_size: int [@key "bodySize"];
      transfer_size: int def [@default None] [@key "_transferSize"];
      error: error def [@default None] [@key "_error"];
      fulfilled_by: string def [@default None] [@key "_fulfilledBy"];
    } [@@deriving yojson]
  end

  type priority = string [@@deriving yojson] (* TODO High, etc. *)

  type resource_type = string [@@deriving yojson] (* TODO xhr, etc. *)

  type cache_state = {
    expires: dt;
    last_access: dt def [@default None] [@key "lastAccess"];
    etag: string def [@default None] [@key "eTag"];
    hit_count: int def [@default None] [@key "hitCount"];
  } [@@deriving yojson]

  type cache = {
    before_request: cache_state def [@default None] [@key "beforeRequest"];
    after_request: cache_state def [@default None] [@key "afterRequest"];
  } [@@deriving yojson]

  type timings = {
    blocked: float [@default -1.];
    dns: float [@default -1.];
    ssl: float [@default -1.];
    connect: float [@default -1.];
    send: float;
    wait: float;
    receive: float;
    blocked_queueing: float def [@default None] [@key "_blocked_queueing"];
  } [@@deriving yojson]

  type t = {
    from_cache: string def [@default None] [@key "_fromCache"];
    initiator: Initiator.t def [@default None] [@key "_initiator"];
    priority: priority def [@default None] [@key "_priority"];
    resource_type: resource_type def [@default None] [@key "_resourceType"];
    cache: cache option;
    connection: string def [@default None];
    page_ref: string def [@default None] [@key "pageref"];
    request: Request.t;
    response: Response.t;
    server_ip_address: string def [@default None] [@key "serverIPAddress"];
    started_date_time: dt [@key "startedDateTime"];
    time: float;
    timings: timings;
  } [@@deriving yojson]
end

type log = {
  version: string;
  creator: creator;
  pages: Page.t list [@default []];
  entries: Entry.t list;
} [@@deriving yojson]

type t = {
  log: log;
} [@@deriving yojson]
