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

module Page = struct
  type page_timings = {
    on_content_load: float [@key "onContentLoad"];
    on_load: float [@key "onLoad"];
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

    type parent = {
      description: description;
      call_frames: call_frame list [@key "callFrames"];
    } [@@deriving yojson]

    type stack = {
      call_frames: call_frame list [@key "callFrames"];
      parent: parent;
    } [@@deriving yojson]

    type typ = string [@@deriving yojson] (* TODO script, etc. *)

    type t = {
      typ: typ [@key "type"];
      stack: stack;
    } [@@deriving yojson]
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

    type cookie = {
      name: string;
      value: string;
      path: string;
      domain: string;
      expires: dt;
      http_only: bool [@key "httpOnly"];
      secure: bool;
      same_site: bool [@key "sameSite"];
    } [@@deriving yojson]

    type post_data = {
      mime_type: mime_type [@key "mimeType"];
      text: string;
      params: nv list;
    } [@@deriving yojson]

    type t = {
      meth: meth [@key "method"];
      url: uri;
      http_version: string [@key "httpVersion"];
      headers: nv list;
      query_string: nv list [@key "queryString"];
      cookies: cookie list;
      headers_size: int [@key "headersSize"];
      body_size: int [@key "bodySize"];
      post_data: post_data [@key "postData"];
    } [@@deriving yojson]
  end

  module Response = struct
    type encoding =
      | Base64 [@name "base64"]
      | Raw (* FIXME? *)
      [@@deriving yojson]

    type content = {
      size: int;
      mime_type: mime_type [@key "mimeType"];
      text: string;
      encoding: encoding [@default Raw];
    } [@@deriving yojson]

    type error = unit [@@deriving yojson] (* FIXME *)

    type t = {
      status: int;
      status_text: string [@key "statusText"];
      http_version: string [@key "httpVersion"];
      headers: nv list;
      cookies: nv list;
      content: content;
      redirect_url: uri [@default Uri.empty] [@key "redirectURL"];
      headers_size: int [@key "headersSize"];
      body_size: int [@key "bodySize"];
      transfer_size: int [@key "_transferSize"];
      error: error [@key "_error"];
    } [@@deriving yojson]
  end

  type priority = string [@@deriving yojson] (* TODO High, etc. *)

  type resource_type = string [@@deriving yojson] (* TODO xhr, etc. *)

  type cache_state = {
    expires: dt;
    last_access: dt [@key "lastAccess"];
    etag: string [@key "eTag"];
    hit_count: int [@key "hitCount"];
  } [@@deriving yojson]

  type cache = {
    before_request: cache_state option [@key "beforeRequest"];
    after_request: cache_state option [@key "afterRequest"];
  } [@@deriving yojson]

  type timings = {
    blocked: float;
    dns: float;
    ssl: float;
    connect: float;
    send: float;
    wait: float;
    receive: float;
    blocked_queueing: float [@key "_blocked_queueing"];
  } [@@deriving yojson]

  type t = {
    from_cache: string option [@key "_fromCache"];
    initiator: Initiator.t [@key "_initiator"];
    priority: priority [@key "_priority"];
    resource_type: resource_type [@key "_resourceType"];
    cache: cache option;
    connection: string option;
    page_ref: string [@key "pageref"];
    request: Request.t;
    response: Response.t;
    server_ip_address: string [@key "serverIPAddress"];
    started_date_time: dt [@key "startedDateTime"];
    time: float;
    timings: timings;
  } [@@deriving yojson]
end

type log = {
  version: string;
  creator: creator;
  pages: Page.t list;
  entries: Entry.t list;
} [@@deriving yojson]

type t = {
  log: log;
} [@@deriving yojson]
