type creator = {
  name: string;
  version: string;
}

type dt = string (* TODO *)

module Page = struct
  type page_timings = {
    on_content_load: float [@key "onContentLoad"];
    on_load: float [@key "onLoad"];
  }

  type t = {
    started_date_time: dt [@key "startedDateTime"];
    id: string;
    title: string;
    page_timings: page_timings [@key "pageTimings"];
  }
end

module Entry = struct
  module Initiator = struct
    type call_frame = {
      function_name: string [@key "functionName"];
      script_id: string [@key "scriptId"];
      url: Uri.t;
      line_number: int [@key "lineNumber"];
      column_number: int [@key "columnNumber"];
    }

    type description = string (* TODO Image, etc.? *)

    type parent = {
      description: description;
      call_frames: call_frame list [@key "callFrames"];
    }

    type stack = {
      call_frames: call_frame list [@key "callFrames"];
      parent: parent;
    }

    type typ = string (* TODO script, etc. *)

    type t = {
      typ: typ [@key "type"];
      stack: stack;
    }
  end

  type nv = {
    name: string;
    value: string;
  }

  type mime_type = string

  module Request = struct
    type meth =
      | GET
      | POST
      (* FIXME *)

    type cookie = {
      name: string;
      value: string;
      path: string;
      domain: string;
      expires: dt;
      http_only: bool [@key "httpOnly"];
      secure: bool;
      same_site: bool [@key "sameSite"];
    }

    type post_data = {
      mime_type: mime_type [@key "mimeType"];
      text: string;
      params: nv list;
    }

    type t = {
      meth: meth [@key "method"];
      url: Uri.t;
      http_version: string [@key "httpVersion"];
      headers: nv list;
      query_string: nv list [@key "queryString"];
      cookies: cookie list;
      headers_size: int [@key "headersSize"];
      body_size: int [@key "bodySize"];
      post_data: post_data [@key "postData"];
    }
  end

  module Response = struct
    type encoding = Base64 (* TODO "base64" *)

    type content = {
      size: int;
      mime_type: mime_type [@key "mimeType"];
      text: string;
      encoding: encoding option;
    }

    type error = unit (* FIXME *)

    type t = {
      status: int;
      status_text: string [@key "statusText"];
      http_version: string [@key "httpVersion"];
      headers: nv list;
      cookies: nv list;
      content: content;
      redirect_url: Uri.t option [@key "redirectURL"];
      headers_size: int [@key "headersSize"];
      body_size: int [@key "bodySize"];
      transfer_size: int [@key "_transferSize"];
      error: error option [@key "_error"];
    }
  end

  type priority = string (* TODO High, etc. *)

  type resource_type = string (* TODO xhr, etc. *)

  type cache = unit (* TODO empty object *)

  type timings = {
    blocked: float;
    dns: float;
    ssl: float;
    connect: float;
    send: float;
    wait: float;
    receive: float;
    blocked_queueing: float [@key "_blocked_queueing"];
  }

  type t = {
    from_cache: string option [@key "_fromCache"];
    initiator: Initiator.t [@key "_initiator"];
    priority: priority [@key "_priority"];
    resource_type: resource_type [@key "_resourceType"];
    cache: cache;
    connection: string option;
    page_ref: string [@key "pageref"];
    request: Request.t;
    response: Response.t;
    server_ip_address: string [@key "serverIPAddress"];
    started_date_time: dt [@key "startedDateTime"];
    time: float;
    timings: timings;
  }
end

type log = {
  version: string;
  creator: creator;
  pages: Page.t list;
  entries: Entry.t list;
}

type t = {
  log: log;
}
