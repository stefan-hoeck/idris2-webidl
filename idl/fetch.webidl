// Extracted from https://fetch.spec.whatwg.org/#idl-index
//            and https://w3c.github.io/webappsec-referrer-policy/#idl-index

typedef (sequence<sequence<ByteString>> or record<ByteString, ByteString>) HeadersInit;

[Exposed=(Window,Worker)]
interface Headers {
  constructor(optional HeadersInit init);

  undefined append(ByteString name, ByteString value);
  undefined delete(ByteString name);
  ByteString? get(ByteString name);
  boolean has(ByteString name);
  undefined set(ByteString name, ByteString value);
  iterable<ByteString, ByteString>;
};

typedef (Blob or BufferSource or FormData or URLSearchParams or USVString) XMLHttpRequestBodyInit;

typedef (ReadableStream or XMLHttpRequestBodyInit) BodyInit;
interface mixin Body {
  readonly attribute ReadableStream? body;
  readonly attribute boolean bodyUsed;
  [NewObject] Promise<ArrayBuffer> arrayBuffer();
  [NewObject] Promise<Blob> blob();
  [NewObject] Promise<FormData> formData();
  [NewObject] Promise<any> json();
  [NewObject] Promise<USVString> text();
};
typedef (Request or USVString) RequestInfo;

[Exposed=(Window,Worker)]
interface Request {
  constructor(RequestInfo input, optional RequestInit init = {});

  readonly attribute ByteString method;
  readonly attribute USVString url;
  [SameObject] readonly attribute Headers headers;

  readonly attribute RequestDestination destination;
  readonly attribute USVString referrer;
  readonly attribute ReferrerPolicy referrerPolicy;
  readonly attribute RequestMode mode;
  readonly attribute RequestCredentials credentials;
  readonly attribute RequestCache cache;
  readonly attribute RequestRedirect redirect;
  readonly attribute DOMString integrity;
  readonly attribute boolean keepalive;
  readonly attribute boolean isReloadNavigation;
  readonly attribute boolean isHistoryNavigation;
  readonly attribute AbortSignal signal;

  [NewObject] Request clone();
};
Request includes Body;

dictionary RequestInit {
  ByteString method;
  HeadersInit headers;
  BodyInit? body;
  USVString referrer;
  ReferrerPolicy referrerPolicy;
  RequestMode mode;
  RequestCredentials credentials;
  RequestCache cache;
  RequestRedirect redirect;
  DOMString integrity;
  boolean keepalive;
  AbortSignal? signal;
  any window; // can only be set to null
};

enum RequestDestination { "", "audio", "audioworklet", "document", "embed", "font", "frame", "iframe", "image", "manifest", "object", "paintworklet", "report", "script", "sharedworker", "style",  "track", "video", "worker", "xslt" };
enum RequestMode { "navigate", "same-origin", "no-cors", "cors" };
enum RequestCredentials { "omit", "same-origin", "include" };
enum RequestCache { "default", "no-store", "reload", "no-cache", "force-cache", "only-if-cached" };
enum RequestRedirect { "follow", "error", "manual" };

[Exposed=(Window,Worker)]interface Response {
  constructor(optional BodyInit? body = null, optional ResponseInit init = {});

  [NewObject] static Response error();
  [NewObject] static Response redirect(USVString url, optional unsigned short status = 302);

  readonly attribute ResponseType type;

  readonly attribute USVString url;
  readonly attribute boolean redirected;
  readonly attribute unsigned short status;
  readonly attribute boolean ok;
  readonly attribute ByteString statusText;
  [SameObject] readonly attribute Headers headers;

  [NewObject] Response clone();
};
Response includes Body;

dictionary ResponseInit {
  unsigned short status = 200;
  ByteString statusText = "";
  HeadersInit headers;
};

enum ResponseType { "basic", "cors", "default", "error", "opaque", "opaqueredirect" };

partial interface mixin WindowOrWorkerGlobalScope {
  [NewObject] Promise<Response> fetch(RequestInfo input, optional RequestInit init = {});
};

enum ReferrerPolicy {
  "",
  "no-referrer",
  "no-referrer-when-downgrade",
  "same-origin",
  "origin",
  "strict-origin",
  "origin-when-cross-origin",
  "strict-origin-when-cross-origin",
  "unsafe-url"
};
