/*!
A fast [`RegexSet`](https://doc.rust-lang.org/regex/regex/struct.RegexSet.html) based router for use with async Hyper (0.13).

Individual handler functions should have the type `H`, where
```rust
    H: Fn(Request) -> F,
    F: Future<Output = Result<S, E>> + Send,
    S: Into<Response>,
    E: Into<Response>,
```

You can return something as simple as `Ok(Response::new("hello world".into()))`. You don't have to worry about futures
unless you need to read the request body or interact with other future-aware things.

## Usage:

```rust
use reset_router::{Request, RequestExtensions, Response, Router, SharedService};
use std::sync::Arc;

pub struct Handler(Arc<String>);

impl SharedService for Handler {
    type Response = Response;
    type Error = Box<dyn std::error::Error + Send + Sync + 'static>;
    type Future = futures::future::Ready<Result<Self::Response, Self::Error>>;

    fn call(&self, _: Request) -> Self::Future {
        futures::future::ready(Ok(http::Response::builder()
            .status(200)
            .body(format!("Hello, {}!", &self.0).into())
            .unwrap()))
    }
}

#[derive(Clone)]
pub struct State(pub i32);

async fn hello(req: Request) -> Result<Response, Response> {
    let (first_name, last_name) = req.parsed_captures::<(String, String)>()?;
    Ok(http::Response::builder()
        .status(200)
        .body(format!("Hello, {} {}!", first_name, last_name).into())
        .unwrap())
}

async fn add(req: Request) -> Result<Response, Response> {
    let (add1, add2) = req.parsed_captures::<(i32, i32)>()?;

    let state_num: i32 = req.data::<State>().map(|x| x.0).unwrap_or(0);

    Ok(http::Response::builder()
        .status(200)
        .body(
            format!("{} + {} + {} = {}\r\n", add1, add2, state_num, add1 + add2 + state_num).into(),
        )
        .unwrap())
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let router = Router::build()
        .data(State(42))
        .add(http::Method::POST, r"^/hello/([^/]+)/(.+)$", hello)
        .add(http::Method::GET, r"^/hello/([^/]+)/(.+)$", hello)
        .add(http::Method::GET, r"^/add/([\d]+)/([\d]+)$", add)
        .add(http::Method::GET, r"^/other$", Handler(Arc::new(String::from("world"))))
        .add_not_found(|_| {
            async {
                Ok::<_, Response>(http::Response::builder().status(404).body("404".into()).unwrap())
            }
        })
        .finish()?;

    let addr = "0.0.0.0:3000".parse()?;

    let server = hyper::Server::bind(&addr).serve(router);

    server.await?;

    Ok(())
}
```
*/

/// Error handling
pub mod err {
    /// The error enum
    #[derive(Debug)]
    pub enum Error {
        CapturesMissing,
        MethodNotSupported,
        Http(http::Error),
        Recognizer(reset_recognizer::err::Error),
    }

    impl std::fmt::Display for Error {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            use Error::*;
            match self {
                CapturesMissing => "Captures missing".fmt(f),
                MethodNotSupported => "Method not supported".fmt(f),
                Http(ref inner) => inner.fmt(f),
                Recognizer(ref inner) => inner.fmt(f),
            }
        }
    }

    impl std::error::Error for Error {
        fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
            use Error::*;
            match self {
                Http(ref inner) => Some(inner),
                Recognizer(ref inner) => Some(inner),
                _ => None,
            }
        }
    }

    /// Result wrapper: `Result<T, Error>`
    pub type Result<T> = std::result::Result<T, Error>;

    impl From<Error> for http::Response<hyper::Body> {
        fn from(t: Error) -> Self {
            http::Response::builder().status(500).body(t.to_string().into()).unwrap()
        }
    }
}

use reset_recognizer as recognizer;

use std::collections::HashMap;

use std::pin::Pin;
use std::sync::Arc;

use futures::{
    future::{ready, Ready},
    ready, Future, FutureExt, TryFuture,
};
use std::task::{Context, Poll};

/// Convenience wrapper for `http::Request<hyper::Body>`
pub type Request = http::Request<hyper::Body>;

/// Convenience wrapper for `http::Response<hyper::Body>`
pub type Response = http::Response<hyper::Body>;

struct MethodMap<T> {
    options: Option<T>,
    get: Option<T>,
    post: Option<T>,
    put: Option<T>,
    delete: Option<T>,
    head: Option<T>,
    trace: Option<T>,
    connect: Option<T>,
    patch: Option<T>,
    extension: Option<HashMap<http::Method, T>>,
}

impl<T> MethodMap<T> {
    fn new() -> Self {
        Self {
            options: None,
            get: None,
            post: None,
            put: None,
            delete: None,
            head: None,
            trace: None,
            connect: None,
            patch: None,
            extension: None,
        }
    }

    fn get(&self, method: &http::Method) -> Option<&T> {
        match *method {
            http::Method::OPTIONS => self.options.as_ref(),
            http::Method::GET => self.get.as_ref(),
            http::Method::POST => self.post.as_ref(),
            http::Method::PUT => self.put.as_ref(),
            http::Method::DELETE => self.delete.as_ref(),
            http::Method::HEAD => self.head.as_ref(),
            http::Method::TRACE => self.trace.as_ref(),
            http::Method::CONNECT => self.connect.as_ref(),
            http::Method::PATCH => self.patch.as_ref(),
            ref m => self.extension.as_ref().and_then(|e| e.get(m)),
        }
    }

    fn insert(&mut self, method: http::Method, t: T) {
        match method {
            http::Method::OPTIONS => {
                self.options = Some(t);
            }
            http::Method::GET => {
                self.get = Some(t);
            }
            http::Method::POST => {
                self.post = Some(t);
            }
            http::Method::PUT => {
                self.put = Some(t);
            }
            http::Method::DELETE => {
                self.delete = Some(t);
            }
            http::Method::HEAD => {
                self.head = Some(t);
            }
            http::Method::TRACE => {
                self.trace = Some(t);
            }
            http::Method::CONNECT => {
                self.connect = Some(t);
            }
            http::Method::PATCH => {
                self.patch = Some(t);
            }
            m => {
                let mut extension = self.extension.take().unwrap_or_else(HashMap::new);
                extension.insert(m, t);
                self.extension = Some(extension);
            }
        }
    }
}

/// Container for application `data`, available in request handler `fn`s
pub struct Data<T>(Arc<T>);

impl<T> Data<T> {
    /// Create a new `data` container
    pub fn new(t: T) -> Self {
        Data(Arc::new(t))
    }

    /// Create a new `data` from an existing `Arc<T>`
    pub fn from_arc(arc: Arc<T>) -> Self {
        Data(arc)
    }
}

impl<T> std::ops::Deref for Data<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl<T> Clone for Data<T> {
    fn clone(&self) -> Self {
        Data(Arc::clone(&self.0))
    }
}

/// Shared trait for route handlers. Similar to `tower::Service`, but takes `&self`.
///
/// Implemented for `H` where
/// ```
/// H: Fn(Request) -> F,
/// F: Future<Output = Result<S, E>> + Send,
/// S: Into<Response>,
/// E: Into<Response>,
/// ```
pub trait SharedService {
    type Response: Into<Response>;
    type Error: Into<Box<dyn std::error::Error + Send + Sync + 'static>>;
    type Future: Future<Output = Result<Self::Response, Self::Error>> + Send;

    fn call(&self, request: Request) -> Self::Future;
}

#[doc(hidden)]
#[pin_project::pin_project]
pub struct HandlerFuture<F> {
    #[pin]
    inner: F,
}

impl<F, S, E> Future for HandlerFuture<F>
where
    F: Future<Output = Result<S, E>> + Send,
    S: Into<Response>,
    E: Into<Response>,
{
    type Output = Result<Response, Box<dyn std::error::Error + Send + Sync + 'static>>;

    #[inline]
    fn poll(self: Pin<&mut Self>, cx: &mut Context) -> Poll<Self::Output> {
        let out = match ready!(self.project().inner.try_poll(cx)) {
            Ok(res) => Ok(res.into()),
            Err(err) => Ok(err.into()),
        };
        Poll::Ready(out)
    }
}

impl<H, F, S, E> SharedService for H
where
    F: Future<Output = Result<S, E>> + Send,
    S: Into<Response>,
    E: Into<Response>,
    H: Fn(Request) -> F,
{
    type Response = Response;
    type Error = Box<dyn std::error::Error + Send + Sync + 'static>;
    type Future = HandlerFuture<F>;

    fn call(&self, request: Request) -> Self::Future {
        HandlerFuture { inner: self(request) }
    }
}

struct BoxedSharedService(
    Arc<
        dyn Fn(
                Request,
            ) -> Pin<
                Box<
                    dyn Future<
                            Output = Result<
                                Response,
                                Box<dyn std::error::Error + Send + Sync + 'static>,
                            >,
                        > + Send,
                >,
            > + Send
            + Sync,
    >,
);

impl BoxedSharedService {
    fn new<T: SharedService + Send + Sync + 'static>(t: T) -> Self {
        Self(Arc::new(move |req: Request| {
            let rt = t.call(req).map(|res| res.map(|s| s.into()).map_err(|e| e.into()));
            Box::pin(rt)
        }))
    }
}

#[derive(Clone)]
struct DataMap(Data<type_map::concurrent::TypeMap>);

struct InnerRouter {
    data: Option<DataMap>,
    not_found: BoxedSharedService,
    routers: MethodMap<recognizer::Router<BoxedSharedService>>,
}

/// The router, impls `hyper::service::Service` and the equivalent of `MakeService`
#[derive(Clone)]
pub struct Router(Arc<InnerRouter>);

impl Router {
    /// Create a new `RouterBuilder`
    pub fn build() -> RouterBuilder {
        RouterBuilder::new()
    }
}

/// Builder for a `Router`
pub struct RouterBuilder {
    data: Option<type_map::concurrent::TypeMap>,
    not_found: Option<BoxedSharedService>,
    inner: HashMap<http::Method, recognizer::RouterBuilder<BoxedSharedService>>,
}

impl RouterBuilder {
    fn new() -> Self {
        RouterBuilder { data: None, not_found: None, inner: HashMap::new() }
    }
}

impl RouterBuilder {
    fn default_not_found(_: Request) -> impl Future<Output = Result<Response, Response>> {
        async { Ok(http::Response::builder().status(404).body("Not Found".into()).unwrap()) }
    }

    /// Add application `data` to router
    pub fn data<T: Send + Sync + 'static>(self, data: T) -> Self {
        self.wrapped_data(Data::new(data))
    }

    /// Add application `data` to router from an existing `Data<T>` object
    pub fn wrapped_data<T: Send + Sync + 'static>(mut self, data: Data<T>) -> Self {
        let mut map = self.data.take().unwrap_or_else(type_map::concurrent::TypeMap::new);
        map.insert(data);
        self.data = Some(map);
        self
    }

    /// Set the `404: Not Found` handler
    pub fn add_not_found<H>(mut self, handler: H) -> Self
    where
        H: SharedService + Send + Sync + 'static,
    {
        self.not_found = Some(BoxedSharedService::new(handler));
        self
    }

    /// Add handler for method and regex. Highest priority wins. Priority is 0 by default.
    pub fn add<H>(self, method: http::Method, regex: &str, handler: H) -> Self
    where
        H: SharedService + Send + Sync + 'static,
    {
        self.add_with_priority(method, regex, 0, handler)
    }

    /// Add handler for method, regex, and priority. Highest priority wins.
    pub fn add_with_priority<H>(
        mut self,
        method: http::Method,
        regex: &str,
        priority: i8,
        handler: H,
    ) -> Self
    where
        H: SharedService + Send + Sync + 'static,
    {
        let handler = BoxedSharedService::new(handler);
        let entry = self
            .inner
            .remove(&method)
            .unwrap_or_else(recognizer::Router::build)
            .add_with_priority(regex, priority, handler);
        self.inner.insert(method, entry);

        self
    }

    /// Consumes the builder, returning the finished `Router`
    pub fn finish(self) -> err::Result<Router> {
        let mut inner_router = InnerRouter {
            data: self.data.map(Data::new).map(DataMap),
            not_found: self
                .not_found
                .unwrap_or_else(|| BoxedSharedService::new(Self::default_not_found)),
            routers: MethodMap::new(),
        };

        for (method, builder) in self.inner {
            let router = builder.finish().map_err(err::Error::Recognizer)?;
            inner_router.routers.insert(method, router);
        }

        Ok(Router(Arc::new(inner_router)))
    }
}

impl tower::Service<Request> for Router {
    type Response = Response;
    type Error = Box<dyn std::error::Error + Send + Sync + 'static>;
    type Future =
        std::pin::Pin<Box<dyn Future<Output = Result<Self::Response, Self::Error>> + Send>>;

    fn poll_ready(
        &mut self,
        _cx: &mut std::task::Context<'_>,
    ) -> std::task::Poll<Result<(), Self::Error>> {
        std::task::Poll::Ready(Ok(()))
    }

    fn call(&mut self, mut request: Request) -> Self::Future {

        let service = Arc::clone(&self.0);

        if let Some(router) = service.routers.get(request.method()) {
            if let Ok(recognizer::Match { handler, captures }) = router.recognize(request.uri().path())
            {
                let extensions_mut = request.extensions_mut();

                if let Some(ref data) = self.0.data {
                    extensions_mut.insert(data.clone());
                }

                extensions_mut.insert(Arc::new(captures));

                return handler.0(request);
            }
        }

        if let Some(ref data) = self.0.data {
            request.extensions_mut().insert(data.clone());
        }

        service.not_found.0(request)
    }
}

impl<'a> tower::Service<&'a hyper::server::conn::AddrStream> for Router {
    type Response = Router;
    type Error = Box<dyn std::error::Error + Send + Sync + 'static>;
    type Future = Ready<Result<Self::Response, Self::Error>>;

    fn poll_ready(&mut self, _cx: &mut Context<'_>) -> Poll<Result<(), Self::Error>> {
        Poll::Ready(Ok(()))
    }

    fn call(&mut self, _: &'a hyper::server::conn::AddrStream) -> Self::Future {
        ready(Ok(Router(Arc::clone(&self.0))))
    }
}

/// Extensions to `http::Request` and `http::request::Parts` to support easy access to captures and `State` object
pub trait RequestExtensions {
    /// Any captures provided by the matching `Regex` for the current path
    fn captures(&self) -> Option<Arc<recognizer::Captures>>;
    /// Positional captures parsed into `FromStr` types, in tuple format
    fn parsed_captures<C: recognizer::FromCaptures>(&self) -> err::Result<C> {
        let captures = self.captures().ok_or_else(|| err::Error::CapturesMissing)?;
        Ok(C::from_captures(&*captures).map_err(err::Error::Recognizer)?)
    }
    /// Copy of any `data` passed into the router builder using `data` or `wrapped_data`
    fn data<T: Send + Sync + 'static>(&self) -> Option<Data<T>>;
}

impl RequestExtensions for Request {
    fn captures(&self) -> Option<Arc<recognizer::Captures>> {
        self.extensions().get::<Arc<recognizer::Captures>>().as_ref().map(|x| Arc::clone(x))
    }

    fn data<T: Send + Sync + 'static>(&self) -> Option<Data<T>> {
        self.extensions().get::<DataMap>().and_then(|x| x.0.get::<Data<T>>()).cloned()
    }
}

impl RequestExtensions for http::request::Parts {
    fn captures(&self) -> Option<Arc<recognizer::Captures>> {
        self.extensions.get::<Arc<recognizer::Captures>>().as_ref().map(|x| Arc::clone(x))
    }

    fn data<T: Send + Sync + 'static>(&self) -> Option<Data<T>> {
        self.extensions.get::<DataMap>().and_then(|x| x.0.get::<Data<T>>()).cloned()
    }
}
