/*!
A fast [`RegexSet`](https://doc.rust-lang.org/regex/regex/struct.RegexSet.html) based router for use with async Hyper (0.12).

Provides optional attribute based routing with `proc_macro_attribute`s. Enable the `with-macros` feature flag to use this feature.

Individual handler functions should have the type `H`, where
```rust
    H: Fn(http::Request<Body>) -> I + Sync + Send + 'static,
    I: IntoFuture<Item = S, Error = E>,
    I::Future: 'static + Send,
    S: Into<http::Response<Body>>,
    E: Into<http::Response<Body>>,
```

You can return something as simple as `Ok(Response::new())`. You don't have to worry about futures
unless you need to read the request body or interact with other future-aware things.

## Usage:

```rust
extern crate futures;
extern crate http;
extern crate hyper;

extern crate reset_router;

use futures::{Future};

use reset_router::{bits::Method, Request, Response, Router, RequestExtensions};

#[derive(Clone)]
pub struct State(pub i32);

fn hello(req: Request) -> Result<Response, Response> {
    let (first_name, last_name) = req.parsed_captures::<(String, String)>()?;
    Ok(http::Response::builder()
        .status(200)
        .body(format!("Hello, {} {}!", first_name, last_name).into())
        .unwrap())
}

fn unreliable_add(req: Request) -> Result<Response, Response> {
    let (add1, add2) = req.parsed_captures::<(i32, i32)>()?;

    let state_num: i32 = req.state::<State>().map(|x| x.0 ).unwrap_or(0);

    Ok(http::Response::builder()
        .status(200)
        .body(format!("{} + {} = {}\r\n", add1, add2, add1 + add2 + state_num).into())
        .unwrap())
}

fn main() {

    let router = Router::build()
        .with_state(State(42))
        .add(Method::GET | Method::POST, r"^/hello/([^/]+)/(.+)$", hello)
        .add(http::Method::GET, r"^/add/([\d]+)/([\d]+)$", unreliable_add)
        .finish()
        .unwrap();

    let addr = "0.0.0.0:3000".parse().unwrap();

    let server =
        hyper::Server::bind(&addr).serve(router).map_err(|e| eprintln!("server error: {}", e));

    hyper::rt::run(server);
}
```

Or with attribute based routing:

```rust
extern crate futures;
extern crate http;
extern crate hyper;

#[macro_use]
extern crate reset_router;

use futures::Future;

use reset_router::Router;

#[derive(Clone, Debug)]
pub struct State {
    pub goodbye: String,
}

pub mod handlers {

    use super::State;
    use reset_router::{Request, Response, RequestExtensions};

    #[route(path = "^/goodbye$", methods = "GET, POST")]
    pub fn goodbye(req: Request) -> Result<Response, Response> {
        let state = req.state::<State>().unwrap();
        Ok(http::Response::builder().status(200).body(state.goodbye.clone().into()).unwrap())
    }

    #[get(r"^/hello/([^/]+)/(\d+)$")]
    pub fn hello(req: Request) -> Result<Response, Response> {
        let (name, age) = req.parsed_captures::<(String, u8)>()?;
        Ok(http::Response::builder()
            .status(200)
            .body(format!("Hello, {} year old named {}!", age, name).into())
            .unwrap())
    }
}

fn main() {
    let router = Router::build()
        .with_state(State { goodbye: "Goodbye".into() })
        .add_routes(routes![handlers::hello, handlers::goodbye])
        .finish()
        .unwrap();

    let addr = "0.0.0.0:3000".parse().unwrap();

    let server =
        hyper::Server::bind(&addr).serve(router).map_err(|e| eprintln!("server error: {}", e));

    hyper::rt::run(server);
}
```
*/

#[cfg(feature = "with-macros")]
pub use reset_router_macros::{
    connect, delete, get, head, options, patch, post, put, route, trace,
};

#[cfg(feature = "with-macros")]
use proc_macro_hack::proc_macro_hack;

#[cfg(feature = "with-macros")]
#[proc_macro_hack]
pub use reset_router_macros::routes;

use reset_recognizer as recognizer;

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

/// Contains `bits::Method` bitflags struct to enable `BitOr` style method folding
pub mod bits {

    use bitflags::bitflags;

    bitflags! {
        /// Flags for predefined HTTP methods
        pub struct Method: u32 {
            const OPTIONS = 1;
            const GET = 2;
            const POST = 4;
            const PUT = 8;
            const DELETE = 16;
            const HEAD = 32;
            const TRACE = 64;
            const CONNECT = 128;
            const PATCH = 256;
        }
    }

    impl Method {
        pub(crate) fn matching_http_methods(&self) -> Vec<http::Method> {
            let pairs = &[
                (Method::OPTIONS, http::Method::OPTIONS),
                (Method::GET, http::Method::GET),
                (Method::POST, http::Method::POST),
                (Method::PUT, http::Method::PUT),
                (Method::DELETE, http::Method::DELETE),
                (Method::HEAD, http::Method::HEAD),
                (Method::TRACE, http::Method::TRACE),
                (Method::CONNECT, http::Method::CONNECT),
                (Method::PATCH, http::Method::PATCH),
            ];

            pairs
                .into_iter()
                .flat_map(|(f, m)| if self.contains(*f) { Some(m.clone()) } else { None })
                .collect()
        }
    }

}

use futures::{Future, IntoFuture};
use std::collections::HashMap;

use std::sync::Arc;

/// Convenience wrapper for `http::Request<hyper::Body>`
pub type Request = http::Request<hyper::Body>;

/// Convenience wrapper for `http::Response<hyper::Body>`
pub type Response = http::Response<hyper::Body>;

/// Placeholder for unstable `!` type
#[derive(Debug)]
pub enum Never {}

impl std::fmt::Display for Never {
    fn fmt(&self, _: &mut std::fmt::Formatter) -> std::fmt::Result {
        match *self {}
    }
}

impl std::error::Error for Never {
    fn description(&self) -> &str {
        match *self {}
    }
}

#[doc(hidden)]
pub struct BoxedHandler(
    Arc<dyn Fn(Request) -> Box<dyn Future<Item = Response, Error = Never> + Send> + Send + Sync>,
);

impl Clone for BoxedHandler {
    fn clone(&self) -> Self {
        BoxedHandler(Arc::clone(&self.0))
    }
}

struct FnWrapper<H>(H);

impl<H, I, S, E> From<FnWrapper<H>> for BoxedHandler
where
    I: IntoFuture<Item = S, Error = E>,
    I::Future: 'static + Send,
    S: Into<Response>,
    E: Into<Response>,
    H: Fn(Request) -> I + Sync + Send + 'static,
{
    fn from(FnWrapper(t): FnWrapper<H>) -> Self {
        BoxedHandler(Arc::new(
            move |request: Request| -> Box<dyn Future<Item = Response, Error = Never> + Send> {
                Box::new(t(request).into_future().map(|s| s.into()).or_else(|e| Ok(e.into())))
            },
        ))
    }
}

impl<H, I, S, E> From<H> for BoxedHandler
where
    I: IntoFuture<Item = S, Error = E>,
    I::Future: 'static + Send,
    S: Into<Response>,
    E: Into<Response>,
    H: Fn(Request) -> I + Sync + Send + 'static,
{
    fn from(t: H) -> Self {
        FnWrapper(t).into()
    }
}

#[doc(hidden)]
#[derive(PartialEq, Eq, Hash)]
pub enum MethodKind {
    Bits(bits::Method),
    Http(http::Method),
}

impl MethodKind {
    fn matching_http_methods(&self) -> Vec<http::Method> {
        match self {
            MethodKind::Bits(ref b) => b.matching_http_methods(),
            MethodKind::Http(ref m) => vec![m.clone()],
        }
    }
}

impl From<bits::Method> for MethodKind {
    fn from(t: bits::Method) -> Self {
        MethodKind::Bits(t)
    }
}

impl From<http::Method> for MethodKind {
    fn from(t: http::Method) -> Self {
        MethodKind::Http(t)
    }
}

/// Builder for a `Router`
pub struct RouterBuilder<S> {
    state: Option<S>,
    not_found: Option<BoxedHandler>,
    inner: HashMap<http::Method, recognizer::RouterBuilder<BoxedHandler>>,
}

impl RouterBuilder<()> {
    fn new() -> Self {
        RouterBuilder { state: None, not_found: None, inner: HashMap::new() }
    }
}

impl<S: 'static> RouterBuilder<S> {
    fn default_not_found(_: Request) -> Result<Response, Response> {
        Ok(http::Response::builder().status(404).body("Not Found".into()).unwrap())
    }

    pub fn with_state<O>(self, state: O) -> RouterBuilder<O> {
        let RouterBuilder { not_found, inner, .. } = self;
        RouterBuilder { state: Some(state), not_found, inner }
    }

    pub fn add_not_found<H>(mut self, handler: H) -> Self
    where
        H: Into<BoxedHandler> + 'static,
    {
        self.not_found = Some(handler.into());
        self
    }

    /// Add handler for method and regex. Priority is 0 by default.
    ///
    /// Method can be a `http::Method` or a `bits::Method` flag
    pub fn add<H, I>(self, method: I, regex: &str, handler: H) -> Self
    where
        H: Into<BoxedHandler> + 'static,
        I: Into<MethodKind>,
    {
        self.add_with_priority(method, regex, 0, handler)
    }

    #[doc(hidden)]
    pub fn add_routes(mut self, routes: Vec<(u32, &str, u8, BoxedHandler)>) -> Self {
        for route in routes {
            let (method_bits, regex, priority, handler) = route;
            let method = bits::Method::from_bits_truncate(method_bits);
            self = self.add_with_priority(method, regex, priority, handler);
        }
        self
    }

    /// Add handler for method, regex, and priority. Lowest priority wins.
    ///
    /// Method can be a `http::Method` or a `bits::Method` flag
    pub fn add_with_priority<H, I>(
        mut self,
        method: I,
        regex: &str,
        priority: u8,
        handler: H,
    ) -> Self
    where
        H: Into<BoxedHandler> + 'static,
        I: Into<MethodKind>,
    {
        let handler = handler.into();
        for method in method.into().matching_http_methods() {
            let entry = self
                .inner
                .remove(&method)
                .unwrap_or_else(recognizer::Router::build)
                .add_with_priority(regex, priority, handler.clone());
            self.inner.insert(method, entry);
        }

        self
    }

    /// Consumes the builder, returning the finished `Router`
    pub fn finish(self) -> err::Result<Router<S>> {
        let mut inner_router = InnerRouter {
            state: self.state.map(Arc::new),
            not_found: self.not_found.unwrap_or_else(|| Self::default_not_found.into()),
            routers: MethodMap::new(),
        };

        for (method, builder) in self.inner {
            let router = builder.finish().map_err(err::Error::Recognizer)?;
            inner_router.routers.set(method, router);
        }

        Ok(Router(Arc::new(inner_router)))
    }
}

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

    fn set(&mut self, method: http::Method, t: T) {
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

struct InnerRouter<S> {
    state: Option<Arc<S>>,
    not_found: BoxedHandler,
    routers: MethodMap<recognizer::Router<BoxedHandler>>,
}

/// The router, impls `hyper::service::Service` and `hyper::service::MakeService`
#[derive(Clone)]
pub struct Router<S>(Arc<InnerRouter<S>>);

impl Router<()> {
    pub fn build() -> RouterBuilder<()> {
        RouterBuilder::new()
    }
}

impl<S: 'static + Send + Sync> Router<S> {
    fn inner_call(
        &self,
        request: Request,
    ) -> Box<dyn Future<Item = Response, Error = Never> + Send> {
        let mut request = request;

        if let Some(router) = self.0.routers.get(request.method()) {
            match router.recognize(request.uri().path()) {
                Ok(recognizer::Match { handler, captures }) => {
                    let extensions_mut = request.extensions_mut();

                    if let Some(ref state) = self.0.state {
                        extensions_mut.insert(State(state.clone()));
                    }

                    extensions_mut.insert(Arc::new(captures));

                    return (&handler.0)(request);
                }
                _ => {}
            }
        }

        if let Some(ref state) = self.0.state {
            request.extensions_mut().insert(State(state.clone()));
        }

        (&(&self.0.not_found).0)(request)
    }
}

impl<S: 'static + Send + Sync> hyper::service::Service for Router<S> {
    type Error = Never;
    type Future = Box<dyn Future<Item = http::Response<Self::ResBody>, Error = Self::Error> + Send>;
    type ReqBody = hyper::Body;
    type ResBody = hyper::Body;

    fn call(&mut self, req: http::Request<Self::ReqBody>) -> Self::Future {
        self.inner_call(req)
    }
}

impl<S: 'static + Send + Sync + Clone, Ctx> hyper::service::MakeService<Ctx> for Router<S> {
    type Error = <Router<S> as hyper::service::Service>::Error;
    type Future = futures::future::FutureResult<Router<S>, Never>;
    type MakeError = Never;
    type ReqBody = <Router<S> as hyper::service::Service>::ReqBody;
    type ResBody = <Router<S> as hyper::service::Service>::ResBody;
    type Service = Router<S>;

    fn make_service(&mut self, _: Ctx) -> Self::Future {
        futures::future::ok(self.clone())
    }
}

struct State<S>(pub S);

/// Extensions to `http::Request` and `http::request::Parts` to support easy access to captures and `State` object
pub trait RequestExtensions {
    /// Any captures provided by the matching `Regex` for the current path
    fn captures(&self) -> Option<Arc<recognizer::Captures>>;
    /// Positional captures parsed into `FromStr` types, in tuple format
    fn parsed_captures<C: recognizer::FromCaptures>(&self) -> err::Result<C> {
        let captures = self.captures().ok_or_else(|| err::Error::CapturesMissing)?;
        Ok(C::from_captures(&*captures).map_err(err::Error::Recognizer)?)
    }
    /// Copy of any state passed into the router builder using `with_state`
    fn state<S: Send + Sync + 'static>(&self) -> Option<Arc<S>>;
}

impl RequestExtensions for Request {
    fn captures(&self) -> Option<Arc<recognizer::Captures>> {
        self.extensions().get::<Arc<recognizer::Captures>>().as_ref().map(|x| Arc::clone(x))
    }

    fn state<S: Send + Sync + 'static>(&self) -> Option<Arc<S>> {
        self.extensions().get::<State<Arc<S>>>().as_ref().map(|x| Arc::clone(&x.0))
    }
}

impl RequestExtensions for http::request::Parts {
    fn captures(&self) -> Option<Arc<recognizer::Captures>> {
        self.extensions.get::<Arc<recognizer::Captures>>().as_ref().map(|x| Arc::clone(x))
    }

    fn state<S: Send + Sync + 'static>(&self) -> Option<Arc<S>> {
        self.extensions.get::<State<Arc<S>>>().as_ref().map(|x| Arc::clone(&x.0))
    }
}
