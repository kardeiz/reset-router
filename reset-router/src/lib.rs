//! # reset-router
//!
//! A [`RegexSet`](https://doc.rust-lang.org/regex/regex/struct.RegexSet.html) based router for use with async Hyper.
//!
//! Provides optional attribute based routing with Rust 1.30's proc_macro_attributes.
//!
//! Your handler functions should look like:
//!
//! ```rust
//! Fn(http::Request<hyper::Body>) -> I
//! where
//!     I: IntoFuture<Item = S, Error = E>,
//!     I::Future: 'static + Send,
//!     S: Into<http::Response<hyper::Body>>,
//!     E: Into<http::Response<hyper::Body>>,
//! ```
//!
//! Pretty straightforward! You can return something as simple as `Ok(Response::new())`. You don't have to worry about futures
//! unless you need to read the request body or interact with other future-aware things.
//!
//! ## Usage:
//!
//! ```rust
//!
//! use reset_router::RequestExtensions;
//!
//! // Example handler from Rocket
//! #[get(r"^/hello/([^/]+)/(\d+)$")]
//! pub fn hello(req: Request) -> Result<Response> {    
//!     let (name, age) = req.parsed_captures::<(String, u8)>()?;
//!     Ok(::http::Response::builder()
//!         .status(200)
//!         .body(format!("Hello, {} year old named {}!", age, name).into())
//!         .unwrap())
//! }
//!
//! fn main() {
//!     let router = reset_router::Router::build()
//!         .add_routes(routes![hello])
//!         .finish()
//!         .unwrap();
//!
//!     let addr = "0.0.0.0:3000".parse().unwrap();
//!
//!     let server =
//!         hyper::Server::bind(&addr).serve(router).map_err(|e| eprintln!("server error: {}", e));
//!
//!     hyper::rt::run(server);
//! }
//! ```
//!
//! `RequestExtensions` provides easy access to your route regex captures, as well as access to an optional `State` object. See [simple.rs](https://github.com/kardeiz/reset-router/blob/master/reset-router/examples/simple.rs) for an example.
//!
//! If you prefer to keep all your path regexes in one place, of if you want to use closures, you can still use the old style:
//!
//! ```rust
//!
//! // Use this custom bitflags instead of http::Method for easy `BitOr` style method combinations
//! use reset_router::bits::Method;
//!
//! let router = reset_router::Router::build()
//!     .add(Method::GET | method::POST, r"^/hello/([^/]+)/(\d+)$", hello)
//!     .finish()
//!     .unwrap();
//! ```
//!
//! License: MIT

extern crate bitflags;
extern crate futures;
extern crate http;
extern crate hyper;
extern crate regex;

#[macro_use]
extern crate failure;

extern crate proc_macro_hack;
extern crate reset_router_macros;

pub use reset_router_macros::{delete, get, head, patch, post, put, route};

use proc_macro_hack::proc_macro_hack;
#[proc_macro_hack]
pub use reset_router_macros::routes;

pub mod err {
    #[derive(Fail, Debug)]
    pub enum Error {
        #[fail(display = "Could not parse captures")]
        Captures,
        #[fail(display = "Method not supported")]
        MethodNotSupported,
        #[fail(display = "{}", _0)]
        Io(#[cause] ::std::io::Error),
        #[fail(display = "{}", _0)]
        Regex(#[cause] ::regex::Error)
    }

    pub type Result<T> = ::std::result::Result<T, Error>;

    impl From<Error> for http::Response<hyper::Body> {
        fn from(t: Error) -> Self {
            http::Response::builder().status(500).body(t.to_string().into()).unwrap()
        }
    }
}

pub mod bits {

    use bitflags::bitflags;

    bitflags! {
        /// Flags for supported HTTP methods; allows easy `BitOr` style combinations
        pub struct Method: u32 {
            const GET = 1;
            const POST = 2;
            const PUT = 4;
            const PATCH = 8;
            const HEAD = 16;
            const DELETE = 32;
        }
    }

}

use futures::{Future, IntoFuture};
use http::{Method, Request, Response};
use hyper::Body;
use regex::{Captures, Regex, RegexSet};
use std::{str::FromStr, sync::Arc};

/// Placeholder for unstable `!` type
#[derive(Debug)]
pub enum Never {}

impl std::fmt::Display for Never {
    fn fmt(&self, _: &mut std::fmt::Formatter) -> std::fmt::Result { match *self {} }
}

impl std::error::Error for Never {
    fn description(&self) -> &str { match *self {} }
}

// type Handler =
//     Fn(Request<Body>) -> Box<Future<Item = Response<Body>, Error = Never> + Send> + Send + Sync;

// /// Container for the boxed handler functions
// pub struct BoxedHandler(Box<Handler>);

// impl<H, I, S, E> From<H> for BoxedHandler
// where
//     I: IntoFuture<Item = S, Error = E>,
//     I::Future: 'static + Send,
//     S: Into<Response<Body>>,
//     E: Into<Response<Body>>,
//     H: Fn(Request<Body>) -> I + Sync + Send + 'static
// {
//     fn from(t: H) -> Self {
//         BoxedHandler(Box::new(move |request: Request<Body>| -> Box<Future<Item = Response<Body>, Error = Never> + Send> {
//             Box::new(t(request).into_future().map(|s| s.into()).or_else(|e| Ok(e.into())))
//         }))
//     }
// }

pub trait Service: Send + Sync {
    fn call(&self, req: Request<Body>) -> Box<Future<Item = Response<Body>, Error = Never> + Send>;
}

pub struct ServiceFn<H>(H);

impl<H> Service for ServiceFn<H> where H: Fn(Request<Body>) -> Box<Future<Item = Response<Body>, Error = Never> + Send> + Send + Sync {
    fn call(&self, req: Request<Body>) -> Box<Future<Item = Response<Body>, Error = Never> + Send> {
        (&self.0)(req)
    }
}

pub struct BoxedService(pub Box<Service>);

impl<H, I, S, E> From<H> for BoxedService
where
    I: IntoFuture<Item = S, Error = E>,
    I::Future: 'static + Send,
    S: Into<Response<Body>>,
    E: Into<Response<Body>>,
    H: Fn(Request<Body>) -> I + Sync + Send + 'static
{
    fn from(t: H) -> Self {
        BoxedService(Box::new(ServiceFn(move |request: Request<Body>| -> Box<Future<Item = Response<Body>, Error = Never> + Send> {
            Box::new(t(request).into_future().map(|s| s.into()).or_else(|e| Ok(e.into())))
        })))
    }
}

#[derive(Default)]
struct MethodMap<T> {
    get: T,
    post: T,
    put: T,
    patch: T,
    head: T,
    delete: T
}

impl<T> MethodMap<T> {
    fn get(&self, method: &Method) -> err::Result<&T> {
        match *method {
            Method::GET => Ok(&self.get),
            Method::POST => Ok(&self.post),
            Method::PUT => Ok(&self.put),
            Method::PATCH => Ok(&self.patch),
            Method::HEAD => Ok(&self.head),
            Method::DELETE => Ok(&self.delete),
            _ => Err(err::Error::MethodNotSupported)
        }
    }

    fn get_mut(&mut self, method: &Method) -> err::Result<&mut T> {
        match *method {
            Method::GET => Ok(&mut self.get),
            Method::POST => Ok(&mut self.post),
            Method::PUT => Ok(&mut self.put),
            Method::PATCH => Ok(&mut self.patch),
            Method::HEAD => Ok(&mut self.head),
            Method::DELETE => Ok(&mut self.delete),
            _ => Err(err::Error::MethodNotSupported)
        }
    }
}

/// Builder for a `Router`
pub struct RouterBuilder<'a, S> {
    state: Option<S>,
    not_found: Option<BoxedService>,
    path_handler_parts: Vec<(bits::Method, &'a str, u8, BoxedService)>
}

impl<'a> RouterBuilder<'a, ()> {
    pub fn new() -> Self {
        RouterBuilder { state: None, not_found: None, path_handler_parts: Vec::new() }
    }
}

impl<'a, S: 'static> RouterBuilder<'a, S> {
    fn default_not_found(_: Request<Body>) -> Result<Response<Body>, Response<Body>> {
        Ok(http::Response::builder().status(404).body("Not found".into()).unwrap())
    }

    pub fn with_state<O>(self, state: O) -> RouterBuilder<'a, O> {
        let RouterBuilder { not_found, path_handler_parts, .. } = self;
        RouterBuilder { state: Some(state), not_found, path_handler_parts }
    }

    pub fn add_not_found<H>(mut self, handler: H) -> Self
    where H: Into<BoxedService> + 'static {
        self.not_found = Some(handler.into());
        self
    }

    /// Add handler for method and regex. Priority is 0 by default.
    pub fn add<H>(mut self, method: bits::Method, regex: &'a str, handler: H) -> Self
    where H: Into<BoxedService> + 'static {
        self.path_handler_parts.push((method, regex, 0, handler.into()));
        self
    }

    pub fn add_routes(mut self, routes: Vec<(u32, &'a str, u8, BoxedService)>) -> Self {
        for route in routes {
            let (method_bits, regex, priority, handler) = route;
            let method = bits::Method::from_bits_truncate(method_bits);
            self = self.add_with_priority(method, regex, priority, handler);
        }
        self
    }

    /// Specify the priority for this regex match.
    /// Default is 0. Lowest match wins.
    pub fn add_with_priority<H>(
        mut self,
        method: bits::Method,
        regex: &'a str,
        priority: u8,
        handler: H
    ) -> Self
    where
        H: Into<BoxedService> + 'static
    {
        self.path_handler_parts.push((method, regex, priority, handler.into()));
        self
    }

    pub fn finish(self) -> err::Result<Router<S>> {
        let mut path_handlers_map = ::std::collections::HashMap::new();

        let pairs = &[
            (bits::Method::GET, Method::GET),
            (bits::Method::POST, Method::POST),
            (bits::Method::PUT, Method::PUT),
            (bits::Method::PATCH, Method::PATCH),
            (bits::Method::HEAD, Method::HEAD),
            (bits::Method::DELETE, Method::DELETE)
        ];

        for (method, path, priority, handler) in self.path_handler_parts {
            let handler = Arc::new(handler);
            for pair in pairs.iter() {
                if method.contains(pair.0) {
                    let &mut (ref mut paths, ref mut priorities, ref mut handlers) =
                        path_handlers_map
                            .entry(pair.1.clone())
                            .or_insert_with(|| (Vec::new(), Vec::new(), Vec::new()));
                    paths.push(path);
                    priorities.push(priority);
                    handlers.push(handler.clone());
                }
            }
        }

        let mut router = InnerRouter {
            state: self.state.map(Arc::new),
            not_found: self.not_found.unwrap_or_else(|| Self::default_not_found.into()),
            handlers: MethodMap::default()
        };

        for (method, (paths, priorities, handlers)) in path_handlers_map {
            let regex_set = RegexSet::new(paths.iter()).map_err(err::Error::Regex)?;
            let mut regexes = Vec::new();
            for path in &paths {
                regexes.push(Arc::new(Regex::new(path).map_err(err::Error::Regex)?));
            }

            let path_handlers = PathHandlers { regex_set, regexes, priorities, handlers };

            *router.handlers.get_mut(&method)? = Some(path_handlers);
        }

        Ok(Router(Arc::new(router)))
    }
}

struct PathHandlers {
    regex_set: RegexSet,
    regexes: Vec<Arc<Regex>>,
    priorities: Vec<u8>,
    handlers: Vec<Arc<BoxedService>>
}

struct InnerRouter<S> {
    state: Option<Arc<S>>,
    not_found: BoxedService,
    handlers: MethodMap<Option<PathHandlers>>
}

/// The routing service
#[derive(Clone)]
pub struct Router<S>(Arc<InnerRouter<S>>);

impl Router<()> {
    pub fn build<'a>() -> RouterBuilder<'a, ()> { RouterBuilder::new() }
}

impl<S: 'static + Send + Sync> Router<S> {
    fn inner_call(
        &self,
        request: Request<Body>
    ) -> Box<Future<Item = Response<Body>, Error = Never> + Send>
    {
        let mut request = request;

        if let Some(path_handlers) =
            self.0.handlers.get(request.method()).ok().and_then(|x| x.as_ref())
        {
            let priorities = &path_handlers.priorities;
            if let Some(i) = path_handlers
                .regex_set
                .matches(request.uri().path())
                .iter()
                .min_by(|x, y| priorities[*x].cmp(&priorities[*y]))
            {
                let handler = &path_handlers.handlers[i];
                let regex = &path_handlers.regexes[i];

                {
                    let extensions_mut = request.extensions_mut();

                    if let Some(ref state) = self.0.state {
                        extensions_mut.insert(State(state.clone()));
                    }

                    extensions_mut.insert(MatchingRegex(regex.clone()));
                }

                return (&handler.0).call(request);
            }
        }

        if let Some(ref state) = self.0.state {
            request.extensions_mut().insert(State(state.clone()));
        }

        (&(&self.0.not_found).0).call(request)
    }
}

struct State<S>(pub S);
struct MatchingRegex(Arc<Regex>);

/// Extensions to `http::Request` to support easy access to captures and `State` object
pub trait RequestExtensions {
    fn captures(&self) -> Option<Captures>;
    fn parsed_captures<C: CaptureParsing>(&self) -> err::Result<C>;
    fn state<S: Send + Sync + 'static>(&self) -> Option<Arc<S>>;
}

impl RequestExtensions for Request<Body> {
    /// Any captures provided by the matching `Regex` for the current path
    fn captures(&self) -> Option<Captures> {
        self.extensions().get::<MatchingRegex>().and_then(|r| r.0.captures(self.uri().path()))
    }

    /// Captures parsed into `FromStr` types, in tuple format
    fn parsed_captures<C: CaptureParsing>(&self) -> err::Result<C> { Ok(C::parse_captures(self)?) }

    /// Copy of any state passed into the router builder using `with_state`
    fn state<S: Send + Sync + 'static>(&self) -> Option<Arc<S>> {
        self.extensions().get::<State<Arc<S>>>().as_ref().map(|x| x.0.clone())
    }
}

/// Implemented for `T: FromStr` tups up to 4
pub trait CaptureParsing: Sized {
    fn parse_captures(request: &Request<Body>) -> err::Result<Self>;
}

impl<U: FromStr> CaptureParsing for (U,) {
    fn parse_captures(request: &Request<Body>) -> err::Result<Self> {
        let captures = request.captures().ok_or(err::Error::Captures)?;
        let out_1 = captures
            .get(1)
            .map(|x| x.as_str())
            .and_then(|x| x.parse().ok())
            .ok_or(err::Error::Captures)?;
        Ok((out_1,))
    }
}

impl<U1: FromStr, U2: FromStr> CaptureParsing for (U1, U2) {
    fn parse_captures(request: &Request<Body>) -> err::Result<Self> {
        let captures = request.captures().ok_or(err::Error::Captures)?;
        let out_1 = captures
            .get(1)
            .map(|x| x.as_str())
            .and_then(|x| x.parse().ok())
            .ok_or(err::Error::Captures)?;
        let out_2 = captures
            .get(2)
            .map(|x| x.as_str())
            .and_then(|x| x.parse().ok())
            .ok_or(err::Error::Captures)?;
        Ok((out_1, out_2))
    }
}

impl<U1: FromStr, U2: FromStr, U3: FromStr> CaptureParsing for (U1, U2, U3) {
    fn parse_captures(request: &Request<Body>) -> err::Result<Self> {
        let captures = request.captures().ok_or(err::Error::Captures)?;
        let out_1 = captures
            .get(1)
            .map(|x| x.as_str())
            .and_then(|x| x.parse().ok())
            .ok_or(err::Error::Captures)?;
        let out_2 = captures
            .get(2)
            .map(|x| x.as_str())
            .and_then(|x| x.parse().ok())
            .ok_or(err::Error::Captures)?;
        let out_3 = captures
            .get(3)
            .map(|x| x.as_str())
            .and_then(|x| x.parse().ok())
            .ok_or(err::Error::Captures)?;
        Ok((out_1, out_2, out_3))
    }
}

impl<U1: FromStr, U2: FromStr, U3: FromStr, U4: FromStr> CaptureParsing for (U1, U2, U3, U4) {
    fn parse_captures(request: &Request<Body>) -> err::Result<Self> {
        let captures = request.captures().ok_or(err::Error::Captures)?;
        let out_1 = captures
            .get(1)
            .map(|x| x.as_str())
            .and_then(|x| x.parse().ok())
            .ok_or(err::Error::Captures)?;
        let out_2 = captures
            .get(2)
            .map(|x| x.as_str())
            .and_then(|x| x.parse().ok())
            .ok_or(err::Error::Captures)?;
        let out_3 = captures
            .get(3)
            .map(|x| x.as_str())
            .and_then(|x| x.parse().ok())
            .ok_or(err::Error::Captures)?;
        let out_4 = captures
            .get(4)
            .map(|x| x.as_str())
            .and_then(|x| x.parse().ok())
            .ok_or(err::Error::Captures)?;
        Ok((out_1, out_2, out_3, out_4))
    }
}

impl<S: 'static + Send + Sync> ::hyper::service::Service for Router<S> {
    type Error = Never;
    type Future = Box<Future<Item = Response<Self::ResBody>, Error = Self::Error> + Send>;
    type ReqBody = Body;
    type ResBody = Body;

    fn call(&mut self, req: Request<Self::ReqBody>) -> Self::Future { self.inner_call(req) }
}

impl<S: 'static + Send + Sync + Clone> ::hyper::service::NewService for Router<S> {
    type Error = <Router<S> as ::hyper::service::Service>::Error;
    type Future = ::futures::future::FutureResult<Router<S>, Never>;
    type InitError = Never;
    type ReqBody = <Router<S> as ::hyper::service::Service>::ReqBody;
    type ResBody = <Router<S> as ::hyper::service::Service>::ResBody;
    type Service = Router<S>;

    fn new_service(&self) -> Self::Future { ::futures::future::ok(self.clone()) }
}
