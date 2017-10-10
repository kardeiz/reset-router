//! A [`RegexSet`](https://doc.rust-lang.org/regex/regex/struct.RegexSet.html) based router for use with Hyper v0.11.x.
//!
//! Similar to and inspired by [reroute](https://github.com/gsquire/reroute), but for async Hyper and potentially
//! faster (no unnecessary string allocations, no hashmaps, and method-first-matching).
//!
//! Enables request handling for functions that look like `Fn(Request) -> RESPONSE`
//! where `Request` is a thin wrapper around `hyper::server::Request` and
//!
//! ```rust,ignore
//! RESPONSE: IntoFuture<Future = F, Item = S, Error = E>,
//!     F: Future<Item = S, Error = E> + 'static + Send,
//!     S: IntoResponse,
//!     E: IntoResponse
//! ```
//!
//! This means you can return something as simple as `Ok(Response::new())`. You don't have to worry about futures
//! unless you need to read the request body or interact with other future-aware things.
//!
//! Use like:
//!
//! ```rust,ignore
//! let router = Router::build()
//!     .add(Method::Get, r"\A/\z", |_| Ok::<_, Response>(Response::new().with_body("ROOT path")))
//!     .add_not_found(|_| Ok::<_, Response>(Response::new().with_body("Route not found")))
//!     .finish()
//!     .unwrap();
//! router.quick_serve(8, "0.0.0.0:3000".parse().unwrap(), || Core::new().unwrap() );
//! ```
//!
//! See [simple.rs](https://github.com/kardeiz/reset-router/blob/master/examples/simple.rs) for examples.
//!
//!

extern crate futures;
extern crate hyper;
extern crate regex;

#[macro_use]
extern crate error_chain;

use hyper::Method;
use hyper::server::Request as HyperRequest;
use hyper::server::Service;

pub use hyper::server::Response;

use regex::{Captures, Regex, RegexSet};
use futures::{Future, IntoFuture};

use std::ops::{Deref, DerefMut};
use std::str::FromStr;

use std::sync::Arc;

pub mod err {

    //! Error handling with `error-chain`

    error_chain! {
        errors {
            NotFoundNotSet {
                description("Did not set not_found handler")
            }
            CapturesError {
                description("Could not parse captures")
            }
            Regex(t: String) {
                description("regex error")
                display("regex error: '{}'", t)
            }
        }

        foreign_links {
            Io(::std::io::Error);
        }
    }

    impl From<::regex::Error> for Error {
        fn from(t: ::regex::Error) -> Self {
            Error::from_kind(ErrorKind::Regex(t.to_string()))
        }
    }

}

#[cfg(feature = "ext")]
pub mod ext;

pub type BoxFuture<I, E> = Box<Future<Item = I, Error = E>>;

use err::{Error, ErrorKind};

/// Something that can be converted into a `Response` (cannot fail)

pub trait IntoResponse {
    fn into_response(self) -> Response;
}

impl<T> IntoResponse for T
where
    T: Into<Response>,
{
    fn into_response(self) -> Response {
        self.into()
    }
}

/// Hyper `Request` and the matching regex

pub struct Request {
    inner: HyperRequest,
    regex_match: Option<Arc<Regex>>,
}

impl Deref for Request {
    type Target = HyperRequest;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl DerefMut for Request {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl From<HyperRequest> for Request {
    fn from(t: HyperRequest) -> Self {
        Request {
            inner: t,
            regex_match: None,
        }
    }
}

impl From<(HyperRequest, Option<Arc<Regex>>)> for Request {
    fn from(t: (HyperRequest, Option<Arc<Regex>>)) -> Self {
        Request {
            inner: t.0,
            regex_match: t.1,
        }
    }
}

impl Request {
    /// Captures (if any) from the matched path regex

    pub fn captures(&self) -> Option<Captures> {
        self.regex_match.as_ref().and_then(|r| r.captures(self.path()))
    }

    /// Parsed capture segments
    ///
    /// Use like:
    ///
    /// ```rust,ignore
    /// let (id, slug): (i32, String) = req.extract_captures().unwrap();
    /// ```

    pub fn extract_captures<T: CaptureExtraction>(&self) -> Result<T, Error> {
        Ok(T::extract_captures(self)?)
    }

    pub fn into_inner(self) -> HyperRequest {
        self.inner
    }

    pub fn split_body(self) -> (Self, ::hyper::Body) {
        let Request { inner, regex_match } = self;
        let (method, uri, version, headers, body) = inner.deconstruct();

        let mut inner = HyperRequest::new(method, uri);
        *inner.headers_mut() = headers;
        inner.set_version(version);

        let new = Request { inner, regex_match };
        (new, body)
    }


}

/// Trait to provide parsed captures from path

pub trait CaptureExtraction: Sized {
    fn extract_captures(req: &Request) -> Result<Self, Error>;
}

impl<T> CaptureExtraction for (T,)
where
    T: FromStr,
{
    fn extract_captures(req: &Request) -> Result<Self, Error> {
        let caps = req.captures().ok_or(ErrorKind::CapturesError)?;
        let out_1 = caps.get(1)
            .map(|x| x.as_str())
            .and_then(|x| x.parse().ok())
            .ok_or(ErrorKind::CapturesError)?;
        Ok((out_1,))
    }
}

impl<T1, T2> CaptureExtraction for (T1, T2)
where
    T1: FromStr,
    T2: FromStr,
{
    fn extract_captures(req: &Request) -> Result<Self, Error> {
        let caps = req.captures().ok_or(ErrorKind::CapturesError)?;
        let out_1 = caps.get(1)
            .map(|x| x.as_str())
            .and_then(|x| x.parse().ok())
            .ok_or(ErrorKind::CapturesError)?;
        let out_2 = caps.get(2)
            .map(|x| x.as_str())
            .and_then(|x| x.parse().ok())
            .ok_or(ErrorKind::CapturesError)?;
        Ok((out_1, out_2))
    }
}

impl<T1, T2, T3> CaptureExtraction for (T1, T2, T3)
where
    T1: FromStr,
    T2: FromStr,
    T3: FromStr,
{
    fn extract_captures(req: &Request) -> Result<Self, Error> {
        let caps = req.captures().ok_or(ErrorKind::CapturesError)?;
        let out_1 = caps.get(1)
            .map(|x| x.as_str())
            .and_then(|x| x.parse().ok())
            .ok_or(ErrorKind::CapturesError)?;
        let out_2 = caps.get(2)
            .map(|x| x.as_str())
            .and_then(|x| x.parse().ok())
            .ok_or(ErrorKind::CapturesError)?;
        let out_3 = caps.get(3)
            .map(|x| x.as_str())
            .and_then(|x| x.parse().ok())
            .ok_or(ErrorKind::CapturesError)?;
        Ok((out_1, out_2, out_3))
    }
}

// type LocalService = Service<Request=HyperRequest, Response=Response, Error=::hyper::Error, Future=BoxFuture<Response, ::hyper::Error>>;

// pub struct Handlet<T>(pub T);

// pub trait IntoHandlet {
//     fn into_handler(self) -> Box<Handler> where Self: Sized + 'static;
// }

/// Handle the request

// pub trait IntoBoxedService<S: Service> {
//     fn into_boxed_service(self) -> Box<S> where Self: Sized + 'static;
// }

// impl<I, R, E, H, S> IntoBoxedService<S> for H where
//     I: IntoFuture<Item=R, Error=E>,
//     I::Future: 'static,
//     R: IntoResponse,
//     E: IntoResponse,
//     H: Fn(Request) -> I + Sync + Send,
//     S: Service<Request=Request, Response=Response, Error=::hyper::Error, Future=BoxFuture<Response, ::hyper::Error>> {

//     fn into_boxed_service(self) -> Box<S> where Self: Sized + 'static {
//         Box::new(move |req: Request| -> BoxFuture<Response, ::hyper::Error> {
//             Box::new((self)(req)
//                 .into_future()
//                 .map(|s| s.into_response())
//                 .or_else(|e| Ok(e.into_response())))
//         })
//     }
// }



pub trait IntoHandler {
    fn into_handler(self) -> Box<Handler> where Self: Sized + 'static;
}

impl<I, S, E, H> IntoHandler for H where
    I: IntoFuture<Item=S, Error=E>,
    I::Future: 'static,
    S: IntoResponse,
    E: IntoResponse,
    H: Fn(Request) -> I + Sync + Send {

    fn into_handler(self) -> Box<Handler> where Self: Sized + 'static {
        Box::new(move |req: Request| -> BoxFuture<Response, ::hyper::Error> {
            Box::new((self)(req)
                .into_future()
                .map(|s| s.into_response())
                .or_else(|e| Ok(e.into_response())))
        })
    }
}

impl IntoHandler for Box<Handler> {
    fn into_handler(self) -> Box<Handler> where Self: Sized + 'static {
        self
    }
}

pub trait Handler: Send + Sync {
    fn handle(&self, Request) -> BoxFuture<Response, ::hyper::Error>;
}

impl<F> Handler for F
where
    F: Fn(Request) -> BoxFuture<Response, ::hyper::Error> + Send + Sync,
{
    fn handle(&self, req: Request) -> BoxFuture<Response, ::hyper::Error> {
        (self)(req)
    }
}

pub enum Next {
    Request(Request),
    Response(BoxFuture<Response, ::hyper::Error>)
}

pub trait Filter: Send + Sync {
    fn filter(&self, Request, Arc<Box<Handler>>) -> Next;
}

impl<F> Filter for F
where
    F: Fn(Request, Arc<Box<Handler>>) -> Next + Send + Sync,
{
    fn filter(&self, req: Request, handler: Arc<Box<Handler>>) -> Next {
        (self)(req, handler)
    }
}

impl Service for Router {
    type Request = HyperRequest;
    type Response = Response;
    type Error = hyper::Error;
    type Future = BoxFuture<Response, ::hyper::Error>;

    fn call(&self, req: Self::Request) -> Self::Future {        
        self.handle(req)
    }
}

#[derive(Default)]
pub struct MethodMap<T> {
    get: T,
    post: T,
    put: T,
    patch: T,
    head: T,
    delete: T
}

impl<T> MethodMap<T> {
    fn get(&self, method: &Method) -> &T {
        match *method {
            Method::Get => &self.get,
            Method::Post => &self.post,
            Method::Put => &self.put,
            Method::Patch => &self.patch,
            Method::Head => &self.head,
            Method::Delete => &self.delete,
            _ => unimplemented!()
        }
    }

    fn get_mut(&mut self, method: &Method) -> &mut T {
        match *method {
            Method::Get => &mut self.get,
            Method::Post => &mut self.post,
            Method::Put => &mut self.put,
            Method::Patch => &mut self.patch,
            Method::Head => &mut self.head,
            Method::Delete => &mut self.delete,
            _ => unimplemented!()
        }
    }
}


pub struct PathHandlers {
    regex_set: RegexSet,
    regexes: Vec<Arc<Regex>>,
    priorities: Vec<usize>,
    handlers: Vec<Arc<Box<Handler>>>
}

pub struct PathFilters {
    regex_set: RegexSet,
    filters: Vec<Arc<Box<Filter>>>
}


/// The "finished" `Router`. See [`RouterBuilder`](/reset-router/*/reset_router/struct.RouterBuilder.html) for how to build a `Router`.

pub struct Router {
    not_found: Arc<Box<Handler>>,
    handlers: MethodMap<Option<PathHandlers>>,
    filters: MethodMap<Option<PathFilters>>
}

impl Router {

    pub fn build<'a>() -> RouterBuilder<'a> {
        RouterBuilder::default()
    }

    fn base(not_found: Box<Handler>) -> Self {
        Router { 
            not_found: Arc::new(not_found), 
            handlers: MethodMap::default() ,
            filters: MethodMap::default()     
        }
    }

    fn handler_and_regex_for(&self, req: &HyperRequest) -> (Arc<Box<Handler>>, Option<Arc<Regex>>) {
        if let Some(ref path_handlers) = *self.handlers.get(req.method()) {
            let priorities = &path_handlers.priorities;
            if let Some(i) = path_handlers.regex_set.matches(req.path())
                .iter()
                .min_by(|x, y| priorities[*x].cmp(&priorities[*y]) ) {

                let handler = &path_handlers.handlers[i];
                let regex = &path_handlers.regexes[i];
                return (handler.clone(), Some(regex.clone()));
            }
        }
        (self.not_found.clone(), None)
    }

    fn filters_for(&self, req: &Request) -> Option<Vec<Arc<Box<Filter>>>> {
        if let Some(ref path_filters) = *self.filters.get(req.method()) {
            let filters = path_filters.regex_set.matches(req.path())
                .into_iter()
                .map(|i| path_filters.filters[i].clone() )
                .collect();
            Some(filters)
        } else {
            None
        }
    }

    fn handle(&self, req: HyperRequest) -> BoxFuture<Response, ::hyper::Error> {

        let (handler, regex_opt) = self.handler_and_regex_for(&req);
        let mut new_req = Request::from((req, regex_opt));

        if let Some(filters) = self.filters_for(&new_req) {
            for filter in filters {
                match filter.filter(new_req, handler.clone()) {
                    Next::Request(req) => { new_req = req; },
                    Next::Response(res) => { return res; }
                }
            }
        }

        handler.handle(new_req)

    }

}

/// Builder for a [`Router`](/reset-router/*/reset_router/struct.Router.html)
///
/// Please note that you can assign a priority to a handler with `add_with_priority`.
///
/// Default priority is 0. Lowest priority (closer to 0) wins.

#[derive(Default)]
pub struct RouterBuilder<'a> {
    not_found: Option<Box<Handler>>,
    path_handler_parts: Vec<(Method, &'a str, usize, Box<Handler>)>,
    path_filter_parts: Vec<(Method, &'a str, usize, Box<Filter>)>
}

impl<'a> RouterBuilder<'a> {
    
    pub fn add_not_found<H>(mut self, handler: H) -> Self
    where
        H: IntoHandler + 'static,
    {
        self.not_found = Some(handler.into_handler());
        self
    }

    pub fn add_filter<H>(mut self, method: Method, regex: &'a str, filter: H) -> Self where
        H: Filter + 'static {
        self.path_filter_parts.push((method, regex, 0, Box::new(filter)));
        self
    }

    pub fn add<H>(mut self, method: Method, regex: &'a str, handler: H) -> Self where
        H: IntoHandler + 'static {
        self.path_handler_parts.push((method, regex, 0, handler.into_handler()));
        self
    }

    pub fn add_with_priority<H>(mut self, method: Method, regex: &'a str, priority: usize, handler: H) -> Self where
        H: IntoHandler + 'static {
        self.path_handler_parts.push((method, regex, priority, handler.into_handler()));
        self
    }

    pub fn finish(self) -> ::err::Result<Router> {
        
        let not_found = self.not_found.ok_or(::err::ErrorKind::NotFoundNotSet)?;

        let mut router = Router::base(not_found);

        let mut path_handlers_map = ::std::collections::HashMap::new();
        let mut path_filters_map = ::std::collections::HashMap::new();

        for (method, path, priority, handler) in self.path_handler_parts {
            let &mut (ref mut paths, ref mut priorities, ref mut handlers) = 
                path_handlers_map.entry(method).or_insert_with(|| (Vec::new(), Vec::new(), Vec::new() ) );
            paths.push(path);
            priorities.push(priority);
            handlers.push(Arc::new(handler));
        }

        for (method, path, priority, filter) in self.path_filter_parts {
            let &mut (ref mut paths, ref mut priorities, ref mut filters) = 
                path_filters_map.entry(method).or_insert_with(|| (Vec::new(), Vec::new(), Vec::new() ) );
            paths.push(path);
            priorities.push(priority);
            filters.push(Arc::new(filter));
        }

        for (method, (paths, priorities, handlers)) in path_handlers_map {
            let regex_set = RegexSet::new(paths.iter())?;
            let mut regexes = Vec::new();
            for path in paths.iter() {
                regexes.push(Arc::new(Regex::new(path)?));
            }

            let path_handlers = PathHandlers { regex_set, regexes, priorities, handlers };

            *router.handlers.get_mut(&method) = Some(path_handlers);
        }

        for (method, (paths, priorities, filters)) in path_filters_map {
            let regex_set = RegexSet::new(paths.iter())?;

            let path_filters = PathFilters { regex_set, filters };

            *router.filters.get_mut(&method) = Some(path_filters);
        }

        Ok(router)
    }

}
