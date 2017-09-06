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
//!     .add_get(r"\A/\z", |_| Ok::<_, Response>(Response::new().with_body("ROOT path")))
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

pub struct Request<'a> {
    inner: HyperRequest,
    regex_match: Option<&'a Regex>,
}

impl<'a> Deref for Request<'a> {
    type Target = HyperRequest;

    fn deref(&self) -> &Self::Target {
        &self.inner
    }
}

impl<'a> DerefMut for Request<'a> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.inner
    }
}

impl<'a> From<HyperRequest> for Request<'a> {
    fn from(t: HyperRequest) -> Self {
        Request {
            inner: t,
            regex_match: None,
        }
    }
}

impl<'a> From<(HyperRequest, Option<&'a Regex>)> for Request<'a> {
    fn from(t: (HyperRequest, Option<&'a Regex>)) -> Self {
        Request {
            inner: t.0,
            regex_match: t.1,
        }
    }
}

impl<'a> Request<'a> {
    /// Captures (if any) from the matched path regex

    pub fn captures(&self) -> Option<Captures> {
        self.regex_match.and_then(|r| r.captures(self.path()))
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

impl Router {
    pub fn build<'a>() -> RouterBuilder<'a> {
        RouterBuilder::default()
    }
}

/// Handle the request

pub trait Handler: 'static + Send + Sync {
    fn handle(&self, Request) -> Box<Future<Item = Response, Error = ::hyper::Error> + Send>;
}

impl<F> Handler for F
where
    F: Fn(Request)
        -> Box<Future<Item = Response, Error = ::hyper::Error> + Send>
        + 'static
        + Send
        + Sync,
{
    fn handle(&self, req: Request) -> Box<Future<Item = Response, Error = ::hyper::Error> + Send> {
        self(req)
    }
}

impl<'a> RouterBuilder<'a> {
    pub fn add_not_found<I, F, S, E, H>(mut self, handler: H) -> Self
    where
        I: IntoFuture<Future = F, Item = S, Error = E>,
        F: Future<Item = S, Error = E> + 'static + Send,
        S: IntoResponse,
        E: IntoResponse,
        H: Fn(Request) -> I + Sync + Send + 'static,
    {
        self.not_found = Some(Box::new(move |req: Request| {
            Box::new(
                handler(req)
                    .into_future()
                    .map(|s| s.into_response())
                    .or_else(|e| Ok(e.into_response())),
            ) as Box<Future<Item = _, Error = _> + Send>
        }));
        self
    }
}

impl Service for Router {
    type Request = HyperRequest;
    type Response = Response;
    type Error = hyper::Error;
    type Future = Box<Future<Item = Self::Response, Error = Self::Error> + Send>;

    fn call(&self, req: Self::Request) -> Self::Future {
        let (handler, regex_opt) = self.handler_and_regex_for(&req);
        let new_req = Request::from((req, regex_opt));
        handler.handle(new_req)
    }
}


macro_rules! build {
    ($([$regex_set_for_x:ident,
        $regexes_for_x:ident,
        $priorities_for_x:ident,
        $handlers_for_x:ident,
        $strs_for_x:ident,
        $add_x:ident,
        $add_x_with_priority:ident,
        $hyper_method:pat]),+) => {

        /// The "finished" `Router`. See [`RouterBuilder`](/reset_router/struct.RouterBuilder.html) for how to build a `Router`.

        pub struct Router {
            not_found: Box<Handler>,
            $(
                $regexes_for_x: Option<Vec<Regex>>,
                $regex_set_for_x: Option<RegexSet>,
                $priorities_for_x: Option<Vec<usize>>,
                $handlers_for_x: Option<Vec<Box<Handler>>>
            ),+
        }

        /// Builder for a [`Router`](/reset_router/struct.Router.html)
        ///
        /// Please note that you can assign a priority to a handler with, e.g., `add_get_with_priority`.
        ///
        /// Default priority is 0. Lowest priority (closer to 0) wins.

        pub struct RouterBuilder<'a> {
            not_found: Option<Box<Handler>>,
            $(
                $strs_for_x: Option<Vec<&'a str>>,
                $priorities_for_x: Option<Vec<usize>>,
                $handlers_for_x: Option<Vec<Box<Handler>>>
            ),+
        }

        impl<'a> std::default::Default for RouterBuilder<'a> {
            fn default() -> Self {
                RouterBuilder {
                    not_found: None,
                    $(
                        $strs_for_x: None,
                        $priorities_for_x: None,
                        $handlers_for_x: None
                    ),+
                }
            }
        }

        impl<'a> RouterBuilder<'a> {
            $(
                pub fn $add_x<I, F, S, E, H>(mut self, re: &'a str, handler: H) -> Self where
                    I: IntoFuture<Future=F, Item=S, Error=E>,
                    F: Future<Item = S, Error = E> + 'static + Send,
                    S: IntoResponse,
                    E: IntoResponse,
                    H: Fn(Request) -> I + Sync + Send + 'static
                 {
                    let mut strs = self.$strs_for_x.take().unwrap_or_else(Vec::new);
                    let mut priorities = self.$priorities_for_x.take().unwrap_or_else(Vec::new);
                    let mut handlers = self.$handlers_for_x.take().unwrap_or_else(Vec::new);

                    strs.push(re);
                    priorities.push(0);

                    handlers.push(Box::new(move |req: Request| 
                        Box::new(handler(req)
                            .into_future()
                            .map(|s| s.into_response() )
                            .or_else(|e| Ok(e.into_response()))) as Box<Future<Item=_,Error=_> + Send>));

                    self.$strs_for_x = Some(strs);
                    self.$priorities_for_x = Some(priorities);
                    self.$handlers_for_x = Some(handlers);

                    self
                }
            )*

            $(
                pub fn $add_x_with_priority<I, F, S, E, H>(mut self, re: &'a str, priority: usize, handler: H) -> Self where
                    I: IntoFuture<Future=F, Item=S, Error=E>,
                    F: Future<Item = S, Error = E> + 'static + Send,
                    S: IntoResponse,
                    E: IntoResponse,
                    H: Fn(Request) -> I + Sync + Send + 'static
                 {
                    let mut strs = self.$strs_for_x.take().unwrap_or_else(Vec::new);
                    let mut priorities = self.$priorities_for_x.take().unwrap_or_else(Vec::new);
                    let mut handlers = self.$handlers_for_x.take().unwrap_or_else(Vec::new);

                    strs.push(re);
                    priorities.push(priority);
                    handlers.push(Box::new(move |req: Request| 
                        Box::new(handler(req)
                            .into_future()
                            .map(|s| s.into_response() )
                            .or_else(|e| Ok(e.into_response()))) as Box<Future<Item=_,Error=_> + Send>));

                    self.$strs_for_x = Some(strs);
                    self.$priorities_for_x = Some(priorities);
                    self.$handlers_for_x = Some(handlers);

                    self
                }
            )*

            pub fn finish(self) -> ::err::Result<Router> {
                $(
                    let mut $regex_set_for_x = None;
                    let mut $regexes_for_x = None;
                    if let Some(ss) = self.$strs_for_x {
                        $regex_set_for_x = Some(RegexSet::new(ss.iter())?);
                        $regexes_for_x = {
                            let mut out = Vec::new();
                            for s in &ss {
                                out.push(Regex::new(s)?);
                            }
                            Some(out)
                        };
                    }
                )+

                let out = Router {
                    not_found: self.not_found.ok_or(::err::ErrorKind::NotFoundNotSet)?,
                    $(
                        $regex_set_for_x: $regex_set_for_x,
                        $regexes_for_x: $regexes_for_x,
                        $priorities_for_x: self.$priorities_for_x,
                        $handlers_for_x: self.$handlers_for_x
                    ),*
                };

                Ok(out)
            }
        }

        impl Router {

            fn handler_and_regex_for<'a>(&'a self, req: &HyperRequest) -> (&'a Box<Handler>, Option<&'a Regex>) {
                match *req.method() {
                    $(
                        $hyper_method => {
                            if let Some(i) = self.$regex_set_for_x.iter()
                                .flat_map(|s| s.matches(req.path()) )
                                .min_by(|x, y| {
                                    let priorities_opt = self.$priorities_for_x.as_ref();
                                    (&priorities_opt.unwrap()[*x]).cmp(&priorities_opt.unwrap()[*y])
                                }) {
                                let handler = &self.$handlers_for_x.as_ref().unwrap()[i];
                                let regex = &self.$regexes_for_x.as_ref().unwrap()[i];
                                return (handler, Some(regex));
                            }
                        },
                    )+
                    _ => {}
                }
                return (&self.not_found, None);
            }

        }

    }
}

build!{
    [regex_set_for_gets, regexes_for_gets, priorities_for_gets, handlers_for_gets, strs_for_gets, add_get, add_get_with_priority, Method::Get],
    [regex_set_for_posts, regexes_for_posts, priorities_for_posts, handlers_for_posts, strs_for_posts, add_post, add_post_with_priority, Method::Post],
    [regex_set_for_puts, regexes_for_puts, priorities_for_puts, handlers_for_puts, strs_for_puts, add_put, add_put_with_priority, Method::Put],
    [regex_set_for_patchs, regexes_for_patchs, priorities_for_patchs, handlers_for_patchs, strs_for_patchs, add_patch, add_patch_with_priority, Method::Patch],
    [regex_set_for_heads, regexes_for_heads, priorities_for_heads, handlers_for_heads, strs_for_heads, add_head, add_head_with_priority, Method::Head],
    [regex_set_for_deletes, regexes_for_deletes, priorities_for_deletes, handlers_for_deletes, strs_for_deletes, add_delete, add_delete_with_priority, Method::Delete]
}
