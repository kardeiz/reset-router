//! A [`RegexSet`](https://doc.rust-lang.org/regex/regex/struct.RegexSet.html) based router for use with Rust web servers
//! (currently only supports Hyper 0.11.6).
//!
//! Similar to and inspired by [reroute](https://github.com/gsquire/reroute), but  potentially
//! faster (no unnecessary string allocations, no hashmaps, and method-first-matching).
//!
//! When used with Hyper, enables request handling for functions that look like `Fn(Request) -> RESPONSE`
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
//! When used with Hyper, usage looks like:
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
//! See [simple.rs](https://github.com/kardeiz/reset-router/blob/master/examples/simple.rs) for an example of use with Hyper.

extern crate regex;
extern crate http;

// extern crate hyper;

#[macro_use]
extern crate error_chain;

use http::Method;

use regex::{Captures, Regex, RegexSet};

use std::default::Default;
use std::ops::{Deref, DerefMut};
use std::str::FromStr;
use std::sync::Arc;

pub mod err {

    //! Error handling with `error-chain`
    error_chain! {
        errors {
            NotFound {
                description("Path not found")
            }
            CapturesError {
                description("Could not parse captures")
            }
            MethodNotSupported {
                description("Method not supported")
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

#[cfg(feature = "hyper")]
pub mod hyper;

pub trait RequestLike {
    fn path(&self) -> &str;
    fn query(&self) -> Option<&str>;
    fn method(&self) -> &Method;
}

pub struct Context<R> {
    request: R,
    regex_match: Option<Arc<Regex>>,
}

impl<R> Deref for Context<R> {
    type Target = R;

    fn deref(&self) -> &Self::Target {
        &self.request
    }
}

impl<R> DerefMut for Context<R> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.request
    }
}

impl<R: RequestLike> Context<R> {
    /// Captures (if any) from the matched path regex
    pub fn captures(&self) -> Option<Captures> {
        self.regex_match.as_ref().and_then(|r| {
            r.captures(RequestLike::path(&self.request))
        })
    }

    /// Parsed capture segments
    ///
    /// Use like:
    ///
    /// ```rust,ignore
    /// let (id, slug): (i32, String) = req.extract_captures().unwrap();
    /// ```
    pub fn extract_captures<C: CaptureExtracting<R>>(&self) -> Result<C, err::Error> {
        Ok(C::extract_captures(self)?)
    }

    /// Consume and return the inner `RequestLike` object
    pub fn into_request(self) -> R {
        self.request
    }

    #[deprecated]
    pub fn into_inner(self) -> R {
        self.into_request()
    }

    pub fn from_request_and_regex(request: R, regex_match: Option<Arc<Regex>>) -> Self {
        Context {
            request,
            regex_match,
        }
    }
}

pub trait CaptureExtracting<R>: Sized {
    fn extract_captures(req: &Context<R>) -> err::Result<Self>;
}

impl<R, T> CaptureExtracting<R> for (T,)
where
    R: RequestLike,
    T: FromStr,
{
    fn extract_captures(context: &Context<R>) -> err::Result<Self> {
        let caps = context.captures().ok_or(err::ErrorKind::CapturesError)?;
        let out_1 = caps.get(1)
            .map(|x| x.as_str())
            .and_then(|x| x.parse().ok())
            .ok_or(err::ErrorKind::CapturesError)?;
        Ok((out_1,))
    }
}

impl<R, T1, T2> CaptureExtracting<R> for (T1, T2)
where
    R: RequestLike,
    T1: FromStr,
    T2: FromStr,
{
    fn extract_captures(context: &Context<R>) -> err::Result<Self> {
        let caps = context.captures().ok_or(err::ErrorKind::CapturesError)?;
        let out_1 = caps.get(1)
            .map(|x| x.as_str())
            .and_then(|x| x.parse().ok())
            .ok_or(err::ErrorKind::CapturesError)?;
        let out_2 = caps.get(2)
            .map(|x| x.as_str())
            .and_then(|x| x.parse().ok())
            .ok_or(err::ErrorKind::CapturesError)?;
        Ok((out_1, out_2))
    }
}

impl<R, T1, T2, T3> CaptureExtracting<R> for (T1, T2, T3)
where
    R: RequestLike,
    T1: FromStr,
    T2: FromStr,
    T3: FromStr,
{
    fn extract_captures(context: &Context<R>) -> err::Result<Self> {
        let caps = context.captures().ok_or(err::ErrorKind::CapturesError)?;
        let out_1 = caps.get(1)
            .map(|x| x.as_str())
            .and_then(|x| x.parse().ok())
            .ok_or(err::ErrorKind::CapturesError)?;
        let out_2 = caps.get(2)
            .map(|x| x.as_str())
            .and_then(|x| x.parse().ok())
            .ok_or(err::ErrorKind::CapturesError)?;
        let out_3 = caps.get(3)
            .map(|x| x.as_str())
            .and_then(|x| x.parse().ok())
            .ok_or(err::ErrorKind::CapturesError)?;
        Ok((out_1, out_2, out_3))
    }
}

/// Provides a way to convert some item into a boxed handler.
///
/// This is useful to converge e.g. disparate functions into a single signature.

pub trait IntoBoxedHandler<T, S> {
    fn into_boxed_handler(self) -> Box<Handler<T, S>>
    where
        Self: Sized + 'static;
}

impl<T, S> IntoBoxedHandler<T, S> for Box<Handler<T, S>> {
    fn into_boxed_handler(self) -> Box<Handler<T, S>> {
        self
    }
}

/// The handler. For example, takes a `Request` and returns a `Response`.
pub trait Handler<T, S>: Send + Sync {
    fn handle(&self, T) -> S;
}

#[derive(Default)]
pub struct MethodMap<T> {
    get: T,
    post: T,
    put: T,
    patch: T,
    head: T,
    delete: T,
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
            _ => Err(err::ErrorKind::MethodNotSupported.into()),
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
            _ => Err(err::ErrorKind::MethodNotSupported.into()),
        }
    }
}

pub struct PathHandlers<T, S> {
    regex_set: RegexSet,
    regexes: Vec<Arc<Regex>>,
    priorities: Vec<usize>,
    handlers: Vec<Box<Handler<T, S>>>,
}

/// The "finished" `Router`. See [`RouterBuilder`](/reset-router/*/reset_router/struct.RouterBuilder.html) for building a `Router`.
pub struct Router<T, S> {
    not_found: Option<Box<Handler<T, S>>>,
    handlers: MethodMap<Option<PathHandlers<T, S>>>,
}

impl<T, S> Router<T, S> {
    pub fn build<'a>() -> RouterBuilder<'a, T, S> {
        RouterBuilder::new()
    }

    fn find_handler_and_context<R: RequestLike>(
        &self,
        request: R,
    ) -> err::Result<(&Box<Handler<T, S>>, Context<R>)> {
        if let Some(path_handlers) =
            self.handlers
                .get(&RequestLike::method(&request))
                .ok()
                .and_then(|x| x.as_ref())
        {
            let priorities = &path_handlers.priorities;
            if let Some(i) = path_handlers
                .regex_set
                .matches(RequestLike::path(&request))
                .iter()
                .min_by(|x, y| priorities[*x].cmp(&priorities[*y]))
            {

                let handler = &path_handlers.handlers[i];
                let regex = &path_handlers.regexes[i];

                let context = Context::from_request_and_regex(request, Some(regex.clone()));

                return Ok((handler, context));
            }
        }

        if let Some(handler) = self.not_found.as_ref() {
            return Ok((handler, Context::from_request_and_regex(request, None)));
        }

        Err(err::ErrorKind::NotFound.into())

    }
}

/// Builder for a [`Router`](/reset-router/*/reset_router/struct.Router.html)
///
/// Please note that you can assign a priority to a handler with `add_with_priority`.
///
/// Default priority is 0. Lowest priority (closer to 0) wins.
pub struct RouterBuilder<'a, T, S> {
    not_found: Option<Box<Handler<T, S>>>,
    path_handler_parts: Vec<(Method, &'a str, usize, Box<Handler<T, S>>)>,
}

impl<'a, T, S> RouterBuilder<'a, T, S> {
    pub fn new() -> Self {
        RouterBuilder {
            not_found: None,
            path_handler_parts: Vec::new(),
        }
    }

    pub fn add_not_found<H>(mut self, handler: H) -> Self
    where
        H: IntoBoxedHandler<T, S> + 'static,
    {
        self.not_found = Some(handler.into_boxed_handler());
        self
    }

    pub fn add<H>(mut self, method: Method, regex: &'a str, handler: H) -> Self
    where
        H: IntoBoxedHandler<T, S> + 'static,
    {
        self.path_handler_parts.push((
            method,
            regex,
            0,
            handler.into_boxed_handler(),
        ));
        self
    }

    pub fn add_with_priority<H>(
        mut self,
        method: Method,
        regex: &'a str,
        priority: usize,
        handler: H,
    ) -> Self
    where
        H: IntoBoxedHandler<T, S> + 'static,
    {
        self.path_handler_parts.push((
            method,
            regex,
            priority,
            handler.into_boxed_handler(),
        ));
        self
    }

    pub fn finish(self) -> err::Result<Router<T, S>> {

        let mut path_handlers_map = ::std::collections::HashMap::new();

        for (method, path, priority, handler) in self.path_handler_parts {
            let &mut (ref mut paths, ref mut priorities, ref mut handlers) =
                path_handlers_map.entry(method).or_insert_with(|| {
                    (Vec::new(), Vec::new(), Vec::new())
                });
            paths.push(path);
            priorities.push(priority);
            handlers.push(handler);
        }

        let mut router = Router {
            not_found: self.not_found,
            handlers: MethodMap::default(),
        };

        for (method, (paths, priorities, handlers)) in path_handlers_map {
            let regex_set = RegexSet::new(paths.iter())?;
            let mut regexes = Vec::new();
            for path in &paths {
                regexes.push(Arc::new(Regex::new(path)?));
            }

            let path_handlers = PathHandlers {
                regex_set,
                regexes,
                priorities,
                handlers,
            };

            *router.handlers.get_mut(&method)? = Some(path_handlers);
        }

        Ok(router)
    }
}

macro_rules! __build__ {
    ($([$add_method:ident, $add_method_with_priority:ident, $method:path]),+) => {

        $(
            pub fn $add_method<H>(self, regex: &'a str, handler: H) -> Self
            where
                H: IntoBoxedHandler<T, S> + 'static
            {
                self.add($method, regex, handler)
            }

            pub fn $add_method_with_priority<H>(
                self,
                regex: &'a str,
                priority: usize,
                handler: H
            ) -> Self
            where
                H: IntoBoxedHandler<T, S> + 'static
            {
                self.add_with_priority($method, regex, priority, handler)
            }
        )*

    }
}

impl<'a, T, S> RouterBuilder<'a, T, S> {
    __build__! {
        [add_get, add_get_with_priority, Method::GET],
        [add_post, add_post_with_priority, Method::POST],
        [add_put, add_put_with_priority, Method::PUT],
        [add_patch, add_patch_with_priority, Method::PATCH],
        [add_head, add_head_with_priority, Method::HEAD],
        [add_delete, add_delete_with_priority, Method::DELETE]
    }
}
