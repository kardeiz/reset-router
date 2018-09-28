extern crate futures;
extern crate http;
extern crate hyper;
extern crate regex;

#[macro_use]
extern crate failure;

use self::futures::{Future, IntoFuture};

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
        Regex(#[cause] ::regex::Error),
    }

    pub type Result<T> = ::std::result::Result<T, Error>;
}

pub mod common {
    use std::error::Error;
    use std::fmt;

    pub type Never = ::hyper::Error;

    // #[derive(Debug)]
    // pub enum Never {}

    // impl fmt::Display for Never {
    //     fn fmt(&self, _: &mut fmt::Formatter) -> fmt::Result {
    //         match *self {}
    //     }
    // }

    // impl Error for Never {
    //     fn description(&self) -> &str {
    //         match *self {}
    //     }
    // }

    // unsafe impl Send for Never {}
}

use regex::{Captures, Regex, RegexSet};
use http::{Method, Request as HttpRequest, Response as HttpResponse};
use hyper::Body;
use std::str::FromStr;
use std::sync::Arc;

type BoxFuture<I, E> = Box<Future<Item = I, Error = E>>;
type NeverFailingFuture<I> = Box<Future<Item = I, Error = ::common::Never> + Send>;

pub type Request = HttpRequest<Body>;
pub type Response = HttpResponse<Body>;

pub struct Context {
    request: Request,
    regex_match: Option<Arc<Regex>>,
}

impl Context {
    pub fn captures(&self) -> Option<Captures> {
        self.regex_match
            .as_ref()
            .and_then(|r| r.captures(&self.request.uri().path()))
    }

    pub fn parsed_captures<C: CaptureParsing>(&self) -> err::Result<C> {
        Ok(C::parse_captures(self)?)
    }
}

pub trait CaptureParsing: Sized {
    fn parse_captures(ctx: &Context) -> err::Result<Self>;
}

impl<U: FromStr> CaptureParsing for (U,) {
    fn parse_captures(ctx: &Context) -> err::Result<Self> {
        let captures = ctx.captures().ok_or(err::Error::Captures)?;
        let out_1 = captures
            .get(1)
            .map(|x| x.as_str())
            .and_then(|x| x.parse().ok())
            .ok_or(err::Error::Captures)?;
        Ok((out_1,))
    }
}

impl<U1: FromStr, U2: FromStr> CaptureParsing for (U1, U2) {
    fn parse_captures(ctx: &Context) -> err::Result<Self> {
        let captures = ctx.captures().ok_or(err::Error::Captures)?;
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
    fn parse_captures(ctx: &Context) -> err::Result<Self> {
        let captures = ctx.captures().ok_or(err::Error::Captures)?;
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

pub trait Handler: Send + Sync {
    fn handle(&self, Context) -> NeverFailingFuture<Response>;
}

pub trait IntoBoxedHandler {
    fn into_boxed_handler(self) -> Box<Handler + Send>
    where
        Self: Sized + 'static;
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
            _ => Err(err::Error::MethodNotSupported),
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
            _ => Err(err::Error::MethodNotSupported),
        }
    }
}

pub struct PathHandlers {
    regex_set: RegexSet,
    regexes: Vec<Arc<Regex>>,
    priorities: Vec<usize>,
    handlers: Vec<Box<Handler>>,
}

struct InnerRouter {
    not_found: Box<Handler>,
    handlers: MethodMap<Option<PathHandlers>>
}

#[derive(Clone)]
pub struct Router(Arc<InnerRouter>);

impl Router {
    pub fn build<'a>() -> RouterBuilder<'a> {
        RouterBuilder::new()
    }

    fn find_handler_and_context(&self, request: Request) -> (&Box<Handler>, Context) {
        if let Some(path_handlers) = self.0.handlers
            .get(request.method())
            .ok()
            .and_then(|x| x.as_ref())
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

                let context = Context {
                    request,
                    regex_match: Some(regex.clone()),
                };

                return (handler, context);
            }
        }

        (
            &self.0.not_found,
            Context {
                request,
                regex_match: None,
            },
        )
    }
}

pub struct RouterBuilder<'a> {
    not_found: Option<Box<Handler>>,
    path_handler_parts: Vec<(Method, &'a str, usize, Box<Handler>)>,
}

impl<'a> RouterBuilder<'a> {
    pub fn new() -> Self {
        RouterBuilder {
            not_found: None,
            path_handler_parts: Vec::new(),
        }
    }

    pub fn add_not_found<H>(mut self, handler: H) -> Self
    where
        H: IntoBoxedHandler + 'static,
    {
        self.not_found = Some(handler.into_boxed_handler());
        self
    }

    pub fn add<H>(mut self, method: Method, regex: &'a str, handler: H) -> Self
    where
        H: IntoBoxedHandler + 'static,
    {
        self.path_handler_parts
            .push((method, regex, 0, handler.into_boxed_handler()));
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
        H: IntoBoxedHandler + 'static,
    {
        self.path_handler_parts
            .push((method, regex, priority, handler.into_boxed_handler()));
        self
    }

    pub fn finish(self) -> err::Result<Router> {
        
        fn default_not_found(_: Context) -> Result<Response, Response> {
            Ok(::http::Response::builder()
                .status(404)
                .body("Not found".into())
                .unwrap())
        }

        let mut path_handlers_map = ::std::collections::HashMap::new();

        for (method, path, priority, handler) in self.path_handler_parts {
            let &mut (ref mut paths, ref mut priorities, ref mut handlers) = path_handlers_map
                .entry(method)
                .or_insert_with(|| (Vec::new(), Vec::new(), Vec::new()));
            paths.push(path);
            priorities.push(priority);
            handlers.push(handler);
        }

        let mut router = InnerRouter {
            not_found: self.not_found.unwrap_or_else(|| default_not_found.into_boxed_handler() ),
            handlers: MethodMap::default(),
        };

        for (method, (paths, priorities, handlers)) in path_handlers_map {
            let regex_set = RegexSet::new(paths.iter()).map_err(err::Error::Regex)?;
            let mut regexes = Vec::new();
            for path in &paths {
                regexes.push(Arc::new(Regex::new(path).map_err(err::Error::Regex)?));
            }

            let path_handlers = PathHandlers {
                regex_set,
                regexes,
                priorities,
                handlers,
            };

            *router.handlers.get_mut(&method)? = Some(path_handlers);
        }

        Ok(Router(Arc::new(router)))
    }
}

impl<F> Handler for F
where
    F: Fn(Context) -> NeverFailingFuture<Response> + Send + Sync,
{
    fn handle(&self, context: Context) -> NeverFailingFuture<Response> {
        (self)(context)
    }
}

impl<I, S, E, H> IntoBoxedHandler for H
where
    I: IntoFuture<Item = S, Error = E>,
    I::Future: 'static + Send,
    S: Into<Response>,
    E: Into<Response>,
    H: Fn(Context) -> I + Sync + Send,
{
    fn into_boxed_handler(self) -> Box<Handler + Send>
    where
        Self: Sized + 'static,
    {
        Box::new(
            move |context: Context| -> NeverFailingFuture<Response> {
                Box::new(
                    (self)(context)
                        .into_future()
                        .map(|s| s.into())
                        .or_else(|e| Ok(e.into())),
                )
            },
        )
    }
}

impl ::hyper::service::Service for Router {
    type ReqBody = Body;
    type ResBody = Body;
    type Error = ::common::Never;
    type Future = Box<Future<Item=Response, Error=Self::Error> + Send>;

    fn call(&mut self, req: Request) -> Self::Future {
        let (handler, ctx) = self.find_handler_and_context(req);
        handler.handle(ctx)
    }
}

impl ::hyper::service::NewService for Router {
    type Service = Router;
    type ReqBody = <Router as ::hyper::service::Service>::ReqBody;
    type ResBody = <Router as ::hyper::service::Service>::ResBody;
    type Error = <Router as ::hyper::service::Service>::Error;
    type Future = ::futures::future::FutureResult<Router, ::common::Never>;
    type InitError = ::common::Never;

    fn new_service(&self) -> Self::Future {
        ::futures::future::ok(self.clone())
    }
}