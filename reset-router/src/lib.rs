extern crate futures;
extern crate http;
extern crate hyper;
extern crate regex;
extern crate bitflags;

// #[macro_use] extern crate debugit;

#[macro_use]
extern crate failure;

extern crate proc_macro_hack;
extern crate reset_router_macros;


#[macro_export] 
pub use reset_router_macros::{
    get,
    route
};

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
}

pub mod bits {
    
    use crate::err;    
    use bitflags::bitflags;

    bitflags! {
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

use std::sync::Arc;
use std::str::FromStr;
use futures::{Future, IntoFuture};
use http::{Method, Request, Response};
use hyper::Body;
use regex::{Regex, RegexSet, Captures};

#[derive(Debug)]
pub enum Never {}

impl std::fmt::Display for Never {
    fn fmt(&self, _: &mut std::fmt::Formatter) -> std::fmt::Result { match *self {} }
}

impl std::error::Error for Never {
    fn description(&self) -> &str { match *self {} }
}

pub trait Handler: Send + Sync {
    fn handle(&self, Request<Body>) -> Box<Future<Item = Response<Body>, Error = Never> + Send>;
}

impl<H> Handler for H
where H: Fn(Request<Body>) -> Box<Future<Item = Response<Body>, Error = Never> + Send> + Send + Sync
{
    fn handle(&self, request: Request<Body>) -> Box<Future<Item = Response<Body>, Error = Never> + Send> { (self)(request) }
}

pub trait IntoBoxedHandler: Sized + 'static {
    fn into_boxed_handler(self) -> Box<Handler + Send>;
}

impl<H, I, S, E> IntoBoxedHandler for H where 
    I: IntoFuture<Item = S, Error = E>,
    I::Future: 'static + Send,
    S: Into<Response<Body>>,
    E: Into<Response<Body>>,
    H: Fn(Request<Body>) -> I + Sync + Send + 'static {
    fn into_boxed_handler(self) -> Box<Handler + Send>  {
        Box::new(move |request: Request<Body>| -> Box<Future<Item = Response<Body>, Error = Never> + Send> {
            Box::new(self(request).into_future().map(|s| s.into()).or_else(|e| Ok(e.into())))
        })
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

pub struct RouterBuilder<'a, S> {
    state: Option<S>,
    not_found: Option<Box<Handler>>,
    path_handler_parts: Vec<(bits::Method, &'a str, usize, Box<Handler>)>
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
        RouterBuilder {
            state: Some(state),
            not_found,
            path_handler_parts
        }
    }

    pub fn add_not_found<H>(mut self, handler: H) -> Self
    where H: IntoBoxedHandler + 'static {
        self.not_found = Some(handler.into_boxed_handler());
        self
    }

    pub fn add<H>(mut self, method: bits::Method, regex: &'a str, handler: H) -> Self
    where H: IntoBoxedHandler + 'static {
        self.path_handler_parts.push((method, regex, 0, handler.into_boxed_handler()));
        self
    }

    pub fn add_with_priority<H>(
        mut self,
        method: bits::Method,
        regex: &'a str,
        priority: usize,
        handler: H
    ) -> Self
    where
        H: IntoBoxedHandler + 'static
    {
        self.path_handler_parts.push((method, regex, priority, handler.into_boxed_handler()));
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
                    let &mut (ref mut paths, ref mut priorities, ref mut handlers) = path_handlers_map
                        .entry(pair.1.clone())
                        .or_insert_with(|| (Vec::new(), Vec::new(), Vec::new()));
                    paths.push(path);
                    priorities.push(priority);
                    handlers.push(handler.clone());
                }
            }            
        }

        let mut router = InnerRouter {
            state: Arc::new(self.state.unwrap()),
            not_found: self
                .not_found
                .unwrap_or_else(|| Self::default_not_found.into_boxed_handler()),
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
    priorities: Vec<usize>,
    handlers: Vec<Arc<Box<Handler>>>
}

struct InnerRouter<S> {
    state: Arc<S>,
    not_found: Box<Handler>,
    handlers: MethodMap<Option<PathHandlers>>
}

#[derive(Clone)]
pub struct Router<S>(Arc<InnerRouter<S>>);

impl Router<()> {
    pub fn build<'a>() -> RouterBuilder<'a, ()> { RouterBuilder::new() }
}

impl<S: 'static + Send + Sync> Handler for Router<S> {
    fn handle(&self, request: Request<Body>) -> Box<Future<Item = Response<Body>, Error = Never> + Send> { 
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
                    extensions_mut.insert(State(self.0.state.clone()));
                    extensions_mut.insert(MatchingRegex(regex.clone()));
                }

                return handler.handle(request);
            }
        }

        request.extensions_mut().insert(State(self.0.state.clone()));
        self.0.not_found.handle(request)
    }
}

pub struct State<S>(pub S);
pub struct MatchingRegex(Arc<Regex>);

pub trait RequestExtensions {
    fn captures(&self) -> Option<Captures>;
    fn parsed_captures<C: CaptureParsing>(&self) -> err::Result<C>;
    fn state<S: Send + Sync + 'static>(&self) -> Option<Arc<S>>;
}

impl RequestExtensions for Request<Body> {
    fn captures(&self) -> Option<Captures> {
        self.extensions().get::<MatchingRegex>()
            .and_then(|r| r.0.captures(self.uri().path()) )
    }
    fn parsed_captures<C: CaptureParsing>(&self) -> err::Result<C> {
        Ok(C::parse_captures(self)?)
    }
    fn state<S: Send + Sync + 'static>(&self) -> Option<Arc<S>> {
        self.extensions().get::<State<Arc<S>>>()
            .as_ref()
            .map(|x| x.0.clone() )
    }
}

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

impl<S: 'static + Send + Sync> ::hyper::service::Service for Router<S> {
    type Error = Never;
    type Future = Box<Future<Item = Response<Self::ResBody>, Error = Self::Error> + Send>;
    type ReqBody = Body;
    type ResBody = Body;

    fn call(&mut self, req: Request<Self::ReqBody>) -> Self::Future {
        self.handle(req)
    }
}

impl<S: 'static + Send + Sync + Clone> ::hyper::service::NewService for Router<S> {
    type Error = <Router<S> as ::hyper::service::Service>::Error;
    type Future = ::futures::future::FutureResult<Router<S>, Never>;
    type InitError = Never;
    type ReqBody = <Router<S> as ::hyper::service::Service>::ReqBody;
    type ResBody = <Router<S> as ::hyper::service::Service>::ResBody;
    type Service = Router<S>;

    fn new_service(&self) -> Self::Future { 
        ::futures::future::ok(self.clone()) 
    }
}
