extern crate hyper;
extern crate regex;
extern crate futures;
extern crate tokio_core;
extern crate net2;

#[macro_use]
extern crate error_chain;

use hyper::Method;
use hyper::server::Request as HyperRequest;
use hyper::server::Response;
use hyper::server::Service;
use hyper::server::Http;

use regex::{Regex, RegexSet, Captures};
use futures::Future;
// use futures::future::BoxFuture;

// use std::convert::Into;
use std::ops::{Deref, DerefMut};
use std::str::FromStr;
use std::net::SocketAddr;

pub mod err {
    error_chain! {
        errors {
            NotFoundNotSet {
                description("Did not set not_found handler")
            }
            CapturesError {
                description("Could not parse captures")
            }
        }

        foreign_links {
            Io(::std::io::Error);
            Regex(::regex::Error);
        }
    }

}

use err::{Error, ErrorKind};

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

impl<'a> Request<'a> {
    pub fn captures(&self) -> Option<Captures> {
        self.regex_match.and_then(|r| r.captures(self.path()))
    }

    pub fn extract_captures<T: CaptureExtraction>(&self) -> Result<T, Error> {
        Ok(T::extract_captures(self)?)
    }
}

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

impl<T: Future<Item = Response, Error = E> + Send, E: Into<Response>> Router<T, E> {
    pub fn build<'a>() -> RouterBuilder<'a, T, E> {
        RouterBuilder::default()
    }
}

pub trait Handler<T: Future<Item = Response, Error = E> + 'static + Send, E: Into<Response>>
    : 'static + Send + Sync {
    fn handle(&self, Request) -> T;
}

impl<F, T, E> Handler<T, E> for F
where
    T: Future<Item = Response, Error = E> + 'static + Send,
    E: Into<Response>,
    F: Fn(Request) -> T + Sync + Send + 'static,
{
    fn handle(&self, req: Request) -> T {
        self(req)
    }
}

impl<'a, T: Future<Item = Response, Error = E> + 'static + Send, E: Into<Response>>
    RouterBuilder<'a, T, E> {
    pub fn add_not_found<B>(mut self, handler: B) -> Self
    where
        B: Handler<T, E> + 'static,
    {
        self.not_found = Some(Box::new(handler));
        self
    }
}

impl<T: Future<Item = Response, Error = E> + 'static + Send, E: Into<Response> + 'static>
    Router<T, E> {
    pub fn quick_serve(self, num_threads: usize, addr: SocketAddr) {
        use std::sync::Arc;
        use net2::unix::UnixTcpBuilderExt;
        use futures::Stream;

        fn inner<
            T: Future<Item = Response, Error = E> + 'static + Send,
            E: Into<Response> + 'static,
        >(
            addr: &SocketAddr,
            protocol: Arc<Http>,
            router: Arc<Router<T, E>>,
        ) {

            let mut core = tokio_core::reactor::Core::new().unwrap();
            let hdl = core.handle();
            let listener = ::net2::TcpBuilder::new_v4()
                .unwrap()
                .reuse_port(true)
                .unwrap()
                .bind(addr)
                .unwrap()
                .listen(128)
                .unwrap();
            let listener =
                ::tokio_core::net::TcpListener::from_listener(listener, addr, &hdl).unwrap();
            core.run(listener.incoming().for_each(|(socket, addr)| {
                protocol.bind_connection(&hdl, socket, addr, router.clone());
                Ok(())
            })).unwrap();
        }

        let protocol = Arc::new(Http::new());
        let router = Arc::new(self);

        for _ in 0..(num_threads - 1) {
            let protocol_c = protocol.clone();
            let router_c = router.clone();
            ::std::thread::spawn(move || inner(&addr, protocol_c, router_c));
        }

        inner(&addr, protocol, router);

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

        pub struct Router<T, E> {
            not_found: Box<Handler<T, E>>,
            $(
                $regexes_for_x: Option<Vec<Regex>>,
                $regex_set_for_x: Option<RegexSet>,
                $priorities_for_x: Option<Vec<usize>>,
                $handlers_for_x: Option<Vec<Box<Handler<T, E>>>>
            ),+
        }

        pub struct RouterBuilder<'a, T, E> {
            not_found: Option<Box<Handler<T, E>>>,
            $(
                $strs_for_x: Option<Vec<&'a str>>,
                $priorities_for_x: Option<Vec<usize>>,
                $handlers_for_x: Option<Vec<Box<Handler<T, E>>>>
            ),+
        }

        impl<'a, T: Future<Item=Response, Error=E> + Send, E: Into<Response>> std::default::Default for RouterBuilder<'a, T, E> {
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

        impl<'a, T: Future<Item=Response, Error=E> + 'static + Send, E: Into<Response>> RouterBuilder<'a, T, E> {
            $(
                pub fn $add_x<B>(mut self, re: &'a str, handler: B) -> Self where B: Handler<T, E>  {
                    let mut strs = self.$strs_for_x.take().unwrap_or_else(Vec::new);
                    let mut priorities = self.$priorities_for_x.take().unwrap_or_else(Vec::new);
                    let mut handlers = self.$handlers_for_x.take().unwrap_or_else(Vec::new);

                    strs.push(re);
                    priorities.push(0);
                    handlers.push(Box::new(handler));

                    self.$strs_for_x = Some(strs);
                    self.$priorities_for_x = Some(priorities);
                    self.$handlers_for_x = Some(handlers);

                    self
                }
            )*

            $(
                pub fn $add_x_with_priority<B>(mut self, re: &'a str, priority: usize, handler: B) -> Self where B: Handler<T, E>  {
                    let mut strs = self.$strs_for_x.take().unwrap_or_else(Vec::new);
                    let mut priorities = self.$priorities_for_x.take().unwrap_or_else(Vec::new);
                    let mut handlers = self.$handlers_for_x.take().unwrap_or_else(Vec::new);

                    strs.push(re);
                    priorities.push(priority);
                    handlers.push(Box::new(handler));

                    self.$strs_for_x = Some(strs);
                    self.$priorities_for_x = Some(priorities);
                    self.$handlers_for_x = Some(handlers);

                    self
                }
            )*

            pub fn finish(self) -> ::err::Result<Router<T, E>> {                
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

        impl<T: Future<Item=Response, Error=E> + 'static + Send, E: Into<Response> + 'static> Service for Router<T, E> {
            type Request = HyperRequest;
            type Response = Response;
            type Error = hyper::Error;
            type Future = futures::future::BoxFuture<Self::Response, Self::Error>;

            fn call(&self, req: Self::Request) -> Self::Future {
                let mut new_req = Request::from(req);
                match *new_req.method() {
                    $(
                        $hyper_method => {
                            let priorities_opt = self.$priorities_for_x.as_ref();
                            if let Some(i) = self.$regex_set_for_x.iter()
                                .flat_map(|s| s.matches(new_req.path()) )
                                .min_by(|x, y| (&priorities_opt.unwrap()[*x]).cmp(&priorities_opt.unwrap()[*y]) ) {
                                let handler = &self.$handlers_for_x.as_ref().unwrap()[i];
                                let regex = &self.$regexes_for_x.as_ref().unwrap()[i];
                                new_req.regex_match = Some(regex);
                                return handler.handle(new_req).or_else(|e| Ok(e.into()) ).boxed()
                            } 
                        },
                    )+
                    _ => {}
                }
                self.not_found.handle(new_req).or_else(|e| Ok(e.into()) ).boxed()
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
