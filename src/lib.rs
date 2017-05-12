extern crate hyper;
extern crate regex;
extern crate futures;

#[macro_use]
extern crate error_chain;

#[cfg(feature="extensions")]
#[macro_use]
extern crate lazy_static;

#[cfg(feature="extensions")]
extern crate conduit_mime_types;

#[cfg(feature="extensions")]
extern crate tokio_core;

#[cfg(feature="extensions")]
extern crate bytes;

#[cfg(feature="extensions")]
pub mod extensions;

use hyper::Method;
use hyper::server::Request as HyperRequest;
use hyper::server::Response;
use hyper::server::Service;

use regex::{Regex, RegexSet, Captures};
use futures::Future;

use std::ops::{Deref, DerefMut};

pub mod err {
    error_chain! {
        errors {
            NotFoundNotSet {
                description("Did not set not_found handler")
            }
            #[cfg(feature="extensions")]
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

pub struct Request<'a> {
    inner: HyperRequest,
    regex_match: Option<&'a Regex>
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
            regex_match: None
        }
    }
}


impl<'a> Request<'a> {
    fn captures(&self) -> Option<Captures> {
        self.regex_match
            .and_then(|r| r.captures(self.path()))
    }
}


impl<T: Future<Item=Response, Error=::hyper::Error>> Router<T> {
    pub fn build<'a>() -> RouterBuilder<'a, T> { RouterBuilder::default() }
}

pub trait Handler<T: Future<Item=Response, Error=::hyper::Error>>: Send + Sync {
    fn handle(&self, Request) -> T;
}

impl<F, E, T> Handler<T> for F 
    where T: Future<Item=Response, Error=::hyper::Error>, 
        E: Into<T>, 
        F: Fn(Request) -> Result<T, E> + Sync + Send {
    fn handle(&self, req: Request) -> T {
        self(req).unwrap_or_else(|e| e.into() )
    }
}

impl<'a, T: Future<Item=Response, Error=::hyper::Error>> RouterBuilder<'a, T> {
    pub fn add_not_found<B>(mut self, handler: B) -> Self where B: Handler<T> + 'static {
        self.not_found = Some(Box::new(handler));
        self
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

        pub struct Router<T> {
            not_found: Box<Handler<T>>,
            $(
                $regexes_for_x: Option<Vec<Regex>>,
                $regex_set_for_x: Option<RegexSet>,
                $priorities_for_x: Option<Vec<usize>>,
                $handlers_for_x: Option<Vec<Box<Handler<T>>>>
            ),+
        }

        pub struct RouterBuilder<'a, T> {
            not_found: Option<Box<Handler<T>>>,
            $(
                $strs_for_x: Option<Vec<&'a str>>,
                $priorities_for_x: Option<Vec<usize>>,
                $handlers_for_x: Option<Vec<Box<Handler<T>>>>
            ),+
        }

        impl<'a, T: Future<Item=Response, Error=::hyper::Error>> std::default::Default for RouterBuilder<'a, T> {
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

        impl<'a, T: Future<Item=Response, Error=::hyper::Error>> RouterBuilder<'a, T> {
            $(
                pub fn $add_x<B>(mut self, re: &'a str, handler: B) -> Self where B: Handler<T> + 'static {
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
                pub fn $add_x_with_priority<B>(mut self, re: &'a str, priority: usize, handler: B) -> Self where B: Handler<T> + 'static {
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

            pub fn finish(self) -> ::err::Result<Router<T>> {                
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

        impl<T: Future<Item=Response, Error=::hyper::Error>> Service for Router<T> {
            type Request = HyperRequest;
            type Response = Response;
            type Error = hyper::Error;
            type Future = T;

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
                                return handler.handle(new_req);
                            } 
                        },
                    )+
                    _ => {}
                }
                self.not_found.handle(new_req)
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