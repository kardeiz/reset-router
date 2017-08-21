//! Provides some extensions to Request, Response, and Router

extern crate cookie;
extern crate tokio_core;
extern crate net2;
extern crate serde;
extern crate serde_json;

use self::cookie::{Cookie, CookieJar};

use hyper::header;
use hyper::Response;

use std::net::SocketAddr;
use self::tokio_core::reactor::Core;

pub trait RequestExtensions {
    fn cookie_jar(&self) -> Option<CookieJar>;
    fn back(&self) -> Option<&str>;
}

pub trait HavingLen: Into<::hyper::Body> {
    fn len_(&self) -> usize;
}

impl HavingLen for &'static [u8] { fn len_(&self) -> usize { (self.len()) } }
impl HavingLen for Vec<u8> { fn len_(&self) -> usize { (self.len()) } }
impl HavingLen for &'static str { fn len_(&self) -> usize { (self.len()) } }
impl HavingLen for String { fn len_(&self) -> usize { (self.len()) } }

impl<'a> RequestExtensions for super::Request<'a> {
    fn cookie_jar(&self) -> Option<CookieJar> {
        if let Some(cookies) = self.headers().get::<header::Cookie>() {
            let mut jar = CookieJar::new();
            for (k, v) in cookies.iter() {
                jar.add_original(Cookie::new(String::from(k), String::from(v)));
            }
            Some(jar)
        } else {
            None
        }
    }

    fn back(&self) -> Option<&str> {
        self.headers()
            .get::<header::Referer>()
            .as_ref()
            .map(|x| &***x)
    }
}

pub trait ResponseExtensions {

    fn set_cookies(&mut self, jar: &CookieJar);
    fn with_cookies(self, jar: &CookieJar) -> Response;

    fn set_sized_body<T: HavingLen>(&mut self, t: T);
    fn with_sized_body<T: HavingLen>(self, t: T) -> Response;

    fn set_json<S: self::serde::Serialize>(&mut self, obj: &S) -> Result<(), self::serde_json::Error>;
    fn with_json<S: self::serde::Serialize>(self, obj: &S) -> Result<Response, self::serde_json::Error>;

}

impl ResponseExtensions for Response {

    fn set_cookies(&mut self, jar: &CookieJar) {
        let cookies = jar.delta().map(Cookie::to_string).collect::<Vec<_>>();
        if !cookies.is_empty() {
            self.headers_mut().set(header::SetCookie(cookies));
        }
    }

    fn with_cookies(mut self, jar: &CookieJar) -> Response {
        self.set_cookies(jar);
        self
    }

    fn set_json<S: self::serde::Serialize>(&mut self, obj: &S) -> Result<(), self::serde_json::Error> {
        use hyper::header::ContentType;
        let out = self::serde_json::to_vec_pretty(obj)?;
        self.headers_mut().set(ContentType(::hyper::mime::APPLICATION_JSON));
        self.set_sized_body(out);
        Ok(())
    }

    fn with_json<S: self::serde::Serialize>(mut self, obj: &S) -> Result<Response, self::serde_json::Error> {
        self.set_json(obj)?;
        Ok(self)
    }

    fn set_sized_body<T: HavingLen>(&mut self, t: T) {
        use hyper::header::ContentLength;
        self.headers_mut().set(ContentLength(t.len_() as u64));
        self.set_body(t);
    }

    fn with_sized_body<T: HavingLen>(mut self, t: T) -> Response {
        self.set_sized_body(t);
        self
    }

}

pub trait RouterExtensions {
    fn quick_serve<F: Fn() -> Core + 'static + Send + Sync>(
        self,
        num_threads: usize,
        addr: SocketAddr,
        core_gen: F,
    );
}

impl RouterExtensions for super::Router {
    fn quick_serve<F: Fn() -> Core + 'static + Send + Sync>(
        self,
        num_threads: usize,
        addr: SocketAddr,
        core_gen: F,
    ) {
        use std::sync::Arc;
        use futures::Stream;
        use hyper::server::Http;
        use self::net2::unix::UnixTcpBuilderExt;

        let core_gen_ref = Arc::new(core_gen);

        fn inner(
            addr: &SocketAddr,
            protocol: Arc<Http>,
            router: Arc<super::Router>,
            mut core: Core,
        ) {

            let hdl = core.handle();
            let listener = self::net2::TcpBuilder::new_v4()
                .unwrap()
                .reuse_port(true)
                .unwrap()
                .bind(addr)
                .unwrap()
                .listen(128)
                .unwrap();
            let listener =
                self::tokio_core::net::TcpListener::from_listener(listener, addr, &hdl).unwrap();
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
            let core_gen_ref = core_gen_ref.clone();
            ::std::thread::spawn(move || inner(&addr, protocol_c, router_c, core_gen_ref()));
        }

        inner(&addr, protocol, router, core_gen_ref());

    }
}
