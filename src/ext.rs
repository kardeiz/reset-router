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

pub trait CommonLen {
    fn common_len(&self) -> usize;
}

impl CommonLen for &'static [u8] {
    fn common_len(&self) -> usize {
        self.len()
    }
}
impl CommonLen for Vec<u8> {
    fn common_len(&self) -> usize {
        self.len()
    }
}
impl CommonLen for &'static str {
    fn common_len(&self) -> usize {
        self.len()
    }
}
impl CommonLen for String {
    fn common_len(&self) -> usize {
        self.len()
    }
}

pub trait RequestExtensions {
    fn cookie_jar(&self) -> Option<CookieJar>;
    fn back(&self) -> Option<&str>;
}

impl RequestExtensions for super::Request {
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
    fn set_sized_body<T: CommonLen + Into<::hyper::Body>>(&mut self, t: T);
    fn with_sized_body<T: CommonLen + Into<::hyper::Body>>(self, t: T) -> Response;
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

    fn set_sized_body<T: CommonLen + Into<::hyper::Body>>(&mut self, t: T) {
        use hyper::header::ContentLength;
        self.headers_mut().set(ContentLength(t.common_len() as u64));
        self.set_body(t);
    }

    fn with_sized_body<T: CommonLen + Into<::hyper::Body>>(mut self, t: T) -> Response {
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
    ) -> ::err::Result<()>;
}

impl RouterExtensions for super::Router {
    fn quick_serve<F: Fn() -> Core + 'static + Send + Sync>(
        self,
        num_threads: usize,
        addr: SocketAddr,
        core_gen: F,
    ) -> ::err::Result<()> {
        use std::sync::Arc;
        use futures::Stream;
        use hyper::server::Http;
        use self::net2::unix::UnixTcpBuilderExt;

        fn inner(
            addr: &SocketAddr,
            protocol: Arc<Http>,
            router: Arc<super::Router>,
            mut core: Core,
        ) -> ::err::Result<()> {

            let hdl = core.handle();
            let listener = self::net2::TcpBuilder::new_v4()?
                .reuse_port(true)?
                .bind(addr)?
                .listen(128)?;
            let listener =
                self::tokio_core::net::TcpListener::from_listener(listener, addr, &hdl)?;
            core.run(listener.incoming().for_each(|(socket, addr)| {
                protocol.bind_connection(&hdl, socket, addr, router.clone());
                Ok(())
            }))?;

            Ok(())
        }

        let protocol = Arc::new(Http::new());
        let router = Arc::new(self);
        let core_gen_ref = Arc::new(core_gen);

        for _ in 0..(num_threads - 1) {
            let protocol_c = protocol.clone();
            let router_c = router.clone();
            let core_gen_ref = core_gen_ref.clone();
            ::std::thread::spawn(move || -> ::err::Result<()> {
                inner(&addr, protocol_c, router_c, core_gen_ref())?;
                Ok(())
            });
        }

        inner(&addr, protocol, router, core_gen_ref())?;

        Ok(())

    }
}
