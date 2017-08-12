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
    fn with_cookies(self, jar: &CookieJar) -> Self;
}

impl ResponseExtensions for Response {
    fn set_cookies(&mut self, jar: &CookieJar) {
        let cookies = jar.delta().map(Cookie::to_string).collect::<Vec<_>>();
        if !cookies.is_empty() {
            self.headers_mut().set(header::SetCookie(cookies));
        }
    }

    fn with_cookies(mut self, jar: &CookieJar) -> Self {
        self.set_cookies(jar);
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

pub struct Json<T = self::serde_json::Value>(pub T);

impl<T: self::serde::Serialize> super::IntoResponse for Json<T> {
    fn into_response(self) -> super::Response {
        use hyper::header::{ContentLength, ContentType};

        let out = self::serde_json::to_string(&self.0).unwrap();
        Response::new()
            .with_status(::hyper::StatusCode::Ok)
            .with_header(ContentLength(out.len() as u64))
            .with_header(ContentType(::hyper::mime::APPLICATION_JSON))
            .with_body(out)
    }
}
