extern crate hyper;
extern crate futures;

use self::hyper::server::Request as HyperRequest;
use self::hyper::Method as HyperMethod;
use self::hyper::server::Response as HyperResponse;
use self::hyper::Error as HyperError;
use self::hyper::server::Service as HyperService;

use self::futures::{Future, IntoFuture};

use super::{err, Handler, IntoBoxedHandler, RequestLike};

pub type Context = super::Context<HyperRequest>;
pub type Router = super::Router<Context, BoxFuture<HyperResponse, HyperError>>;

type BoxFuture<I, E> = Box<Future<Item = I, Error = E>>;

pub trait IntoResponse {
    fn into_response(self) -> HyperResponse;
}

impl<T> IntoResponse for T
where
    T: Into<HyperResponse>,
{
    fn into_response(self) -> HyperResponse {
        self.into()
    }
}

impl RequestLike for HyperRequest {
    fn path(&self) -> &str {
        self.path()
    }
    fn query(&self) -> Option<&str> {
        self.query()
    }
    fn method(&self) -> &::http::Method {

        pub const GET: &'static ::http::Method = &::http::Method::GET;
        pub const POST: &'static ::http::Method = &::http::Method::POST;
        pub const PUT: &'static ::http::Method = &::http::Method::PUT;
        pub const PATCH: &'static ::http::Method = &::http::Method::PATCH;
        pub const HEAD: &'static ::http::Method = &::http::Method::HEAD;
        pub const DELETE: &'static ::http::Method = &::http::Method::DELETE;

        match self.method() {
            &HyperMethod::Get => GET,
            &HyperMethod::Post => POST,
            &HyperMethod::Put => PUT,
            &HyperMethod::Patch => PATCH,
            &HyperMethod::Head => HEAD,
            &HyperMethod::Delete => DELETE,
            _ => panic!("Unsupported method"),
        }
    }
}

impl Context {
    /// Splits the `hyper::Body` from the request so that it can be consumed
    pub fn split_body(self) -> (Self, self::hyper::Body) {
        let super::Context {
            request,
            regex_match,
        } = self;
        let (method, uri, version, headers, body) = request.deconstruct();

        let mut request = HyperRequest::new(method, uri);
        *request.headers_mut() = headers;
        request.set_version(version);

        let new = super::Context {
            request,
            regex_match,
        };
        (new, body)
    }
}

impl<F> Handler<Context, BoxFuture<HyperResponse, HyperError>> for F
where
    F: Fn(Context)
       -> BoxFuture<
        HyperResponse,
        HyperError,
    >
        + Send
        + Sync,
{
    fn handle(&self, context: Context) -> BoxFuture<HyperResponse, HyperError> {
        (self)(context)
    }
}


impl<I, S, E, H> IntoBoxedHandler<Context, BoxFuture<HyperResponse, HyperError>> for H
where
    I: IntoFuture<Item = S, Error = E>,
    I::Future: 'static,
    S: IntoResponse,
    E: IntoResponse,
    H: Fn(Context) -> I + Sync + Send,
{
    fn into_boxed_handler(self) -> Box<Handler<Context, BoxFuture<HyperResponse, HyperError>>>
    where
        Self: Sized + 'static,
    {
        Box::new(move |context: Context| -> BoxFuture<HyperResponse, HyperError> {
            Box::new(
                (self)(context)
                    .into_future()
                    .map(|s| s.into_response())
                    .or_else(|e| Ok(e.into_response())),
            )
        })
    }
}

impl Handler<HyperRequest, err::Result<BoxFuture<HyperResponse, HyperError>>> for Router {
    fn handle(&self, request: HyperRequest) -> err::Result<BoxFuture<HyperResponse, HyperError>> {
        let (handler, context) = self.find_handler_and_context(request)?;
        Ok(handler.handle(context))
    }
}

impl HyperService for Router {
    type Request = HyperRequest;
    type Response = HyperResponse;
    type Error = HyperError;
    type Future = BoxFuture<HyperResponse, HyperError>;

    fn call(&self, req: Self::Request) -> Self::Future {
        use self::hyper::StatusCode;
        use self::hyper::header::{ContentLength, ContentType};
        self.handle(req).unwrap_or_else(|_| {
            let msg = "NOT FOUND";
            Box::new(
                Ok(
                    HyperResponse::new()
                        .with_status(StatusCode::NotFound)
                        .with_header(ContentLength(msg.len() as u64))
                        .with_header(ContentType::plaintext())
                        .with_body(msg),
                ).into_future(),
            )
        })
    }
}

#[cfg(feature = "hyper_ext")]
pub mod ext {

    //! Various extension functions for use with Hyper

    extern crate cookie;
    extern crate tokio_core;
    extern crate net2;
    extern crate futures;

    use super::hyper::server::Request as HyperRequest;
    use super::hyper::server::Response as HyperResponse;
    use super::hyper::Error as HyperError;
    use super::hyper::server::Service as HyperService;
    use super::hyper::server::Http as HyperHttp;
    use super::hyper::Body as HyperBody;
    use super::hyper::header as hyper_header;

    use std::net::SocketAddr;

    use self::cookie::{Cookie, CookieJar};
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

    impl RequestExtensions for HyperRequest {
        fn cookie_jar(&self) -> Option<CookieJar> {
            if let Some(cookies) = self.headers().get::<hyper_header::Cookie>() {
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
            self.headers().get::<hyper_header::Referer>().as_ref().map(
                |x| {
                    &***x
                },
            )
        }
    }

    pub trait ResponseExtensions: Sized {
        fn set_cookies(&mut self, jar: &CookieJar);
        fn with_cookies(mut self, jar: &CookieJar) -> Self {
            self.set_cookies(jar);
            self
        }

        fn set_sized_body<T: CommonLen + Into<HyperBody>>(&mut self, t: T);
        fn with_sized_body<T: CommonLen + Into<HyperBody>>(mut self, t: T) -> Self {
            self.set_sized_body(t);
            self
        }
    }

    impl ResponseExtensions for HyperResponse {
        fn set_cookies(&mut self, jar: &CookieJar) {
            let cookies = jar.delta().map(Cookie::to_string).collect::<Vec<_>>();
            if !cookies.is_empty() {
                self.headers_mut().set(hyper_header::SetCookie(cookies));
            }
        }

        fn set_sized_body<T: CommonLen + Into<HyperBody>>(&mut self, t: T) {
            self.headers_mut().set(hyper_header::ContentLength(
                t.common_len() as u64,
            ));
            self.set_body(t);
        }
    }

    pub trait ServiceExtensions {
        fn quick_serve<F: Fn() -> Core + 'static + Send + Sync>(
            self,
            num_threads: usize,
            addr: SocketAddr,
            core_gen: F,
        ) -> ::err::Result<()>;
    }

    impl<T> ServiceExtensions for T
    where
        T: HyperService<
            Request = HyperRequest,
            Response = HyperResponse,
            Error = HyperError,
        >
            + Send
            + Sync
            + 'static,
    {
        fn quick_serve<F: Fn() -> Core + 'static + Send + Sync>(
            self,
            num_threads: usize,
            addr: SocketAddr,
            core_gen: F,
        ) -> ::err::Result<()> {
            use std::sync::Arc;
            use self::futures::Stream;
            use self::net2::unix::UnixTcpBuilderExt;

            fn inner<
                U: HyperService<
                    Request = HyperRequest,
                    Response = HyperResponse,
                    Error = HyperError,
                >
                    + Send
                    + Sync
                    + 'static,
            >(
                addr: &SocketAddr,
                protocol: Arc<HyperHttp>,
                service: Arc<U>,
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
                    protocol.bind_connection(&hdl, socket, addr, service.clone());
                    Ok(())
                }))?;

                Ok(())
            }

            let protocol = Arc::new(HyperHttp::new());
            let service = Arc::new(self);
            let core_gen_ref = Arc::new(core_gen);

            for _ in 0..(num_threads - 1) {
                let protocol_c = protocol.clone();
                let service_c = service.clone();
                let core_gen_ref = core_gen_ref.clone();
                ::std::thread::spawn(move || -> ::err::Result<()> {
                    inner(&addr, protocol_c, service_c, core_gen_ref())?;
                    Ok(())
                });
            }

            inner(&addr, protocol, service, core_gen_ref())?;

            Ok(())

        }
    }
}
