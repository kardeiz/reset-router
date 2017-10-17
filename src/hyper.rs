extern crate hyper;
extern crate futures;

use self::hyper::server::Request as HyperRequest;
use self::hyper::Method as HyperMethod;
use self::hyper::server::Response as HyperResponse;
use self::hyper::Error as HyperError;
use self::hyper::server::Service as HyperService;

use self::futures::{Future, IntoFuture};

pub type HyperContext = super::Context<HyperRequest>;
pub type BoxFuture<I, E> = Box<Future<Item = I, Error = E>>;

impl super::RequestLike for HyperRequest {
    fn path(&self) -> &str { self.path() }
    fn query(&self) -> Option<&str> { self.query() }
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
            _ => panic!("Unsupported method")
        }
    }
}

impl<I, S, E, H> super::IntoBoxedHandler<HyperRequest, BoxFuture<HyperResponse, HyperError>> for H
where
    I: IntoFuture<Item = S, Error = E>,
    I::Future: 'static,
    S: Into<HyperResponse>,
    E: Into<HyperResponse>,
    H: Fn(HyperContext) -> I + Sync + Send,
{
    fn into_boxed_handler(self) -> Box<super::Handler<HyperRequest, BoxFuture<HyperResponse, HyperError>>>
    where
        Self: Sized + 'static,
    {
        Box::new(move |context: HyperContext| -> BoxFuture<HyperResponse, HyperError> {
            Box::new(
                (self)(context)
                    .into_future()
                    .map(|s| s.into())
                    .or_else(|e| Ok(e.into())),
            )
        })
    }
}

impl HyperService for super::Router<HyperRequest, BoxFuture<HyperResponse, HyperError>> {
    type Request = HyperRequest;
    type Response = HyperResponse;
    type Error = HyperError;
    type Future = BoxFuture<HyperResponse, HyperError>;

    fn call(&self, req: Self::Request) -> Self::Future {
        use self::hyper::StatusCode;
        use self::hyper::header::{ContentLength, ContentType};
        self.handle(req).unwrap_or_else(|_| {
            let msg = "NOT FOUND";
            Box::new(Ok(HyperResponse::new()
                .with_status(StatusCode::NotFound)
                .with_header(ContentLength(msg.len() as u64))
                .with_header(ContentType::plaintext())
                .with_body(msg)).into_future())
        })
    }
}