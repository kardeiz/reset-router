extern crate simple_server;

use http::Request;
use http::Response;
use http::method::Method;
use http::response::Builder as ResponseBuilder;

use self::simple_server::Error as SimpleServerError;

pub type SimpleServerRequest<'a> = Request<&'a [u8]>;
pub type SimpleServerContext<'a> = super::Context<SimpleServerRequest<'a>>;
pub type SimpleServerResponse<'a> = Result<Response<&'a [u8]>, SimpleServerError>;
pub type SimpleServerRouter<'a>  = super::Router<(SimpleServerContext<'a>, ResponseBuilder), SimpleServerResponse<'a>>;


impl<'a> super::RequestLike for SimpleServerRequest<'a> {
    fn path(&self) -> &str { self.uri().path() }
    fn query(&self) -> Option<&str> { self.uri().query() }
    fn method(&self) -> &Method { 
        self.method()
    }
}

impl<'a> SimpleServerContext<'a> {    
    #[deprecated]
    pub fn into_inner(self) -> SimpleServerRequest<'a> {
        self.into_request()
    }

    #[deprecated]
    pub fn extract_captures<T: super::CaptureParsing<SimpleServerRequest<'a>>>(&self) -> super::err::Result<T> {
        Ok(T::parse_captures(self)?)
    }
}


impl<'a, F> super::Handler<(SimpleServerContext<'a>, ResponseBuilder), SimpleServerResponse<'a>> for F
where
    F: Fn((SimpleServerContext<'a>, ResponseBuilder)) -> SimpleServerResponse<'a> + Send + Sync,
{
    fn handle(&self, tup: (SimpleServerContext<'a>, ResponseBuilder)) -> SimpleServerResponse<'a> {
        (self)(tup)
    }
}


impl<'a, F> super::IntoBoxedHandler<(SimpleServerContext<'a>, ResponseBuilder), SimpleServerResponse<'a>> for F
where
    F: Fn(SimpleServerContext<'a>, ResponseBuilder) -> SimpleServerResponse<'a> + Sync + Send,
{
    fn into_boxed_handler(self) -> Box<super::Handler<(SimpleServerContext<'a>, ResponseBuilder), SimpleServerResponse<'a>>>
    where
        Self: Sized + 'static,
    {
        Box::new(move |tup: (SimpleServerContext<'a>, ResponseBuilder)| -> SimpleServerResponse<'a> {
            (self)(tup.0, tup.1)
        })
    }
}

impl<'a> SimpleServerRouter<'a> {
    pub fn handle(&self, request: SimpleServerRequest<'a>, response_builder: ResponseBuilder) -> super::err::Result<SimpleServerResponse<'a>> {
        let (handler, context) = self.find_handler_and_context(request)?;
        Ok(handler.handle((context, response_builder)))
    }
}
