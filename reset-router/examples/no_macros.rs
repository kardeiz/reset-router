extern crate failure;
extern crate futures;
extern crate http;
extern crate hyper;

#[macro_use]
extern crate reset_router;

use futures::{Future, IntoFuture};

use reset_router::{Router, BoxedService, Service, Never, bits::Method};

pub type Request = http::Request<hyper::Body>;
pub type Response = http::Response<hyper::Body>;
pub type Result<T> = std::result::Result<T, Response>;

struct Hello(String);

impl Service for Hello {
    fn call(&self, req: Request) -> Box<Future<Item = Response, Error = Never> + Send> {
        Box::new(Ok(Response::new(String::from(self.0.as_str()).into())).into_future())
    }
}

fn main() {
    let router = Router::build()
        .add(Method::GET, r"^/hello/?$", |_req| Ok::<_, Response>(Response::new("Hello".into())))
        .add(Method::GET, r"^/hello/loud/?$", BoxedService(Box::new(Hello("HELLO".into()))))
        .finish()
        .unwrap();

    let addr = "0.0.0.0:3000".parse().unwrap();

    let server =
        ::hyper::Server::bind(&addr).serve(router).map_err(|e| eprintln!("server error: {}", e));

    ::hyper::rt::run(server);
}
