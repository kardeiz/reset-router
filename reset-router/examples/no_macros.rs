extern crate futures;
extern crate http;
extern crate hyper;

extern crate reset_router;

use futures::{Future, IntoFuture};

use reset_router::{bits::Method, Router, RequestExtensions};

pub type Request = http::Request<hyper::Body>;
pub type Response = http::Response<hyper::Body>;
pub type Result<T> = std::result::Result<T, Response>;

fn hello(req: Request) -> Result<Response> {    
    let (name, age) = req.parsed_captures::<(String, u8)>()?;
    Ok(http::Response::builder()
        .status(200)
        .body(format!("Hello, {} year old named {}!", age, name).into())
        .unwrap())
}

fn main() {

    let router = Router::build()
        .add(Method::GET | Method::POST, r"^/hello/([^/]+)/(\d+)$", hello)
        .finish()
        .unwrap();

    let addr = "0.0.0.0:3000".parse().unwrap();

    let server =
        ::hyper::Server::bind(&addr).serve(router).map_err(|e| eprintln!("server error: {}", e));

    ::hyper::rt::run(server);
}
