extern crate futures;
extern crate http;
extern crate hyper;

extern crate reset_router;

use futures::{Future, IntoFuture};

use reset_router::{Router, Never, bits::Method};

pub type Request = http::Request<hyper::Body>;
pub type Response = http::Response<hyper::Body>;
pub type Result<T> = std::result::Result<T, Response>;

fn main() {


    let router = Router::build()
        .add(Method::GET, r"^/hello/?$", |_req| Ok::<_, Response>(Response::new("Hello".into())))
        .finish()
        .unwrap();

    let addr = "0.0.0.0:3000".parse().unwrap();

    let server =
        ::hyper::Server::bind(&addr).serve(router).map_err(|e| eprintln!("server error: {}", e));

    ::hyper::rt::run(server);
}
