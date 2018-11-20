extern crate failure;
extern crate futures;
extern crate http;
extern crate hyper;

#[macro_use]
extern crate reset_router;

use hyper::rt::Future;

use reset_router::{Router, RequestExtensions};
// use reset_router::add_one;


#[derive(Clone, Debug)]
pub struct State {
    pub greetings: String
}

pub mod other {
    #[route(path=r"^/goodbye$", methods="GET")]
    pub fn goodbye(req: http::Request<hyper::Body>) -> Result<http::Response<hyper::Body>, http::Response<hyper::Body>> {
        Ok(::http::Response::builder()
            .status(200)
            .body("GOODBYE".into())
            .unwrap())
    }
}

#[get(r"^/hello/(.+)$", priority=1)]
pub fn hello(req: http::Request<hyper::Body>) -> Result<http::Response<hyper::Body>, http::Response<hyper::Body>> {

    let greetings = &req.state::<State>().expect("NO STATE").greetings;
    let (name,) = req.parsed_captures::<(String,)>().expect("NO CAPS");
    Ok(::http::Response::builder()
        .status(200)
        .body(format!("{}, {}", greetings, name).into())
        .unwrap())
}

fn main() {
    let router = ::reset_router::Router::build()
        .with_state(State { greetings: "Greetings".into() })
        .add_routes(routes![
            hello,
            other::goodbye
        ])
        .finish()
        .unwrap();

    let addr = "0.0.0.0:3000".parse().unwrap();

    let server =
        ::hyper::Server::bind(&addr).serve(router).map_err(|e| eprintln!("server error: {}", e));

    ::hyper::rt::run(server);
}
