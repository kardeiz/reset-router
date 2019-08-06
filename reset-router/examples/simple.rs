extern crate futures;
extern crate http;
extern crate hyper;

extern crate reset_router;

use futures::Future;

use reset_router::{bits::Method, Request, RequestExtensions, Response, Router};

#[derive(Clone)]
pub struct State(pub i32);

fn hello(req: Request) -> Result<Response, Response> {
    let (first_name, last_name) = req.parsed_captures::<(String, String)>()?;
    Ok(http::Response::builder()
        .status(200)
        .body(format!("Hello, {} {}!", first_name, last_name).into())
        .unwrap())
}

fn unreliable_add(req: Request) -> Result<Response, Response> {
    let (add1, add2) = req.parsed_captures::<(i32, i32)>()?;

    let state_num: i32 = req.state::<State>().map(|x| x.0).unwrap_or(0);

    Ok(http::Response::builder()
        .status(200)
        .body(format!("{} + {} = {}\r\n", add1, add2, add1 + add2 + state_num).into())
        .unwrap())
}

fn main() {
    let router = Router::build()
        .with_state(State(42))
        .add(Method::GET | Method::POST, r"^/hello/([^/]+)/(.+)$", hello)
        .add(http::Method::GET, r"^/add/([\d]+)/([\d]+)$", unreliable_add)
        .finish()
        .unwrap();

    let addr = "0.0.0.0:3000".parse().unwrap();

    let server =
        hyper::Server::bind(&addr).serve(router).map_err(|e| eprintln!("server error: {}", e));

    hyper::rt::run(server);
}
