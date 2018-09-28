extern crate http;
extern crate hyper;
extern crate reset_router;
extern crate futures;

use std::sync::Arc;
use futures::future::IntoFuture;
use hyper::service::Service;
use hyper::rt::Future;

fn hello(_: ::reset_router::Context) -> Result<::reset_router::Response, ::reset_router::Response> {
    Ok(::http::Response::builder()
        .status(200)
        .body("hello".into())
        .unwrap())
}

fn main() {
    let router = ::reset_router::Router::build()
        .add(::http::Method::GET, r"\A/hello", hello)
        .finish()
        .unwrap();

    let addr = "0.0.0.0:3000".parse().unwrap();

    let server = ::hyper::Server::bind(&addr)
        .serve(router)
        .map_err(|e| eprintln!("server error: {}", e));

    ::hyper::rt::run(server);
}
