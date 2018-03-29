extern crate http;
extern crate reset_router;
extern crate hyper;

use std::sync::Arc;

type Response = ::http::Response<::hyper::Body>;


fn not_found(_: ::reset_router::Context) -> Result<Response, Response> {
    Ok(::http::Response::builder()
        .status(200)
        .body("Not found".into())
        .unwrap())
}

fn main() {
    let router = ::reset_router::Router::build()
        .add_not_found(not_found)
        .finish()
        .unwrap();

    let router = Arc::new(router);

    let addr = "0.0.0.0:3000".parse().unwrap();
    let server = ::hyper::server::Http::new().bind_compat(&addr, move || Ok(router.clone())).unwrap();
    server.run().unwrap();
}