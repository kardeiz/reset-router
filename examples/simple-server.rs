extern crate reset_router;
extern crate simple_server;
extern crate http;

#[macro_use] 
extern crate lazy_static;

use simple_server::Server;
use reset_router::{Context, Router};
use reset_router::simple_server::*;
use http::Method;

fn other<'a>(ctx: SimpleServerContext<'a>, mut res: ::http::response::Builder) -> SimpleServerResponse<'a> {
    Ok(res.body("Hello Rust!".as_bytes())?)
}

fn main() {

    let host = "0.0.0.0";
    let port = "3000";

    let server = Server::new(|request, mut response| {
        let router = Router::build()
            .add(Method::GET, r"\A/other\z", other)
            .finish()
            .unwrap();
        router.handle(request, response).unwrap()
    });

    server.listen(host, port);
}