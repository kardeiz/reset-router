extern crate failure;
extern crate futures;
extern crate http;
extern crate hyper;

#[macro_use]
extern crate reset_router;

use hyper::rt::Future;

use reset_router::Router;

#[derive(Clone, Debug)]
pub struct State {
    pub goodbye: String
}

pub mod handlers {

    pub type Request = http::Request<hyper::Body>;
    pub type Response = http::Response<hyper::Body>;
    pub type Result<T> = std::result::Result<T, Response>;

    use super::State;
    use reset_router::RequestExtensions;

    #[route(path = "^/goodbye$", methods = "GET, POST")]
    pub fn goodbye(req: Request) -> Result<Response> {
        let state = req.state::<State>().unwrap();
        Ok(http::Response::builder().status(200).body(state.goodbye.clone().into()).unwrap())
    }

    #[get(r"^/hello/([^/]+)/(\d+)$")]
    pub fn hello(req: Request) -> Result<Response> {
        let (name, age) = req.parsed_captures::<(String, u8)>()?;
        Ok(::http::Response::builder()
            .status(200)
            .body(format!("Hello, {} year old named {}!", age, name).into())
            .unwrap())
    }
}

fn main() {
    let router = Router::build()
        .with_state(State { goodbye: "Goodbye".into() })
        .add_routes(routes![handlers::hello, handlers::goodbye])
        .finish()
        .unwrap();

    let addr = "0.0.0.0:3000".parse().unwrap();

    let server =
        ::hyper::Server::bind(&addr).serve(router).map_err(|e| eprintln!("server error: {}", e));

    ::hyper::rt::run(server);
}
