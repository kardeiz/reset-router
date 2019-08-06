[![Docs](https://docs.rs/reset-router/badge.svg)](https://docs.rs/crate/reset-router/)

# reset-router

A fast [`RegexSet`](https://doc.rust-lang.org/regex/regex/struct.RegexSet.html) based router for use with async Hyper (0.12).

Provides optional attribute based routing with `proc_macro_attribute`s. Enable the `with-macros` feature flag to use this feature.

Individual handler functions should have the type `H`, where
```rust
    H: Fn(http::Request<Body>) -> I + Sync + Send + 'static,
    I: IntoFuture<Item = S, Error = E>,
    I::Future: 'static + Send,
    S: Into<http::Response<Body>>,
    E: Into<http::Response<Body>>,
```

You can return something as simple as `Ok(Response::new())`. You don't have to worry about futures
unless you need to read the request body or interact with other future-aware things.

### Usage:

```rust
extern crate futures;
extern crate http;
extern crate hyper;

extern crate reset_router;

use futures::{Future};

use reset_router::{bits::Method, Request, Response, Router, RequestExtensions};

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

    let state_num: i32 = req.state::<State>().map(|x| x.0 ).unwrap_or(0);

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
```

Or with attribute based routing:

```rust
extern crate futures;
extern crate http;
extern crate hyper;

#[macro_use]
extern crate reset_router;

use futures::Future;

use reset_router::Router;

#[derive(Clone, Debug)]
pub struct State {
    pub goodbye: String,
}

pub mod handlers {

    use super::State;
    use reset_router::{Request, Response, RequestExtensions};

    #[route(path = "^/goodbye$", methods = "GET, POST")]
    pub fn goodbye(req: Request) -> Result<Response, Response> {
        let state = req.state::<State>().unwrap();
        Ok(http::Response::builder().status(200).body(state.goodbye.clone().into()).unwrap())
    }

    #[get(r"^/hello/([^/]+)/(\d+)$")]
    pub fn hello(req: Request) -> Result<Response, Response> {
        let (name, age) = req.parsed_captures::<(String, u8)>()?;
        Ok(http::Response::builder()
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
        hyper::Server::bind(&addr).serve(router).map_err(|e| eprintln!("server error: {}", e));

    hyper::rt::run(server);
}
```

Current version: 0.6.5

License: MIT
