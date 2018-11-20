# reset-router

A [`RegexSet`](https://doc.rust-lang.org/regex/regex/struct.RegexSet.html) based router for use with async Hyper.

Provides optional attribute based routing with Rust 1.30's proc_macro_attributes.

Your handler functions should look like:

```rust
Fn(http::Request<hyper::Body>) -> I
where
    I: IntoFuture<Item = S, Error = E>,
    I::Future: 'static + Send,
    S: Into<http::Response<hyper::Body>>,
    E: Into<http::Response<hyper::Body>>,
```

Pretty straightforward! You can return something as simple as `Ok(Response::new())`. You don't have to worry about futures
unless you need to read the request body or interact with other future-aware things.

## Usage:

```rust

use reset_router::RequestExtensions;

// Example handler from Rocket
#[get(r"^/hello/([^/]+)/(\d+)$")]
pub fn hello(req: Request) -> Result<Response> {    
    let (name, age) = req.parsed_captures::<(String, u8)>()?;
    Ok(::http::Response::builder()
        .status(200)
        .body(format!("Hello, {} year old named {}!", age, name).into())
        .unwrap())
}

fn main() {
    let router = reset_router::Router::build()
        .add_routes(routes![hello])
        .finish()
        .unwrap();

    let addr = "0.0.0.0:3000".parse().unwrap();

    let server =
        hyper::Server::bind(&addr).serve(router).map_err(|e| eprintln!("server error: {}", e));

    hyper::rt::run(server);
}
```

`RequestExtensions` provides easy access to your route regex captures, as well as access to an optional `State` object. See [simple.rs](https://github.com/kardeiz/reset-router/blob/master/examples/simple.rs) for an example.

If you prefer to keep all your path regexes in one place, of if you want to use closures, you can still use the old style:

```rust

// Use this custom bitflags instead of http::Method for easy `BitOr` style method combinations
use reset_router::bits::Method;

let router = reset_router::Router::build()
    .add(Method::GET | method::POST, r"^/hello/([^/]+)/(\d+)$", hello)
    .finish()
    .unwrap();
```

License: MIT