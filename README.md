# reset-router

A [`RegexSet`](https://doc.rust-lang.org/regex/regex/struct.RegexSet.html) based router for use with Hyper v0.12.

Similar to and inspired by [reroute](https://github.com/gsquire/reroute), but for async Hyper and potentially
faster (no unnecessary string allocations, no hashmaps, and method-first-matching).

Enables request handling for functions that look like `Fn(Context) -> RESPONSE`
where `Context` is a wrapper around `http::Request` and the matching regex, and

```rust,ignore
RESPONSE: IntoFuture<Item = S, Error = E>,
    S: Into<http::Response>,
    E: Into<http::Response>
```

This means you can return something as simple as `Ok(Response::new())`. You don't have to worry about futures
unless you need to read the request body or interact with other future-aware things.

Use like:

```rust,ignore
let router = ::reset_router::Router::build()
    .add(Method::GET, r"\A/\z", |_| Ok::<Response, Response>(
        http::Response::builder().status(200).body("ROOT path".into()).unwrap()))
    .finish()
    .unwrap();

let addr = "0.0.0.0:3000".parse().unwrap();

let server =
    hyper::Server::bind(&addr).serve(router).map_err(|e| eprintln!("server error: {}", e));

hyper::rt::run(server);
```

See [simple.rs](https://github.com/kardeiz/reset-router/blob/master/examples/simple.rs) for an example.

License: MIT