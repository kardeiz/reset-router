# reset-router

A [RegexSet](https://doc.rust-lang.org/regex/regex/struct.RegexSet.html) based router for use with Hyper v0.11.x.

Similar to and inspired by [reroute](https://github.com/gsquire/reroute), but for async Hyper and potentially
faster (no unnecessary string allocations, no hashmaps, and method-first-matching).

Enables request handling for functions that look like `Fn(Request) -> FUTURE`
where Request is a thin wrapper around `hyper::server::Request` and

```rust,ignore
FUTURE: IntoFuture<Future = F, Item = S, Error = E>,
    F: Future<Item = S, Error = E> + 'static + Send,
    S: IntoResponse,
    E: IntoResponse
```

This means you can return something as simple as `Ok(Response::new())`. You don't have to worry about futures
unless you need to read the request body or interact with other future-aware things.

Use like:

```rust,ignore
let router = Router::build()
    .add_get(r"\A/\z", |_| Ok::<_, Response>(Response::new().with_body("ROOT path")))
    .add_not_found(|_| Ok::<_, Response>(Response::new().with_body("Route not found")))
    .finish()
    .unwrap();
router.quick_serve(8, "0.0.0.0:3000".parse().unwrap(), || Core::new().unwrap() );
```

See [simple.rs](https://github.com/kardeiz/reset-router/blob/master/examples/simple.rs) for examples.



License: MIT
