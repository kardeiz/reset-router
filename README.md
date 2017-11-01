# reset-router

A [`RegexSet`](https://doc.rust-lang.org/regex/regex/struct.RegexSet.html) based router for use with Rust web servers 
(currently only supports Hyper 0.11.6).

Use like:

```rust,ignore
let router = Router::build()
    .add_get(r"\A/\z", |_| Ok::<_, Response>(Response::new().with_body("ROOT path")))
    .add_not_found(|_| Ok::<_, Response>(Response::new().with_body("Route not found")))
    .finish()
    .unwrap();
router.quick_serve(8, "0.0.0.0:3000".parse().unwrap(), || Core::new().unwrap() );
```

See [simple.rs](https://github.com/kardeiz/reset-router/blob/master/examples/simple.rs) for how to use the router with Hyper.

Works well with [`futures-await`](https://github.com/alexcrichton/futures-await) for a nicer async experience.

License: MIT


## Casual changelog

### v0.3.0 (2017-11-01)

* Changed the request wrapper to `Context`