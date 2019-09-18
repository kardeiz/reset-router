# reset-recognizer

[![Docs](https://docs.rs/reset-recognizer/badge.svg)](https://docs.rs/crate/reset-recognizer/)
[![Crates.io](https://img.shields.io/crates/v/reset-recognizer.svg)](https://crates.io/crates/reset-recognizer)

A fast [`RegexSet`](https://doc.rust-lang.org/regex/regex/struct.RegexSet.html) based path router, in the style of
[route-recognizer](https://docs.rs/route-recognizer).

[reset-router](https://docs.rs/reset-router), a higher level path router for use with Hyper 0.12, uses this library internally.

### Usage:

```rust
let mut router = reset_recognizer::Router::build()
    .add(r"^/posts/(.+)/comments/(.+)$", "comment".to_string())
    .add(r"^/posts/(.+)/comments$", "comments".to_string())
    .add(r"^/posts/(.+)$", "post".to_string())
    .add(r"^/posts$", "posts".to_string())
    .add(r"^/comments$", "comments2".to_string())
    .add(r"^/comments/(.+)$", "comment2".to_string())
    .add_with_priority(r"^/(.+)$", 1, "not_found".to_string())
    .finish()?;

let matched = router.recognize("/posts/100/comments/200")?;

let (post_id, comment_id) = matched.captures.parsed::<(i32, i32)>()?;

println!("{:?}", &matched.handler, &post_id, &comment_id);

```

Current version: 0.7.1

License: MIT
