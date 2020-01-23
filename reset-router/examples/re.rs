use reset_router::{Request, RequestExtensions, Response, Router};

async fn digit_handler(req: Request) -> Result<Response, Response> {
    // We know there are captures because that is the only way this function is triggered.
    let caps = req.captures().unwrap();
    let digits = &caps.get(1).unwrap();
    if digits.len() > 5 {
        Ok(Response::new(hyper::Body::from("that's a big number!")))
    } else {
        Ok(Response::new(hyper::Body::from("not a big number")))
    }
}

// You can ignore captures if you don't want to use them.
async fn body_handler(req: Request) -> Result<Response, Response> {
    Ok(Response::new(req.into_body()))
}

// A custom 404 handler.
async fn not_found(req: Request) -> Result<Response, Response> {
    let message = format!("why you calling {}?", req.uri());
    Ok(Response::new(hyper::Body::from(message)))
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let router = Router::build()
        .add(http::Method::GET, r"/(\d+)", digit_handler)
        .add(http::Method::POST, r"/body", body_handler)
        .add(http::Method::DELETE, r"/closure", |_| {
            async {
                Ok::<_, Response>(Response::new(hyper::Body::from(
                    "You used a closure here, and called a delete. How neat.",
                )))
            }
        })
        .add_not_found(not_found)
        .finish()?;

    let addr = ([127, 0, 0, 1], 3000).into();

    let server = hyper::Server::bind(&addr).serve(router);

    server.await?;

    Ok(())
}
