use reset_router::{Request, RequestExtensions, Response, Router, SharedService};
use std::sync::Arc;

pub struct Handler(Arc<String>);

impl SharedService for Handler {
    type Response = Response;
    type Error = Box<dyn std::error::Error + Send + Sync + 'static>;
    type Future = futures::future::Ready<Result<Self::Response, Self::Error>>;

    fn call(&self, request: Request) -> Self::Future {
        let inner = Arc::clone(&self.0);
        futures::future::ready(Ok(http::Response::builder()
            .status(200)
            .body(format!("Hello, {}!", inner).into())
            .unwrap()))
    }
}

#[derive(Clone)]
pub struct State(pub i32);

async fn hello(req: Request) -> Result<Response, Response> {
    let (first_name, last_name) = req.parsed_captures::<(String, String)>()?;
    Ok(http::Response::builder()
        .status(200)
        .body(format!("Hello, {} {}!", first_name, last_name).into())
        .unwrap())
}

async fn add(req: Request) -> Result<Response, Response> {
    let (add1, add2) = req.parsed_captures::<(i32, i32)>()?;

    let state_num: i32 = req.data::<State>().map(|x| x.0).unwrap_or(0);

    Ok(http::Response::builder()
        .status(200)
        .body(
            format!("{} + {} + {} = {}\r\n", add1, add2, state_num, add1 + add2 + state_num).into(),
        )
        .unwrap())
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    let router = Router::build()
        .data(State(42))
        .add(http::Method::POST, r"^/hello/([^/]+)/(.+)$", hello)
        .add(http::Method::GET, r"^/hello/([^/]+)/(.+)$", hello)
        .add(http::Method::GET, r"^/add/([\d]+)/([\d]+)$", add)
        .add(http::Method::GET, r"^/other$", Handler(Arc::new(String::from("world"))))
        .add_not_found(|_| {
            async {
                Ok::<_, Response>(http::Response::builder().status(404).body("404".into()).unwrap())
            }
        })
        .finish()?;

    let addr = "0.0.0.0:3000".parse()?;

    let server = hyper::Server::bind(&addr).serve(router);

    server.await?;

    Ok(())
}
