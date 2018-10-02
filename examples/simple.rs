extern crate failure;
extern crate futures;
extern crate http;
extern crate hyper;
extern crate reset_router;

use hyper::rt::Future;

pub mod err {

    pub struct Error(::failure::Error);

    impl Into<::reset_router::Response> for Error {
        fn into(self) -> ::reset_router::Response {
            ::http::Response::builder().status(500).body(self.0.to_string().into()).unwrap()
        }
    }

    impl<F: ::failure::Fail> From<F> for Error {
        fn from(f: F) -> Error { Error(f.into()) }
    }

    pub type Result<T> = ::std::result::Result<T, Error>;
}

#[derive(Clone)]
pub struct State {
    pub greetings: String
}

pub type Context = ::reset_router::Context<State>;

fn hello(ctx: Context) -> ::err::Result<::reset_router::Response> {
    let greetings = &ctx.state.greetings;
    let (name,) = ctx.parsed_captures::<(String,)>()?;
    Ok(::http::Response::builder()
        .status(200)
        .body(format!("{}, {}", greetings, name).into())
        .unwrap())
}

fn main() {
    let router = ::reset_router::Router::build()
        .with_state(State { greetings: "Greetings".into() })
        .add(::http::Method::GET, r"\A/hello/(.+)\z", hello)
        .finish()
        .unwrap();

    let addr = "0.0.0.0:3000".parse().unwrap();

    let server =
        ::hyper::Server::bind(&addr).serve(router).map_err(|e| eprintln!("server error: {}", e));

    ::hyper::rt::run(server);
}
