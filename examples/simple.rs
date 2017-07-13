extern crate futures;
extern crate hyper;
extern crate reset_router;
extern crate num_cpus;

extern crate futures_fs;
extern crate conduit_mime_types;

use reset_router::{Router, Request, Response};

use futures::Stream;
use futures::{Future, BoxFuture, IntoFuture};

use std::error::Error;

#[macro_use]
extern crate lazy_static;

use hyper::header::{ContentType, ContentLength};

fn mime_for<P: AsRef<::std::path::Path>>(path: P) -> ::hyper::mime::Mime {
    lazy_static! {
        pub static ref MIME_TYPES: ::conduit_mime_types::Types = 
            ::conduit_mime_types::Types::new().unwrap();
    }

    let path_as_ref = path.as_ref();

    MIME_TYPES
        .mime_for_path(path_as_ref)
        .parse::<::hyper::mime::Mime>()
        .expect("Unknown MIME type")
}


type BoxedResponse = BoxFuture<Response, LocalError>;

pub struct LocalError(String);

lazy_static! {
    static ref FS_POOL: ::futures_fs::FsPool = ::futures_fs::FsPool::new(4);
}

impl Into<Response> for LocalError {
    fn into(self) -> Response {
        let msg = self.0;
        Response::new()
            .with_header(ContentLength(msg.len() as u64))
            .with_header(ContentType::plaintext())
            .with_body(msg)
    }
}

impl From<::std::io::Error> for LocalError {
    fn from(e: ::std::io::Error) -> LocalError {
        LocalError(e.description().into())
    }
}

impl From<::hyper::Error> for LocalError {
    fn from(e: ::hyper::Error) -> LocalError {
        LocalError(e.description().into())
    }
}

fn not_found(_: Request) -> BoxedResponse {
    let msg = "NOT FOUND";
    ::futures::future::ok(
        Response::new()
            .with_status(::hyper::StatusCode::NotFound)
            .with_header(ContentLength(msg.len() as u64))
            .with_header(ContentType::plaintext())
            .with_body(msg),
    ).boxed()
}

fn other(_: Request) -> BoxedResponse {
    let msg = "OTHER";
    let response = Response::new()
        .with_status(::hyper::StatusCode::Ok)
        .with_header(ContentLength(msg.len() as u64))
        .with_header(ContentType::plaintext())
        .with_body(msg);
    Ok(response).into_future().boxed()
}


fn post_body(mut req: Request) -> BoxedResponse {

    let body = req.into_inner().body();

    body.concat2()
        .from_err()
        .map(|bytes| {
            Response::new()
                .with_status(::hyper::StatusCode::Ok)
                .with_header(ContentLength(bytes.len() as u64))
                .with_header(ContentType::plaintext())
                .with_body(bytes)
        })
        .boxed()

}

fn assets(req: Request) -> BoxedResponse {

    let (p,): (String,) = req.extract_captures().unwrap();
    let mime = mime_for(&p);

    if p.starts_with("/") || p.contains("..") {
        return futures::future::err(LocalError("Bad path".into())).boxed();
    }

    FS_POOL
        .read(p)
        .concat2()
        .from_err()
        .map(|bytes| {
            Response::new()
                .with_status(::hyper::StatusCode::Ok)
                .with_header(ContentLength(bytes.len() as u64))
                .with_header(ContentType(mime))
                .with_body(bytes)
        })
        .boxed()

}



fn main() {
    let addr = "0.0.0.0:3000".parse().unwrap();

    let router = Router::build()
        .add_get(r"\A/assets/(.+)\z", assets)
        .add_get(r"\A/other\z", other)
        .add_post(r"\A/post_body\z", post_body)
        .add_not_found(not_found)
        .finish()
        .unwrap();

    router.quick_serve(num_cpus::get(), addr);

}
