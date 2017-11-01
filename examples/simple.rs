extern crate futures;
extern crate hyper;
extern crate reset_router;
extern crate num_cpus;

extern crate http;

#[macro_use]
extern crate error_chain;

extern crate futures_fs;
extern crate conduit_mime_types;

extern crate tokio_core;
extern crate net2;

use reset_router::hyper::{Context, Router};

use futures::Stream;
use futures::Future;

#[macro_use]
extern crate lazy_static;

use hyper::header::{ContentLength, ContentType};
use hyper::Response;

pub mod err {

    error_chain! {
        errors { }

        foreign_links {
            Io(::std::io::Error);
            Hyper(::hyper::Error);
        }
    }

    impl ::reset_router::hyper::IntoResponse for Error {
        fn into_response(self) -> ::hyper::Response {
            use hyper::header::{ContentLength, ContentType};
            let msg = format!("{}", &self);
            ::hyper::Response::new()
                .with_status(::hyper::StatusCode::InternalServerError)
                .with_header(ContentLength(msg.len() as u64))
                .with_header(ContentType::plaintext())
                .with_body(msg)
        }
    }

}

mod utils {
    pub fn mime_for<P: AsRef<::std::path::Path>>(path: P) -> ::hyper::mime::Mime {
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
}

type BoxFuture<I, E> = Box<Future<Item = I, Error = E>>;
type BoxedResponse = BoxFuture<hyper::Response, err::Error>;

fn not_found(_: Context) -> BoxedResponse {
    let msg = "NOT FOUND";
    Box::new(::futures::future::ok(
        Response::new()
            .with_status(::hyper::StatusCode::NotFound)
            .with_header(ContentLength(msg.len() as u64))
            .with_header(ContentType::plaintext())
            .with_body(msg),
    ))
}

fn other(_: Context) -> err::Result<Response> {
    let msg = "OTHER";
    let response = Response::new()
        .with_status(::hyper::StatusCode::Ok)
        .with_header(ContentLength(msg.len() as u64))
        .with_header(ContentType::plaintext())
        .with_body(msg);
    Ok(response)
}

fn post_body(ctx: Context) -> BoxedResponse {

    let body = ctx.into_request().body();

    Box::new(body.concat2().from_err().map(|bytes| {
        Response::new()
            .with_status(::hyper::StatusCode::Ok)
            .with_header(ContentLength(bytes.len() as u64))
            .with_header(ContentType::plaintext())
            .with_body(bytes)
    }))
}

fn assets(ctx: Context) -> BoxedResponse {

    lazy_static! {
        static ref FS_POOL: ::futures_fs::FsPool = ::futures_fs::FsPool::new(4);
    }

    // Don't do this with a public webserver
    // You will potentially expose any file on your server to user requests

    let path = {
        let (p,): (String,) = ctx.extract_captures().unwrap();
        ::std::path::Path::new(&::std::env::var("ASSETS_BASE").unwrap()).join(&p)
    };

    let mime = utils::mime_for(&path);

    Box::new(FS_POOL.read(path).concat2().from_err().map(|bytes| {
        Response::new()
            .with_status(::hyper::StatusCode::Ok)
            .with_header(ContentLength(bytes.len() as u64))
            .with_header(ContentType(mime))
            .with_body(bytes)
    }))

}



fn main() {

    use reset_router::hyper::ext::ServiceExtensions;

    let addr = "0.0.0.0:3000".parse().unwrap();

    let router = Router::build()
        .add_get(r"\A/assets/(.+)\z", assets)
        .add_get(r"\A/other\z", other)
        .add_post(r"\A/post_body\z", post_body)
        .add_not_found(not_found)
        .finish()
        .unwrap();

    router
        .quick_serve(num_cpus::get(), addr, || {
            ::tokio_core::reactor::Core::new().unwrap()
        })
        .unwrap();
}
