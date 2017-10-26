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
extern crate serde_json;

use reset_router::hyper::{HyperContext as Context, HyperRouter as Router, IntoResponse};

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
            SerdeJson(::serde_json::Error);
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

mod core_handling {
    use tokio_core::reactor::{Core, Handle};
    use std::cell::RefCell;

    pub struct CoreHandler(RefCell<Option<Core>>, Handle);

    thread_local!(static CORE_HANDLER: CoreHandler = {
        let core = Core::new().unwrap();
        let handle = core.handle();
        CoreHandler(RefCell::new(Some(core)), handle)
    });

    pub fn local_core_take() -> Option<Core> {
        CORE_HANDLER.with(|o| o.0.borrow_mut().take())
    }

    pub fn with_local_handle<T, F: FnOnce(&Handle) -> T>(cls: F) -> T {
        CORE_HANDLER.with(|o| cls(&o.1))
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
    ::futures::future::ok(
        Response::new()
            .with_status(::hyper::StatusCode::NotFound)
            .with_header(ContentLength(msg.len() as u64))
            .with_header(ContentType::plaintext())
            .with_body(msg),
    ).boxed()
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

    FS_POOL
        .read(path)
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

    use reset_router::hyper::ext::ServiceExtensions;

    let addr = "0.0.0.0:3000".parse().unwrap();

    let router = Router::build()
        .add_get(r"\A/assets/(.+)\z", assets)
        .add_get(r"\A/other\z", other)
        .add_post(r"\A/post_body\z", post_body)
        .finish()
        .unwrap();

    router.quick_serve(num_cpus::get(), addr, || {
        core_handling::local_core_take().unwrap()
    });
}