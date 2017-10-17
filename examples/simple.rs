extern crate futures;
extern crate hyper;
extern crate reset_router;
extern crate num_cpus;

#[macro_use]
extern crate error_chain;

extern crate futures_fs;
extern crate conduit_mime_types;

extern crate tokio_core;
extern crate net2;
extern crate serde_json;
extern crate http;

use reset_router::{Context, Router};
use reset_router::hyper::HyperContext;

use futures::Stream;
use futures::{BoxFuture, Future};
#[macro_use]
extern crate lazy_static;

use hyper::server::Http;
use hyper::Request;
use hyper::server::Response;

use http::Method;

use hyper::header::{ContentLength, ContentType};

use std::sync::Arc;

fn other(ctx: HyperContext) -> Result<Response, Response> {
    println!("{:?}", &ctx.path());
    let msg = "OTHER";
    let response = Response::new()
        .with_status(::hyper::StatusCode::Ok)
        .with_header(ContentLength(msg.len() as u64))
        .with_header(ContentType::plaintext())
        .with_body(msg);
    Ok(response)
}

fn main() {
    let addr = "0.0.0.0:3000".parse().unwrap();    

    let router = Router::build()
        .add(Method::GET, r"\A/other\z", other)
        .finish()
        .unwrap();

    let wrapped = Arc::new(router);

    let server = Http::new().bind(&addr, move || Ok(wrapped.clone())).unwrap();

    server.run().unwrap();
}


// pub mod err {
//     error_chain! {
//         errors { }

//         foreign_links {
//             Io(::std::io::Error);
//             Hyper(::hyper::Error);
//             SerdeJson(::serde_json::Error);
//         }
//     }

//     impl ::reset_router::IntoResponse for Error {
//         fn into_response(self) -> ::reset_router::Response {
//             use hyper::header::{ContentLength, ContentType};
//             let msg = format!("{}", &self);
//             ::reset_router::Response::new()
//                 .with_status(::hyper::StatusCode::InternalServerError)
//                 .with_header(ContentLength(msg.len() as u64))
//                 .with_header(ContentType::plaintext())
//                 .with_body(msg)
//         }
//     }

// }

// mod core_handling {
//     use tokio_core::reactor::{Core, Handle};
//     use std::cell::RefCell;

//     pub struct CoreHandler(RefCell<Option<Core>>, Handle);

//     thread_local!(static CORE_HANDLER: CoreHandler = {
//         let core = Core::new().unwrap();
//         let handle = core.handle();
//         CoreHandler(RefCell::new(Some(core)), handle)
//     });

//     pub fn local_core_take() -> Option<Core> {
//         CORE_HANDLER.with(|o| o.0.borrow_mut().take())
//     }

//     pub fn with_local_handle<T, F: FnOnce(&Handle) -> T>(cls: F) -> T {
//         CORE_HANDLER.with(|o| cls(&o.1))
//     }
// }

// mod utils {
//     pub fn mime_for<P: AsRef<::std::path::Path>>(path: P) -> ::hyper::mime::Mime {
//         lazy_static! {
//             pub static ref MIME_TYPES: ::conduit_mime_types::Types =
//                 ::conduit_mime_types::Types::new().unwrap();
//         }

//         let path_as_ref = path.as_ref();

//         MIME_TYPES
//             .mime_for_path(path_as_ref)
//             .parse::<::hyper::mime::Mime>()
//             .expect("Unknown MIME type")
//     }
// }

// use reset_router::ext::*;

// type BoxedResponse = BoxFuture<Response, err::Error>;

// fn not_found(_: Request) -> BoxedResponse {
//     let msg = "NOT FOUND";
//     ::futures::future::ok(
//         Response::new()
//             .with_status(::hyper::StatusCode::NotFound)
//             .with_header(ContentLength(msg.len() as u64))
//             .with_header(ContentType::plaintext())
//             .with_body(msg),
//     ).boxed()
// }



// fn json(_: Request) -> err::Result<Response> {
//     Ok(Response::new().with_sized_body("[1,2,3]"))
// }


// fn post_body(req: Request) -> BoxedResponse {

//     let body = req.into_inner().body();

//     body.concat2()
//         .from_err()
//         .map(|bytes| {
//             Response::new()
//                 .with_status(::hyper::StatusCode::Ok)
//                 .with_header(ContentLength(bytes.len() as u64))
//                 .with_header(ContentType::plaintext())
//                 .with_body(bytes)
//         })
//         .boxed()

// }

// fn assets(req: Request) -> BoxedResponse {

//     lazy_static! {
//         static ref FS_POOL: ::futures_fs::FsPool = ::futures_fs::FsPool::new(4);
//     }

//     // Don't do this with a public webserver
//     // You will potentially expose any file on your server to user requests

//     let path = {
//         let (p,): (String,) = req.extract_captures().unwrap();
//         ::std::path::Path::new(&::std::env::var("ASSETS_BASE").unwrap()).join(&p)
//     };

//     let mime = utils::mime_for(&path);

//     FS_POOL
//         .read(path)
//         .concat2()
//         .from_err()
//         .map(|bytes| {
//             Response::new()
//                 .with_status(::hyper::StatusCode::Ok)
//                 .with_header(ContentLength(bytes.len() as u64))
//                 .with_header(ContentType(mime))
//                 .with_body(bytes)
//         })
//         .boxed()

// }

// struct Server(Router);

// impl Service for Server {
//     type Request = <Router as Service>::Request;
//     type Response = <Router as Service>::Response;
//     type Error = <Router as Service>::Error;
//     type Future = <Router as Service>::Future;

//     fn call(&self, req: Self::Request) -> Self::Future {
//         println!("{:?}", &req.path());
//         self.0.call(req)
//     }
// }


// fn main() {
//     let addr = "0.0.0.0:3000".parse().unwrap();

//     let router = Router::build()
//         .add(Method::Get, r"\A/assets/(.+)\z", assets)
//         .add(Method::Get, r"\A/json/*\z", json)
//         .add(Method::Get, r"\A/other\z", other)
//         .add(Method::Post, r"\A/post_body\z", post_body)
//         .add_not_found(not_found)
//         .finish()
//         .unwrap();

//     Server(router).quick_serve(num_cpus::get() * 8, addr, || {
//         core_handling::local_core_take().unwrap()
//     });
// }
