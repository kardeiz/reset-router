extern crate futures;
extern crate hyper;
extern crate reset_router;
extern crate num_cpus;

extern crate futures_fs;
extern crate conduit_mime_types;

extern crate tokio_core;
extern crate net2;

use reset_router::{Router, Request, Response, IntoResponse};

use futures::Stream;
use futures::{Future, BoxFuture, IntoFuture};

use std::error::Error;
use std::net::SocketAddr;

#[macro_use]
extern crate lazy_static;

use hyper::header::{ContentType, ContentLength};
use tokio_core::reactor::{Core, Handle};

use std::cell::RefCell;

pub struct CoreHandler(RefCell<Option<Core>>, Handle);

thread_local!(static CORE_HANDLER: CoreHandler = {
    let core = Core::new().unwrap();
    let handle = core.handle();
    CoreHandler(RefCell::new(Some(core)), handle)
});

pub fn local_core_take() -> Option<Core> {
    CORE_HANDLER.with(|o| o.0.borrow_mut().take() )
}

pub fn with_local_handle<T, F: FnOnce(&Handle) -> T>(cls: F) -> T {
    CORE_HANDLER.with(|o| cls(&o.1))
}


pub fn quick_serve(router: Router, num_threads: usize, addr: SocketAddr) {
    use std::sync::Arc;
    use net2::unix::UnixTcpBuilderExt;
    use futures::Stream;
    use hyper::server::Http;

    fn inner(addr: &SocketAddr, protocol: Arc<Http>, router: Arc<Router>) {

        let mut core = local_core_take().unwrap();
        let hdl = core.handle();
        let listener = ::net2::TcpBuilder::new_v4()
            .unwrap()
            .reuse_port(true)
            .unwrap()
            .bind(addr)
            .unwrap()
            .listen(128)
            .unwrap();
        let listener =
            ::tokio_core::net::TcpListener::from_listener(listener, addr, &hdl).unwrap();
        core.run(listener.incoming().for_each(|(socket, addr)| {
            protocol.bind_connection(&hdl, socket, addr, router.clone());
            Ok(())
        })).unwrap();
    }

    let protocol = Arc::new(Http::new());
    let router = Arc::new(router);

    for _ in 0..(num_threads - 1) {
        let protocol_c = protocol.clone();
        let router_c = router.clone();
        ::std::thread::spawn(move || inner(&addr, protocol_c, router_c));
    }

    inner(&addr, protocol, router);

}

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

impl IntoResponse for LocalError {
    fn into_response(self) -> Response {
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

fn other(_: Request) -> Result<Response, LocalError> {
    let msg = "OTHER";
    let response = Response::new()
        .with_status(::hyper::StatusCode::Ok)
        .with_header(ContentLength(msg.len() as u64))
        .with_header(ContentType::plaintext())
        .with_body(msg);
    Ok(response)
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

    quick_serve(router, num_cpus::get(), addr);

}
