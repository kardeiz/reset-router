extern crate futures;
extern crate hyper;
extern crate reset_router;
extern crate num_cpus;

#[macro_use]
extern crate lazy_static;

extern crate tokio_core;
extern crate net2;

use reset_router::{Router, Request};

use futures::future::FutureResult;
use futures::Stream;
use futures::Future;

use hyper::{Get, Post, StatusCode};
use hyper::header::{ContentType,ContentLength};
use hyper::server::{Http, Service, Response};
use hyper::server::Server;

use hyper::{Body, Chunk};

use tokio_core::net::TcpListener;
use tokio_core::reactor::Core;
use tokio_core::reactor::Remote;

use net2::unix::UnixTcpBuilderExt;

use std::sync::Arc;

use std::convert::Into;
use std::cell::RefCell;

use reset_router::extensions::ResponseExtensions;

type BoxedResponse = Box<::futures::Future<Item=Response, Error=::hyper::Error>>;

pub struct LocalError {}

impl Into<BoxedResponse> for LocalError {
    fn into(self) -> BoxedResponse {
        let msg = "Internal Error";
        ::futures::future::ok(Response::new()
            .with_header(ContentLength(msg.len() as u64))
            .with_header(ContentType::plaintext())
            .with_body(msg)).boxed()
    }
}

fn not_found(_: Request) -> Result<BoxedResponse, LocalError> {
    let msg = "NOT FOUND";
    Ok(::futures::future::ok(Response::new()
        .with_status(::hyper::StatusCode::NotFound)
        .with_header(ContentLength(msg.len() as u64))
        .with_header(ContentType::plaintext())
        .with_body(msg)).boxed())
}

fn index(_: Request) -> Result<BoxedResponse, LocalError> {

    let response = coring::with_handle(|hdl| Response::new().with_path(hdl, "gc-nlp-2.json") ).unwrap();

    Ok(::futures::future::ok(response).boxed())
}

fn other(_: Request) -> Result<BoxedResponse, LocalError> {
    let msg = "OTHER";
    Ok(::futures::future::ok(Response::new()
        .with_status(::hyper::StatusCode::Ok)
        .with_header(ContentLength(msg.len() as u64))
        .with_header(ContentType::plaintext())
        .with_body(msg)).boxed())
}


pub mod coring {

    use tokio_core::reactor::Core;
    use tokio_core::reactor::Handle;
    use std::cell::RefCell;

    thread_local!(static CORE: (RefCell<Core>, Handle) = {
        let core = Core::new().unwrap();
        let handle = core.handle();
        (RefCell::new(core), handle)
    });

    pub fn with_core_mut<T, F: FnOnce(&mut Core) -> T>(cls: F) -> Result<T, ::std::cell::BorrowMutError> {
        CORE.with(|core| core.0.try_borrow_mut().map(|mut x| cls(&mut *x)) )
    }

    pub fn with_core<T, F: FnOnce(&Core) -> T>(cls: F) -> Result<T, ::std::cell::BorrowError> {
        CORE.with(|core| core.0.try_borrow().map(|x| cls(&*x)) )
    }

    pub fn with_handle<T, F: FnOnce(&Handle) -> T>(cls: F) -> T {
        CORE.with(|core| cls(&core.1) )
    }

}



fn main() {
    let addr = "0.0.0.0:3000".parse().unwrap();

    let protocol = Arc::new(Http::new());

    let router = Router::build()
        .add_get(r"\A/\z", index)
        .add_get(r"\A/other\z", other)
        .add_not_found(not_found)
        .finish()
        .unwrap();

    let router = Arc::new(router);

    for _ in 0..(num_cpus::get() - 1) {
        let protocol = protocol.clone();
        let router_c = router.clone();
        ::std::thread::spawn(move || serve(&addr, &protocol, router_c));
    }

    serve(&addr, &protocol, router);    
}

fn serve(addr: &::std::net::SocketAddr, protocol: &Http, server: Arc<Router<BoxedResponse>>) {
    coring::with_core_mut(|core| {
        let handle = core.handle();
        let listener = net2::TcpBuilder::new_v4().unwrap()
            .reuse_port(true).unwrap()
            .bind(addr).unwrap()
            .listen(128).unwrap();
        let listener = TcpListener::from_listener(listener, addr, &handle).unwrap();
        core.run(listener.incoming().for_each(|(socket, addr)| {
            protocol.bind_connection(&handle, socket, addr, server.clone());
            Ok(())
        })).unwrap();
    });
}
