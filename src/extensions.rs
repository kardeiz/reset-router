use err::{Error, ErrorKind};
use {Request, Response};

use std::path::Path;
use std::str::FromStr;

use tokio_core::reactor::Handle;
use hyper::header;

pub mod utils {

    use std::fs::File;
    use bytes::Bytes;
    use hyper::Chunk;

    const BUF_SIZE: usize = 1024 * 128;

    pub struct FileIter { file: File, buf: [u8; BUF_SIZE] }

    impl ::std::iter::Iterator for FileIter {
        type Item = Result<Chunk, ()>;

        fn next(&mut self) -> Option<Self::Item> {
            use std::io::Read;
            
            match self.file.read(&mut self.buf) {
                Err(_) => None,
                Ok(0) => None,
                Ok(result) => Some(Ok(Chunk::from(Bytes::from(&self.buf[0..result])))),
            }
        }
    }

    impl FileIter {
        pub fn new(file: File) -> Self {
            FileIter { file: file, buf: [0; BUF_SIZE] }
        }
    }

}

pub trait ResponseExtensions {
    fn set_path<T: AsRef<Path>>(&mut self, hdl: &Handle, path: T) -> Result<&mut Self, Error>;
    fn with_path<T: AsRef<::std::path::Path>>(self, hdl: &Handle, path: T) -> Result<Self, Error> where Self: Sized;
}

impl ResponseExtensions for Response {

    fn set_path<T: AsRef<Path>>(&mut self, hdl: &Handle, path: T) -> Result<&mut Self, Error> where Self: Sized {
        
        use futures::Sink;
        use futures::Stream;
        use futures::Future;
        use std::fs::File;

        lazy_static! {
            pub static ref MIME_TYPES: ::conduit_mime_types::Types = 
                ::conduit_mime_types::Types::new().unwrap();
        }
        
        let path_as_ref = path.as_ref();

        let md = path_as_ref.metadata()?;

        let mime = MIME_TYPES
            .mime_for_path(path_as_ref)
            .parse::<::hyper::mime::Mime>()
            .map_err(|_| "Unknown MIME type")?;

        let stream = ::futures::stream::iter(utils::FileIter::new(File::open(path_as_ref)?))
            .map(|x| Ok(x))
            .map_err(|_| ());

        let (tx, rx) = ::hyper::Body::pair();

        hdl.spawn(tx.sink_map_err(|_| ()).send_all(stream).map(|_| ()));

        self.headers_mut().set(header::ContentLength(md.len() as u64));
        self.headers_mut().set(header::ContentType(mime));

        self.set_body(rx);

        Ok(self)
    }

    fn with_path<T: AsRef<::std::path::Path>>(mut self, hdl: &Handle, path: T) -> Result<Self, Error> {
        self.set_path(hdl, path)?;
        Ok(self)
    }

}


pub trait RequestExtensions {
    fn extract_captures<T: CaptureExtraction>(&self) -> Result<T, Error>;
}

impl<'a> RequestExtensions for Request<'a> {
    fn extract_captures<T: CaptureExtraction>(&self) -> Result<T, Error> {
        Ok(T::extract_captures(self)?)
    }
}

pub trait CaptureExtraction: Sized {
    fn extract_captures(req: &Request) -> Result<Self, Error>;
}

impl<T> CaptureExtraction for (T,) where T: FromStr {
    fn extract_captures(req: &Request) -> Result<Self, Error> {
        let caps = req.captures().ok_or(ErrorKind::CapturesError)?;
        let out_1 = caps.get(1).map(|x| x.as_str() ).and_then(|x| x.parse().ok())
            .ok_or(ErrorKind::CapturesError)?;
        Ok((out_1,))
    }
}

impl<T1, T2> CaptureExtraction for (T1, T2) where T1: FromStr, T2: FromStr {
    fn extract_captures(req: &Request) -> Result<Self, Error> {
        let caps = req.captures().ok_or(ErrorKind::CapturesError)?;
        let out_1 = caps.get(1).map(|x| x.as_str() ).and_then(|x| x.parse().ok())
            .ok_or(ErrorKind::CapturesError)?;
        let out_2 = caps.get(2).map(|x| x.as_str() ).and_then(|x| x.parse().ok())
            .ok_or(ErrorKind::CapturesError)?;
        Ok((out_1, out_2))
    }
}

impl<T1, T2, T3> CaptureExtraction for (T1, T2, T3) where T1: FromStr, T2: FromStr, T3: FromStr {
    fn extract_captures(req: &Request) -> Result<Self, Error> {
        let caps = req.captures().ok_or(ErrorKind::CapturesError)?;
        let out_1 = caps.get(1).map(|x| x.as_str() ).and_then(|x| x.parse().ok())
            .ok_or(ErrorKind::CapturesError)?;
        let out_2 = caps.get(2).map(|x| x.as_str() ).and_then(|x| x.parse().ok())
            .ok_or(ErrorKind::CapturesError)?;
        let out_3 = caps.get(3).map(|x| x.as_str() ).and_then(|x| x.parse().ok())
            .ok_or(ErrorKind::CapturesError)?;
        Ok((out_1, out_2, out_3))
    }
}


