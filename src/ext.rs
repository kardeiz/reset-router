extern crate cookie;

use self::cookie::{Cookie, CookieJar};

use hyper::header;
use hyper::Response;

pub trait RequestExtensions {
    fn cookies(&self) -> Option<CookieJar>;
    fn back(&self) -> Option<&str>;
}

impl<'a> RequestExtensions for super::Request<'a> {
    
    fn cookies(&self) -> Option<CookieJar> {            
        if let Some(cookies) = self.headers().get::<header::Cookie>() {
            let mut jar = CookieJar::new();
            for (k, v) in cookies.iter() {
                jar.add_original(Cookie::new(String::from(k), String::from(v)));
            }
            Some(jar)
        } else {
            None
        }
    }

    fn back(&self) -> Option<&str> {
        self.headers().get::<header::Referer>()
            .as_ref()
            .map(|x| &***x )
    }

}

pub trait ResponseExtensions {
    fn set_cookies(&mut self, jar: &CookieJar);
    fn with_cookies(self, jar: &CookieJar) -> Self;
}

impl ResponseExtensions for Response {

    fn set_cookies(&mut self, jar: &CookieJar) {
        let cookies = 
            jar.delta().map(Cookie::to_string).collect::<Vec<_>>();
        if !cookies.is_empty() {
            self.headers_mut().set(header::SetCookie(cookies));
        }
    }

    fn with_cookies(mut self, jar: &CookieJar) -> Self {
        self.set_cookies(jar);
        self
    }

}
