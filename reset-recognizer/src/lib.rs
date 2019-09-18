/*!
A fast [`RegexSet`](https://doc.rust-lang.org/regex/regex/struct.RegexSet.html) based path router, in the style of
[route-recognizer](https://docs.rs/route-recognizer).

[reset-router](https://docs.rs/reset-router), a higher level path router for use with Hyper 0.12, uses this library internally.

## Usage:

```rust,no_run
let mut router = reset_recognizer::Router::build()
    .add(r"^/posts/(.+)/comments/(.+)$", "comment".to_string())
    .add(r"^/posts/(.+)/comments$", "comments".to_string())
    .add(r"^/posts/(.+)$", "post".to_string())
    .add(r"^/posts$", "posts".to_string())
    .add(r"^/comments$", "comments2".to_string())
    .add(r"^/comments/(.+)$", "comment2".to_string())
    .add_with_priority(r"^/(.+)$", 1, "not_found".to_string())
    .finish()?;

let matched = router.recognize("/posts/100/comments/200")?;

let (post_id, comment_id) = matched.captures.parsed::<(i32, i32)>()?;

println!("{:?}", &matched.handler, &post_id, &comment_id);

```
*/

use std::collections::HashMap;
use std::str::FromStr;

/// Error handling
pub mod err {
    /// The error enum
    #[derive(Debug)]
    pub enum Error {
        Captures,
        Unmatched(String),
        Regex(regex::Error),
    }

    impl std::fmt::Display for Error {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            use Error::*;
            match self {
                Captures => "Could not parse captures".fmt(f),
                Unmatched(ref path) => write!(f, "Could not match path: {}", path),
                Regex(ref inner) => inner.fmt(f),
            }
        }
    }

    impl std::error::Error for Error {
        fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
            use Error::*;
            match self {
                Regex(ref inner) => Some(inner),
                _ => None,
            }
        }
    }

    /// Result wrapper: `Result<T, Error>`
    pub type Result<T> = std::result::Result<T, Error>;
}

/// Captures data for matched Regex
pub struct Captures {
    path: String,
    locs: Option<regex::CaptureLocations>,
    capture_names: Option<CaptureNames>,
}

impl Captures {
    fn new(
        path: String,
        regex: &regex::Regex,
        mut locs: regex::CaptureLocations,
        capture_names: Option<CaptureNames>,
    ) -> Self {
        let locs = regex.captures_read(&mut locs, &path).map(|_| locs);

        Self { path, locs, capture_names }
    }

    /// Get named capture match
    pub fn name(&self, name: &str) -> Option<&str> {
        self.capture_names.as_ref().and_then(|map| map.0.get(name)).and_then(|i| self.get(*i))
    }

    /// Get positional capture match
    ///
    /// Uses `unsafe`, but should be safe: the string is not changed between `Regex::captures_read` and this call.
    pub fn get(&self, i: usize) -> Option<&str> {
        self.locs
            .as_ref()
            .and_then(|loc| loc.get(i))
            .map(|(start, end)| unsafe { self.path.get_unchecked(start..end) })
    }

    /// Parse positional captures into tuple
    pub fn parsed<C: FromCaptures>(&self) -> err::Result<C> {
        Ok(C::from_captures(&self)?)
    }

    /// Iterate over all captures (including capture 0) as `Iterator<Item=&str>`
    pub fn iter(&self) -> CapturesIter {
        let iter = self
            .locs
            .as_ref()
            .map(|locs| (0..locs.len()).into_iter().filter_map(move |i| locs.get(i)))
            .into_iter()
            .flatten()
            .collect::<Vec<_>>()
            .into_iter();

        CapturesIter(iter, &self.path)
    }
}

#[doc(hidden)]
pub struct CapturesIter<'a>(std::vec::IntoIter<(usize, usize)>, &'a str);

impl<'a> Iterator for CapturesIter<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<&'a str> {
        // As above, this should be safe since we are not changing the path string at any point
        self.0.next().map(|(start, end)| unsafe { self.1.get_unchecked(start..end) })
    }
}

/// Match data for a recognized route
pub struct Match<'a, T> {
    pub handler: &'a T,
    pub captures: Captures,
}

#[derive(Debug)]
struct RouteParts<T> {
    regex: String,
    priority: u8,
    handler: T,
}

/// Builder for a `Router`
#[derive(Default)]
pub struct RouterBuilder<T> {
    parts: Vec<RouteParts<T>>,
    regex_set_options_configurator: Option<Box<dyn Fn(&mut regex::RegexSetBuilder)>>,
    regex_options_configurator: Option<Box<dyn Fn(&mut regex::RegexBuilder)>>,
}

impl<T> RouterBuilder<T> {
    fn new() -> Self {
        RouterBuilder {
            parts: Vec::new(),
            regex_set_options_configurator: None,
            regex_options_configurator: None,
        }
    }

    /// Add a route with handler and default priority 0
    pub fn add<I: Into<String>>(self, regex: I, handler: T) -> Self {
        self.add_with_priority(regex, 0, handler)
    }

    /// Add a route with handler and priority (lower is better)
    pub fn add_with_priority<I: Into<String>>(
        mut self,
        regex: I,
        priority: u8,
        handler: T,
    ) -> Self {
        self.parts.push(RouteParts { regex: regex.into(), priority, handler });

        self
    }

    /// If you need to make any changes to `regex::RegexSetBuilder`
    pub fn configure_regex_set_builder<F: Fn(&mut regex::RegexSetBuilder) + 'static>(
        mut self,
        f: F,
    ) -> Self {
        self.regex_set_options_configurator = Some(Box::new(f));
        self
    }

    /// If you need to make any changes to `regex::RegexBuilder`
    pub fn configure_regex_builder<F: Fn(&mut regex::RegexBuilder) + 'static>(
        mut self,
        f: F,
    ) -> Self {
        self.regex_options_configurator = Some(Box::new(f));
        self
    }

    /// Build finished router
    pub fn finish(self) -> err::Result<Router<T>> {
        let (regex_strs, priorities, handlers) = self.parts.into_iter().fold(
            (Vec::new(), Vec::new(), Vec::new()),
            |(mut regex_strs, mut priorities, mut handlers),
             RouteParts { regex, priority, handler }| {
                regex_strs.push(regex);
                priorities.push(priority);
                handlers.push(handler);
                (regex_strs, priorities, handlers)
            },
        );

        let regex_set = {
            let mut builder = regex::RegexSetBuilder::new(regex_strs.iter());
            if let Some(regex_set_options_configurator) = self.regex_set_options_configurator {
                regex_set_options_configurator(&mut builder);
            }
            builder.build().map_err(err::Error::Regex)?
        };

        let mut regexes = Vec::new();
        let mut capture_names = Vec::new();
        let mut capture_locations = Vec::new();

        for regex_str in regex_strs.iter() {
            let regex = {
                let mut builder = regex::RegexBuilder::new(regex_str);
                if let Some(ref regex_options_configurator) = self.regex_options_configurator {
                    regex_options_configurator(&mut builder);
                }
                builder.build().map_err(err::Error::Regex)?
            };
            capture_names.push(CaptureNames::build(&regex));
            capture_locations.push(regex.capture_locations());
            regexes.push(regex);
        }

        Ok(Router { regex_set, regexes, capture_names, capture_locations, priorities, handlers })
    }
}

#[derive(Clone)]
struct CaptureNames(std::sync::Arc<HashMap<String, usize>>);

impl CaptureNames {
    fn build(regex: &regex::Regex) -> Option<Self> {
        let map = regex
            .capture_names()
            .enumerate()
            .filter_map(|(i, opt_name)| opt_name.map(|name| (String::from(name), i)))
            .collect::<HashMap<_, _>>();
        if map.is_empty() {
            None
        } else {
            Some(Self(std::sync::Arc::new(map)))
        }
    }
}

/// The complete route matcher
pub struct Router<T> {
    regex_set: regex::RegexSet,
    regexes: Vec<regex::Regex>,
    capture_locations: Vec<regex::CaptureLocations>,
    capture_names: Vec<Option<CaptureNames>>,
    priorities: Vec<u8>,
    handlers: Vec<T>,
}

impl<T> Router<T> {
    /// Build a new router
    pub fn build() -> RouterBuilder<T> {
        RouterBuilder::new()
    }

    /// Match a route and return match data
    pub fn recognize<'a, I: Into<String>>(&'a self, path: I) -> err::Result<Match<'a, T>> {
        let path = path.into();
        if let Some(i) = self
            .regex_set
            .matches(&path)
            .iter()
            .min_by(|x, y| self.priorities[*x].cmp(&self.priorities[*y]))
        {
            let handler = &self.handlers[i];
            let regex = &self.regexes[i];
            let capture_names = &self.capture_names[i];
            let capture_locations = &self.capture_locations[i];
            Ok(Match {
                handler,
                captures: Captures::new(
                    path,
                    regex,
                    capture_locations.clone(),
                    capture_names.clone(),
                ),
            })
        } else {
            Err(err::Error::Unmatched(path))
        }
    }
}

/// Parse captures data into tuples
pub trait FromCaptures: Sized {
    fn from_captures(caps: &Captures) -> err::Result<Self>;
}

impl<U: FromStr> FromCaptures for (U,) {
    fn from_captures(caps: &Captures) -> err::Result<Self> {
        let out_1 = caps.get(1).and_then(|x| x.parse().ok()).ok_or(err::Error::Captures)?;
        Ok((out_1,))
    }
}

impl<U1: FromStr, U2: FromStr> FromCaptures for (U1, U2) {
    fn from_captures(caps: &Captures) -> err::Result<Self> {
        let out_1 = caps.get(1).and_then(|x| x.parse().ok()).ok_or(err::Error::Captures)?;
        let out_2 = caps.get(2).and_then(|x| x.parse().ok()).ok_or(err::Error::Captures)?;
        Ok((out_1, out_2))
    }
}

impl<U1: FromStr, U2: FromStr, U3: FromStr> FromCaptures for (U1, U2, U3) {
    fn from_captures(caps: &Captures) -> err::Result<Self> {
        let out_1 = caps.get(1).and_then(|x| x.parse().ok()).ok_or(err::Error::Captures)?;
        let out_2 = caps.get(2).and_then(|x| x.parse().ok()).ok_or(err::Error::Captures)?;
        let out_3 = caps.get(3).and_then(|x| x.parse().ok()).ok_or(err::Error::Captures)?;
        Ok((out_1, out_2, out_3))
    }
}

impl<U1: FromStr, U2: FromStr, U3: FromStr, U4: FromStr> FromCaptures for (U1, U2, U3, U4) {
    fn from_captures(caps: &Captures) -> err::Result<Self> {
        let out_1 = caps.get(1).and_then(|x| x.parse().ok()).ok_or(err::Error::Captures)?;
        let out_2 = caps.get(2).and_then(|x| x.parse().ok()).ok_or(err::Error::Captures)?;
        let out_3 = caps.get(3).and_then(|x| x.parse().ok()).ok_or(err::Error::Captures)?;
        let out_4 = caps.get(4).and_then(|x| x.parse().ok()).ok_or(err::Error::Captures)?;
        Ok((out_1, out_2, out_3, out_4))
    }
}
