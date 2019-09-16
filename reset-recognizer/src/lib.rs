use regex::{CaptureLocations, Regex, RegexSet};
use std::collections::HashMap;
use std::str::FromStr;
/// Error handling
pub mod err {
    /// The error enum
    #[derive(Debug)]
    pub enum Error {
        Captures,
        Unmatched,
        Regex(regex::Error),
    }

    impl std::fmt::Display for Error {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            use Error::*;
            match self {
                Captures => "Could not parse captures".fmt(f),
                Unmatched => "Could not match path".fmt(f),
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

pub struct Captures {
    path: bytes::Bytes,
    locs: Option<CaptureLocations>,
    capture_names: CaptureNames,
}

impl Captures {
    fn new(path: &str, regex: &Regex, capture_names: CaptureNames) -> Self {
        let locs = {
            let mut locs = regex.capture_locations();
            regex.captures_read(&mut locs, path).map(|_| locs)
        };

        Self { path: path.into(), locs, capture_names }
    }

    pub fn name(&self, name: &str) -> Option<&str> {
        self.capture_names
            .0
            .as_ref()
            .and_then(|map| map.get(name.as_bytes()))
            .and_then(|i| self.get(*i))
    }

    pub fn get(&self, i: usize) -> Option<&str> {
        self.locs
            .as_ref()
            .and_then(|loc| loc.get(i))
            .map(|(start, end)| std::str::from_utf8(&self.path[start..end]).unwrap())
    }

    pub fn parsed<C: FromCaptures>(&self) -> err::Result<C> {
        Ok(C::from_captures(&self)?)
    }

    pub fn iter(&self) -> CapturesIter {
        let iter = self
            .locs
            .as_ref()
            .map(|locs| (0..locs.len()).into_iter().filter_map(move |i| locs.get(i)))
            .into_iter()
            .flatten()
            .collect::<Vec<_>>()
            .into_iter();

        CapturesIter(iter, std::str::from_utf8(&self.path).unwrap())
    }

    // pub fn captures(&self) -> Option<Captures> {
    //     self.regex.captures(std::str::from_utf8(self.path.as_ref()).unwrap())
    // }

    // pub fn all_captures(&self) -> Vec<&str> {
    //     self.captures().iter().flat_map(|x| x.iter() ).flatten().map(|x| x.as_str() ).collect()
    // }

    // pub fn parsed_captures<C: FromCaptures>(&self) -> err::Result<C> {
    //     Ok(C::from_captures(self.captures())?)
    // }
}

pub struct CapturesIter<'a>(std::vec::IntoIter<(usize, usize)>, &'a str);

impl<'a> Iterator for CapturesIter<'a> {
    type Item = &'a str;

    fn next(&mut self) -> Option<&'a str> {
        self.0.next().map(|(start, end)| &self.1[start..end])
    }
}

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
#[derive(Debug, Default)]
pub struct RouterBuilder<T> {
    parts: Vec<RouteParts<T>>,
}

impl<T> RouterBuilder<T> {
    pub fn new() -> Self {
        RouterBuilder { parts: Vec::new() }
    }

    pub fn add<I: Into<String>>(self, regex: I, handler: T) -> Self {
        self.add_with_priority(regex, 0, handler)
    }

    pub fn add_with_priority<I: Into<String>>(
        mut self,
        regex: I,
        priority: u8,
        handler: T,
    ) -> Self {
        self.parts.push(RouteParts { regex: regex.into(), priority, handler });

        self
    }

    pub fn finish(self) -> err::Result<Router<T>> {
        let (regexes, priorities, handlers) = self.parts.into_iter().fold(
            (Vec::new(), Vec::new(), Vec::new()),
            |(mut regexes, mut priorities, mut handlers),
             RouteParts { regex, priority, handler }| {
                regexes.push(regex);
                priorities.push(priority);
                handlers.push(handler);
                (regexes, priorities, handlers)
            },
        );

        let regex_set = RegexSet::new(regexes.iter()).map_err(err::Error::Regex)?;

        let (capture_names, regexes) = regexes
            .iter()
            .map(|regex| {
                Regex::new(regex)
                    .map(|regex| (CaptureNames::new(&regex), regex))
                    .map_err(err::Error::Regex)
            })
            .collect::<err::Result<Vec<_>>>()?
            .into_iter()
            .unzip();

        Ok(Router { regex_set, regexes, capture_names, priorities, handlers })
    }
}

#[derive(Clone)]
struct CaptureNames(Option<HashMap<bytes::Bytes, usize>>);

impl CaptureNames {
    fn new(regex: &Regex) -> Self {
        let map = regex
            .capture_names()
            .enumerate()
            .filter_map(|(i, opt_name)| opt_name.map(|name| (bytes::Bytes::from(name), i)))
            .collect::<HashMap<_, _>>();
        Self(if map.is_empty() { None } else { Some(map) })
    }
}

#[derive(Clone)]
pub struct Router<T> {
    regex_set: RegexSet,
    regexes: Vec<Regex>,
    capture_names: Vec<CaptureNames>,
    priorities: Vec<u8>,
    handlers: Vec<T>,
}

impl<T> Router<T> {
    pub fn build() -> RouterBuilder<T> {
        RouterBuilder::new()
    }

    pub fn recognize<'a>(&'a self, path: &str) -> err::Result<Match<'a, T>> {
        if let Some(i) = self
            .regex_set
            .matches(path)
            .iter()
            .min_by(|x, y| self.priorities[*x].cmp(&self.priorities[*y]))
        {
            let handler = &self.handlers[i];
            let regex = &self.regexes[i];
            let capture_names = &self.capture_names[i];
            Ok(Match { handler, captures: Captures::new(path, regex, capture_names.clone()) })
        } else {
            Err(err::Error::Unmatched)
        }
    }
}

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
