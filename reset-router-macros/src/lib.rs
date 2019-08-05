extern crate proc_macro;
extern crate proc_macro2;
extern crate proc_macro_hack;
extern crate quote;
extern crate syn;

use proc_macro::TokenStream;
use proc_macro2::Span;
use proc_macro_hack::proc_macro_hack;
use quote::quote;

use syn::*;

#[proc_macro_hack]
pub fn routes(item: TokenStream) -> TokenStream {
    use syn::parse::Parser;

    let paths = <punctuated::Punctuated<Path, token::Comma>>::parse_terminated.parse(item).unwrap();

    let parts = paths
        .iter()
        .map(|path| {
            let mut fn_name_path = path.clone();
            {
                let mut last_seg = fn_name_path.segments.last_mut().unwrap();
                last_seg.value_mut().ident = Ident::new(
                    &format!(
                        "RESET_ROUTER_ROUTE_PARTS_FOR_{}",
                        last_seg.value().ident.to_string().trim_start_matches("r#").to_owned()
                    ),
                    Span::call_site()
                );
            }
            let fn_name_path = &fn_name_path;
            quote!({
                let (method, regex, priority) = #fn_name_path();
                (method, regex, priority, #path.into())
            })
        })
        .collect::<Vec<_>>();

    let out = quote!(vec![#(#parts),*]);

    out.into()
}

/// Use like `#[get("/path")]`
///
/// Optionally include priority, e.g.: `#[get("/path", priority=1)]`
#[proc_macro_attribute]
pub fn get(attrs: TokenStream, item: TokenStream) -> TokenStream { named_route(attrs, item, "GET") }

/// Use like `#[post("/path")]`
///
/// Optionally include priority, e.g.: `#[post("/path", priority=1)]`
#[proc_macro_attribute]
pub fn post(attrs: TokenStream, item: TokenStream) -> TokenStream {
    named_route(attrs, item, "POST")
}

/// Use like `#[put("/path")]`
///
/// Optionally include priority, e.g.: `#[put("/path", priority=1)]`
#[proc_macro_attribute]
pub fn put(attrs: TokenStream, item: TokenStream) -> TokenStream { named_route(attrs, item, "PUT") }

/// Use like `#[patch("/path")]`
///
/// Optionally include priority, e.g.: `#[patch("/path", priority=1)]`
#[proc_macro_attribute]
pub fn patch(attrs: TokenStream, item: TokenStream) -> TokenStream {
    named_route(attrs, item, "PATCH")
}

/// Use like `#[head("/path")]`
///
/// Optionally include priority, e.g.: `#[head("/path", priority=1)]`
#[proc_macro_attribute]
pub fn head(attrs: TokenStream, item: TokenStream) -> TokenStream {
    named_route(attrs, item, "HEAD")
}

/// Use like `#[delete("/path")]`
///
/// Optionally include priority, e.g.: `#[delete("/path", priority=1)]`
#[proc_macro_attribute]
pub fn delete(attrs: TokenStream, item: TokenStream) -> TokenStream {
    named_route(attrs, item, "DELETE")
}

/// Use like `#[route(path="/path")]`
///
/// Optionally include comma separated HTTP methods to match , e.g.: `#[route(path="/path", methods="GET, POST")]`
///
/// Optionally include priority, e.g.: `#[route(path="/path", methods="GET, POST", priority=1)]`
#[proc_macro_attribute]
pub fn route(attrs: TokenStream, item: TokenStream) -> TokenStream {
    let attrs = parse_macro_input!(attrs as AttributeArgs);
    route_inner(attrs, item)
}

fn named_route(attrs: TokenStream, item: TokenStream, method: &str) -> TokenStream {
    let attrs = parse_macro_input!(attrs as AttributeArgs);
    let mut new_attrs = Vec::with_capacity(attrs.len());
    for attr in attrs.into_iter() {
        if let NestedMeta::Literal(ref lit) = attr {
            new_attrs.push(NestedMeta::Meta(Meta::NameValue(parse_quote!(path=#lit))));
        } else {
            new_attrs.push(attr);
        }
    }
    new_attrs.push(NestedMeta::Meta(Meta::NameValue(parse_quote!(methods=#method))));
    route_inner(new_attrs, item)
}

fn route_inner(attrs: AttributeArgs, item: TokenStream) -> TokenStream {
    let item = parse_macro_input!(item as ItemFn);

    let params = attrs
        .iter()
        .filter_map(|x| match x {
            NestedMeta::Meta(y) => Some(y),
            _ => None
        })
        .filter_map(|x| match x {
            Meta::NameValue(y) => Some(y),
            _ => None
        });

    let regex_str = params
        .clone()
        .find(|x| x.ident == "path")
        .and_then(|x| match x.lit {
            Lit::Str(ref y) => Some(y),
            _ => None
        })
        .map(|x| x.value())
        .expect("No path provided");

    let methods_str = params
        .clone()
        .find(|x| x.ident == "methods")
        .and_then(|x| match x.lit {
            Lit::Str(ref y) => Some(y),
            _ => None
        })
        .map(|x| x.value())
        .unwrap_or_else(|| "".into());

    let priority_int = params
        .clone()
        .find(|x| x.ident == "priority")
        .and_then(|x| match x.lit {
            Lit::Int(ref y) => Some(y),
            _ => None
        })
        .map(|x| x.value() as u8)
        .unwrap_or_else(|| 0);

    let method_bits = {
        let mut method_iter = methods_str.split(',').map(|x| x.trim().to_lowercase());

        let cls = |s: &str| match s {
            "get" => quote!(_reset_router::bits::Method::GET),
            "post" => quote!(_reset_router::bits::Method::POST),
            "put" => quote!(_reset_router::bits::Method::PUT),
            "patch" => quote!(_reset_router::bits::Method::PATCH),
            "head" => quote!(_reset_router::bits::Method::HEAD),
            "delete" => quote!(_reset_router::bits::Method::DELETE),
            _ => panic!("Unknown method parameter")
        };

        let first = method_iter.next();

        match first {
            Some(m) => {
                let base = cls(&m);
                method_iter.fold(base, |acc, val| {
                    let val = cls(&val);
                    quote!(#acc | #val)
                })
            }
            _ => quote!(_reset_router::bits::Method::all())
        }
    };

    let fn_name = Ident::new(
        &format!(
            "RESET_ROUTER_ROUTE_PARTS_FOR_{}",
            item.ident.to_string().trim_start_matches("r#").to_owned()
        ),
        Span::call_site()
    );

    let out = quote!{
        #item
        #[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
        pub fn #fn_name() -> (u32, &'static str, u8) {
            #[allow(unknown_lints)]
            #[cfg_attr(feature = "cargo-clippy", allow(useless_attribute))]
            extern crate reset_router as _reset_router;
            ((#method_bits).bits(), #regex_str, #priority_int)
        }
    };

    out.into()
}
