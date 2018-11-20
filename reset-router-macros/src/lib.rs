extern crate proc_macro;
extern crate proc_macro2;
extern crate proc_macro_hack;
extern crate quote;
extern crate syn;

use proc_macro2::{Span, TokenStream as TokenStream2};
use proc_macro::TokenStream;
use proc_macro_hack::proc_macro_hack;
use quote::quote;

use syn::*;

/// Add one to an expression.
#[proc_macro_hack]
pub fn routes(item: TokenStream) -> TokenStream {
    // panic!("{:#?}", TokenStream2::from(item));
    use syn::parse::Parser;

    let paths = <punctuated::Punctuated<Path, token::Comma>>::parse_terminated
        .parse(item)
        .unwrap();

    let parts = paths
        .iter()
        .map(|path| {
            let mut const_name_path = path.clone();
            {
                let mut last_seg = const_name_path.segments.last_mut().unwrap();
                last_seg.value_mut().ident = Ident::new(
                    &format!("RESET_ROUTER_ROUTE_PARTS_FOR_{}", last_seg.value().ident.to_string().trim_left_matches("r#").to_owned()),
                    Span::call_site()
                );
            }
            quote!((#const_name_path, #path))
        })
        .collect::<Vec<_>>();



    panic!("{}", parts.iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", "));

    // let mut item_iter = TokenStream2::from(item).into_iter();
    
    // let mut arr = Vec::new();

    // let mut curr = None;

    // loop {
    //     match item_iter.next() {
    //         Some(proc_macro2::TokenTree::Punct(ref punct)) if punct.as_char() == ',' => {
    //             if let Some(item) = curr {
    //                 arr.push(item);
    //             }
    //             curr = None;
    //         }
    //         Some(ref other) => {
    //             if let Some(item) = curr {
    //                 curr = Some(quote!(#item #other));
    //             } else {
    //                 curr = Some(quote!(#other))
    //             }
    //         },
    //         None => { break; }
    //     }
    // }

    // if let Some(item) = curr {
    //     arr.push(item);
    // }

    // panic!("{}", arr.into_iter().map(|x| x.to_string()).collect::<Vec<_>>().join(", "));

}

#[proc_macro_attribute]
pub fn get(attrs: TokenStream, item: TokenStream) -> TokenStream {
    panic!("{:#?}", attrs);
}

#[proc_macro_attribute]
pub fn route(attrs: TokenStream, item: TokenStream) -> TokenStream {

    let attrs = parse_macro_input!(attrs as AttributeArgs);
    let item = parse_macro_input!(item as ItemFn);
    
    let params = attrs.iter()
        .filter_map(|x| match x { NestedMeta::Meta(y) => Some(y), _ => None })
        .filter_map(|x| match x { Meta::NameValue(y) => Some(y), _ => None });

    let path_str = params.clone().find(|x| x.ident == "path" )
        .and_then(|x| match x.lit { Lit::Str(ref y) => Some(y), _ => None })
        .map(|x| x.value() )
        .expect("No path provided");

    let methods_str = params.clone().find(|x| x.ident == "methods" )
        .and_then(|x| match x.lit { Lit::Str(ref y) => Some(y), _ => None })
        .map(|x| x.value() )
        .expect("No method provided");

    let method_bits = {
        let mut method_iter = methods_str.split(",").map( |x| x.trim().to_lowercase() );

        let cls = |s: &str| {
            match s {
                "get" => quote!(_reset_router::bits::Method::GET),
                "post" => quote!(_reset_router::bits::Method::POST),
                "put" => quote!(_reset_router::bits::Method::PUT),
                "patch" => quote!(_reset_router::bits::Method::PATCH),
                "head" => quote!(_reset_router::bits::Method::HEAD),
                "delete" => quote!(_reset_router::bits::Method::DELETE),
                _ => panic!("Unknown method parameter")
            }
        };

        let first = method_iter.next();

        match first {
            Some(m) => {
                let base = cls(&m);
                method_iter.fold(base, |acc, val| {
                    let val = cls(&val);
                    quote!(#acc | #val)
                })
            },
            _ => quote!(_reset_router::bits::Method::all())
        }

    };

    let const_name = Ident::new(
        &format!("RESET_ROUTER_ROUTE_PARTS_FOR_{}", item.ident.to_string().trim_left_matches("r#").to_owned()),
        Span::call_site()
    );

    let item_ident = item.ident.to_string();

    let out = quote!{
        #item
        #[allow(non_upper_case_globals, unused_attributes, unused_qualifications)]
        const #const_name: (&'static str, u32) = {
            #[allow(unknown_lints)]
            #[cfg_attr(feature = "cargo-clippy", allow(useless_attribute))]
            extern crate reset_router as _reset_router;
            (#path_str, (#method_bits).bits)
        };
    };

    out.into()

}