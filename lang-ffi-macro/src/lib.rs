extern crate proc_macro;

use proc_macro::TokenStream;
use quote::ToTokens;

#[proc_macro_attribute]
pub fn fluorine(_: TokenStream, body: TokenStream) -> TokenStream {
    match syn::parse::<syn::Item>(body).unwrap() {
        syn::Item::Fn(func) => {
            func.into_token_stream().into()
        }
        _ => panic!("FFI macros can only apply to functions"),
    }
}
