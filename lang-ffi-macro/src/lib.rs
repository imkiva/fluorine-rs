extern crate proc_macro;

#[macro_use]
extern crate quote;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use syn::{FnArg, Item, ItemFn, ItemMod, Signature};

#[proc_macro_attribute]
pub fn fluorine(attr: TokenStream, body: TokenStream) -> TokenStream {
    match syn::parse::<syn::Item>(body).unwrap() {
        syn::Item::Fn(func) => generate_ffi(func).into(),
        syn::Item::Mod(module) => generate_module_entrance(attr.to_string(), module).into(),
        _ => panic!("#[fluorine] should be used on mods"),
    }
}

fn generate_ffi(func: ItemFn) -> TokenStream2 {
    let vis = func.vis;
    let block = func.block;
    let attrs = func.attrs;
    let Signature {
        constness,
        asyncness,
        unsafety,
        abi,
        fn_token: _,
        ident,
        generics,
        paren_token: _,
        inputs,
        variadic,
        output,
    } = func.sig;

    let ffi_closure_name = format_ident!("ffi_closure_{}", ident);
    let argc = inputs.len();

    let args = inputs
        .iter()
        .map(|arg| match arg {
            FnArg::Receiver(_) => panic!("Unsupported receiver"),
            FnArg::Typed(ty) => ty.ty.as_ref(),
        })
        .zip(0..argc)
        .map(|(ty, idx)| {
            (
                ty,
                syn::parse_str::<syn::Expr>(&format!("{}", idx)).unwrap(),
            )
        })
        .map(|(ty, idx)| {
            quote! {
                (
                    {
                        let arg = param.pop_front().unwrap();
                        let t = arg.get_type();
                        <#ty>::ffi_from_value(
                            arg,
                            #idx,
                            stringify!(#ty).to_string(),
                            t,
                        )?
                    }
                ),
            }
        })
        .collect::<TokenStream2>();

    let closure = quote! {
        pub fn #ident(mut param: FFIParam) -> FFIResult {
            #(#attrs)* #vis #constness #unsafety #asyncness #abi
            fn #ffi_closure_name #generics ( #inputs, #variadic ) #output #block

            #ffi_closure_name(#args).ffi_into_value()
        }
    };

    closure.into()
}

fn generate_module_entrance(ffi_mod: String, module: ItemMod) -> TokenStream2 {
    let mod_name = format_ident!("{}", module.ident);

    let body = if let Some((_, items)) = &module.content {
        items
            .iter()
            .filter_map(|item| match item {
                Item::Fn(func) => Some(func),
                _ => None,
            })
            .map(|func| {
                let func_name = &func.sig.ident;
                let name_string = format!("{}", func_name);
                let argc =
                    syn::parse_str::<syn::Expr>(&format!("{}", func.sig.inputs.len())).unwrap();
                quote! {
                    map.insert(#name_string.to_string(), FFIClosure::new(#argc, #mod_name::#func_name));
                }
            })
            .collect::<TokenStream2>()
    } else {
        quote! {}
    };

    let entrance_name = format_ident!("fluorine_module_entrance_for_{}", ffi_mod);

    quote! {
        #module

        #[no_mangle]
        pub extern "C" fn #entrance_name() -> std::collections::HashMap<String, FFIClosure> {
            let mut map = std::collections::HashMap::<String, FFIClosure>::default();
            #body;
            map
        }
    }
}
