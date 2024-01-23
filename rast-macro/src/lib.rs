#![crate_type = "proc-macro"]
extern crate proc_macro;
extern crate rast;
use peekmore::PeekMore;
use proc_macro::TokenStream;
//use rast::combinators;

#[proc_macro]
pub fn rast_ast(item: TokenStream) -> TokenStream {
    let mut i = 0;
    //let mut it = item.into_iter().collect::<Vec<TokenTree>>();
    /*let mut it: combinators::ParseInput =
        proc_macro2::TokenStream::from(item).into_iter().peekmore();

    for tk in it {
        println!(
            "{}: {}\n",
            {
                let tmp = i;
                i += 1;
                tmp
            },
            tk
        );
    }*/
    "pub struct BinExp;".parse().unwrap()
}
