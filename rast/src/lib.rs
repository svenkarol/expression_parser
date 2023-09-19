#![crate_type = "proc-macro"]
extern crate proc_macro;
use proc_macro::TokenStream;



#[proc_macro]
pub fn rast_ast(item: TokenStream) -> TokenStream {
    let mut i = 0;
    for tk in item {
         println!("{}: {}\n", { let tmp = i; i += 1; tmp }, tk);   
    }
    "pub struct BinExp;".parse().unwrap()
}

