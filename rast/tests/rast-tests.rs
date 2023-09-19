#[macro_use]
extern crate rast;

rast_ast!(
    AExp   -> BinExp | Number 
    BinExp -> op: BinAOp left: AExp right: AExp
    BinAOp -> Plus | Minus | Mult | Div
    Number -> value: <i64>
    Plus;
    Minus;
    Mult;
    Div);

#[test]
fn test_hock() {
   
}