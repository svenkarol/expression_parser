struct Plus {
    parent: Option<Box<BinExp>>,
}
struct Minus{
    parent: Option<Box<BinExp>>,
}
struct Mult{
    parent: Option<Box<BinExp>>,
}
struct Div{
    parent: Option<Box<BinExp>>,
}

enum BinAOp {
    /// +
    Plus(Plus),
    /// -
    Minus(Minus),
    /// *
    Mult(Mult),
    /// /
    Div(Div)
}

struct BinExp {
    parent: Option<Box<BinExp>>,
    op: BinAOp,
    left: Box<AExp>,
    right: Box<AExp>
}

struct Number {
    parent: Option<Box<BinExp>>,
    value: i64
}

enum AExp {
    BinExp(BinExp),
    Number(Number)
}

impl BinExp {
    pub fn new(op: BinAOp, left: AExp, right: AExp, parent: Option<Box<BinExp>>) -> Self {
        BinExp {
            op: op,
            parent: parent,
            left: Box::new(left),
            right: Box::new(right)
        }
    }
}
