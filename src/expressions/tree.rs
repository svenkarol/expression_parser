use serde::Serialize;
use std::cell::RefCell;
use std::rc::Rc;

/// https://doc.rust-lang.org/book/ch15-06-reference-cycles.html

#[derive(Serialize)]
pub struct Plus {
    #[serde(skip_serializing)]
    parent: Option<PlusCtx>,
}
impl Plus {
    pub fn new(parent: Option<PlusCtx>) -> Self {
        Plus { parent: parent }
    }
}
#[derive(Serialize)]
pub struct Minus {
    #[serde(skip_serializing)]
    parent: Option<MinusCtx>,
}

impl Minus {
    pub fn new(parent: Option<MinusCtx>) -> Self {
        Minus { parent: parent }
    }
}
#[derive(Serialize)]
pub struct Mult {
    #[serde(skip_serializing)]
    parent: Option<MultCtx>,
}

impl Mult {
    pub fn new(parent: Option<MultCtx>) -> Self {
        Mult { parent: parent }
    }
}
#[derive(Serialize)]
pub struct Div {
    #[serde(skip_serializing)]
    parent: Option<DivCtx>,
}

impl Div {
    pub fn new(parent: Option<DivCtx>) -> Self {
        Div { parent: parent }
    }
}

#[derive(Serialize)]
pub struct BinExp {
    #[serde(skip_serializing)]
    parent: Option<BinExpCtx>,
    op: BinAOp,
    left: AExp,
    right: AExp,
}

impl BinExp {
    pub fn new(op: BinAOp, left: AExp, right: AExp, parent: Option<BinExpCtx>) -> AstRef<Self> {
        let exp = BinExp {
            op: op,
            parent: parent,
            left: left,
            right: right,
        };
        let exp_ref = AstRef::from_value(exp);
        match &exp_ref.borrow_mut().left {
            AExp::BinExp(lexp) => {
                lexp.borrow_mut().parent = Some(BinExpCtx::BinExp(AstRef::from_ref(&exp_ref)))
            }
            AExp::Number(lnum) => {
                lnum.borrow_mut().parent = Some(NumberCtx::BinExp(AstRef::from_ref(&exp_ref)))
            }
        };
        return exp_ref;
    }
}

#[derive(Serialize)]
pub struct Number {
    #[serde(skip_serializing)]
    parent: Option<NumberCtx>,
    value: i64,
}

impl Number {
    pub fn new(value: i64, parent: Option<NumberCtx>) -> Self {
        Number {
            parent: parent,
            value: value,
        }
    }
}

/// Operator choice
#[derive(Serialize)]
pub enum BinAOp {
    /// +
    Plus(Plus),
    /// -
    Minus(Minus),
    /// *
    Mult(Mult),
    /// /
    Div(Div),
}

/// Operand choice
#[derive(Serialize)]
pub enum AExp {
    BinExp(AstRef<BinExp>),
    Number(AstRef<Number>),
}

#[derive(Serialize)]
pub enum PlusCtx {
    BinExp(AstRef<BinExp>),
}
#[derive(Serialize)]
pub enum MultCtx {
    BinExp(AstRef<BinExp>),
}
#[derive(Serialize)]
pub enum DivCtx {
    BinExp(AstRef<BinExp>),
}
#[derive(Serialize)]
pub enum MinusCtx {
    BinExp(AstRef<BinExp>),
}
#[derive(Serialize)]
pub enum NumberCtx {
    BinExp(AstRef<BinExp>),
}
#[derive(Serialize)]
pub enum BinExpCtx {
    BinExp(AstRef<BinExp>),
}

pub type AstRef<T> = Rc<RefCell<T>>;

pub trait RelHelper<T> {
    fn from_value(val: T) -> Self;
    fn from_ref(rc: &Rc<RefCell<T>>) -> Self;
}

impl<T> RelHelper<T> for AstRef<T> {
    fn from_value(val: T) -> Self {
        Rc::new(RefCell::new(val))
    }
    fn from_ref(rc: &Rc<RefCell<T>>) -> Self {
        rc.clone()
    }
}

#[cfg(test)]
mod tests {
    use crate::expressions::tree::RelHelper;

    use super::AExp;
    use super::BinExp;
    use super::Mult;
    use super::Number;
    use super::Plus;
    use super::AstRef;

    #[test]
    fn test_ast_creation() {
        let num1 = AExp::Number(AstRef::from_value(Number::new(0 as i64, None)));
        let num2 = AExp::Number(AstRef::from_value(Number::new(8 as i64, None)));
        let num3 = AExp::Number(AstRef::from_value(Number::new(15 as i64, None)));
        let mul = AExp::BinExp(BinExp::new(
            super::BinAOp::Mult(Mult::new(None)),
            num2,
            num3,
            None, // how to set?
        ));
        let add_tree = BinExp::new(
            super::BinAOp::Plus(Plus::new(None)),
            num1,
            mul,
            None, // how to set?
        );
        let json = serde_json::to_string(&add_tree);
        assert!(json.is_ok());
        let json_txt = json.unwrap();
        assert_eq!(("{\"op\":{\"Plus\":{}},\"left\":{\"Number\":{\"value\":0}},\"right\":{\"BinExp\":{\"op\":{\"Mult\":{}},\"left\":{\"Number\":{\"value\":8}},\"right\":{\"Number\":{\"value\":15}}}}}"), json_txt);
    }
}
