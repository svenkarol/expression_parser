use serde::Serialize;
use std::cell::RefCell;
use std::rc::Rc;

/// https://doc.rust-lang.org/book/ch15-06-reference-cycles.html

#[derive(Serialize)]
pub struct Plus {
    parent: Option<Box<BinExp>>,
}
impl Plus {
    pub fn new(parent: Option<Box<BinExp>>) -> Self {
        Plus { parent: parent }
    }
}
#[derive(Serialize)]
pub struct Minus {
    parent: Option<Box<BinExp>>,
}

impl Minus {
    pub fn new(parent: Option<Box<BinExp>>) -> Self {
        Minus { parent: parent }
    }
}
#[derive(Serialize)]
pub struct Mult {
    parent: Option<Box<BinExp>>,
}

impl Mult {
    pub fn new(parent: Option<Box<BinExp>>) -> Self {
        Mult { parent: parent }
    }
}
#[derive(Serialize)]
pub struct Div {
    parent: Option<Box<BinExp>>,
}

impl Div {
    pub fn new(parent: Option<Box<BinExp>>) -> Self {
        Div { parent: parent }
    }
}
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
#[derive(Serialize)]
pub struct BinExp {
    #[serde(skip_serializing)]
    _parent: ParentRel<BinExp>,
    op: BinAOp,
    left: ChildRel<AExp>,
    right: ChildRel<AExp>,
}

pub type ParentRel<T> = Option<RefCell<Rc<T>>>;
pub type ChildRel<T> = RefCell<Rc<T>>;

pub trait RelHelper<T> {
    fn from_value(val: T) -> Self;
    fn from_ref(rc: &Rc<T>) -> Self;
}

impl<T> RelHelper<T> for ChildRel<T> {
    fn from_value(val: T) -> Self {
        RefCell::new(Rc::new(val))
    }
    fn from_ref(rc: &Rc<T>) -> Self {
        RefCell::new(rc.clone())
    }
}

impl<T> RelHelper<T> for ParentRel<T> {
    
    fn from_value(val: T) -> Self {
        Some(RefCell::new(Rc::new(val)))
    }
    fn from_ref(rc: &Rc<T>) -> Self {
        Some(RefCell::new(rc.clone()))
    }

    /*fn new_from(val: &ChildRel<T>) -> Self {
        let weak_ref = RefCell::new(Weak::new());
        *weak_ref.borrow_mut() = Rc::downgrade(&*val.borrow_mut());
        return Some(weak_ref);
    }*/
    // Side note: would be nice to use weak references when building relations to the parent
    // However, we can only derive weak references when we have a strong one already. As we
    // want the strong references to owned by a child's parent, this creates a chicken-egg-problem
    // and basically urges one to construct the tree top-down with a definitive root. Another option
    // would be to derive the parents after the tree was completed, which gives back freedom
    // in tree construction but implies an extra pass over the tree.
}

impl BinExp {
    pub fn new(op: BinAOp, left: AExp, right: AExp, _self_parent: Option<&Rc<BinExp>>) -> Self {
        let exp = BinExp {
            op: op,
            _parent: None, // how to set this? See above and below ...
            left: ChildRel::from_value(left),
            right: ChildRel::from_value(right),
        };
        /*match *exp.left.borrow().as_ref() {
            AExp::BinExp(lexp) => lexp.parent = ParentRel::from_ref(self_parent.)
        }*/ 
        // Problem: we cannot clone the parent-child-rc as these have different types ...
        // and the parent may derive from different contexts
        return exp;
    }
}

#[derive(Serialize)]
pub struct Number {
    parent: Option<Box<BinExp>>,
    value: i64,
}

impl Number {
    pub fn new(value: i64, parent: Option<Box<BinExp>>) -> Self {
        Number {
            parent: parent,
            value: value,
        }
    }
}
#[derive(Serialize)]
pub enum AExp {
    BinExp(BinExp),
    Number(Number),
}

#[cfg(test)]
mod tests {
    use super::AExp;
    use super::BinExp;
    use super::Mult;
    use super::Plus;
    use super::Number;

    #[test]
    fn test_ast_creation() {
        let num1 = AExp::Number(Number::new(0 as i64, None));
        let num2 = AExp::Number(Number::new(8 as i64, None));
        let num3 = AExp::Number(Number::new(15 as i64, None));
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
        assert_eq!(("{\"op\":{\"Plus\":{\"parent\":null}},\"left\":{\"Number\":{\"parent\":null,\"value\":0}},\"right\":{\"BinExp\":{\"op\":{\"Mult\":{\"parent\":null}},\"left\":{\"Number\":{\"parent\":null,\"value\":8}},\"right\":{\"Number\":{\"parent\":null,\"value\":15}}}}}"), json_txt);
    }
}
