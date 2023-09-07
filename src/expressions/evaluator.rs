use crate::expressions::parser::Tree;
use crate::expressions::parser::NodeType;
use crate::expressions::lexer::ExpToken;
use crate::expressions::lexer::TokenType;


/// Evaluates a given expression tree by computing the expression's value.
pub fn eval(exp: &Tree) -> Result<i64, String> {
    match exp {
        Tree::Leaf(tk) => match tk.ttype {
            TokenType::NUM => match &tk.txt {
                Some(lexem) => lexem
                    .parse::<i64>()
                    .map_err(|err| format!("Error when converting Integer ({:?})", err.kind())),
                _ => Err("No lexem for number token found.".to_string()),
            },
            _ => Err(format!(
                "Wrong token type {:?}. Number was expected.",
                tk.ttype
            )),
        },
        Tree::Node(node) => match &node.kind {
            NodeType::Start => {
                if node.child_count() == 1 {
                    eval(&node.get_child(0))
                } else {
                    Err(format!(
                        "Expected 1 child of start, found {}.",
                        node.child_count()
                    ))
                }
            }
            NodeType::Exp => {
                if node.child_count() == 2 {
                    eval(&node.get_child(0)).and_then(|val| {
                        if node.get_child(1).is_epsilon() {
                            Ok(val)
                        } else {
                            eval_expp(&node.get_child(1), val)
                        }
                    })
                } else {
                    Err(format!(
                        "Expected 2 children of exp, found {}.",
                        node.child_count()
                    ))
                }
            }
            NodeType::Term => {
                if node.child_count() == 2 {
                    eval(&node.get_child(0)).and_then(|val| {
                        if node.get_child(1).is_epsilon() {
                            Ok(val)
                        } else {
                            eval_termp(&node.get_child(1), val)
                        }
                    })
                } else {
                    Err(format!(
                        "Expected 2 children of term, found {}.",
                        node.child_count()
                    ))
                }
            }
            _ => Err(format!("Unexpected case.")),
        },
    }
}

fn eval_expp(exp: &Tree, lop: i64) -> Result<i64, String> {
    exp.as_node().and_then(|node| {
        if node.kind == NodeType::Expp {
            if node.child_count() == 3 {
                eval(&node.get_child(1)).and_then(|rop| {
                    node.get_child(0).as_leaf().and_then(|tk| {
                        eval_addop(tk, lop, rop).and_then(|lop| {
                            if node.get_child(2).is_epsilon() {
                                Ok(lop)
                            } else {
                                eval_expp(&node.get_child(2), lop)
                            }
                        })
                    })
                })
            } else {
                Err(format!(
                    "Expected 3 children of expp, found {}.",
                    node.child_count()
                ))
            }
        } else {
            Err(format!("Node type expp expected."))
        }
    })
} 

fn eval_termp(exp: &Tree, lop: i64) -> Result<i64, String> {
    exp.as_node().and_then(|node| {
        if node.kind == NodeType::Termp {
            if node.child_count() == 3 {
                eval(&node.get_child(1)).and_then(|rop| {
                    node.get_child(0).as_leaf().and_then(|tk| {
                        eval_mulop(tk, lop, rop).and_then(|lop| {
                            if node.get_child(2).is_epsilon() {
                                Ok(lop)
                            } else {
                                eval_termp(&node.get_child(2), lop)
                            }
                        })
                    })
                })
            } else {
                Err(format!(
                    "Expected 3 children of termp, found {}.",
                    node.child_count()
                ))
            }
        } else {
            Err(format!("Node type termp expected. Got {:?}.", node.kind))
        }
    })
}

fn eval_addop(tk: &ExpToken, lop: i64, rop: i64) -> Result<i64, String> {
    if tk.ttype == TokenType::ADDOP {
        match &tk.txt {
            Some(operator) => match operator.as_str() {
                "+" => Ok(lop + rop),
                "-" => Ok(lop - rop),
                _ => Err(format!("Wrong operator text (should not happen).")),
            },
            None => Err(format!("No operator text (should not happen).")),
        }
    } else {
        Err(format!("Got wrong token type, ADDOP expected"))
    }
}

fn eval_mulop(tk: &ExpToken, lop: i64, rop: i64) -> Result<i64, String> {
    if tk.ttype == TokenType::MULOP {
        match &tk.txt {
            Some(operator) => match operator.as_str() {
                "*" => Ok(lop * rop),
                "/" => Ok(lop / rop),
                _ => Err(format!("Wrong operator text (should not happen).")),
            },
            None => Err(format!("No operator text (should not happen).")),
        }
    } else {
        Err(format!("Got wrong token type, MULOP expected"))
    }
}


#[cfg(test)]
mod tests {
    use crate::expressions::evaluator::eval;
    use crate::expressions::parser::ExpParser;
    use crate::expressions::lexer::ExpToken;

   #[test]
   fn test_eval_single_value_ok() {
       let text = "1";
       let mut parser = ExpParser::<std::vec::IntoIter<ExpToken>>::from_str(text);
       let tree = parser.parse_tree();
       assert!(tree.is_some());
       let value = eval(&tree.unwrap());
       assert_eq!(Ok(1), value)
   }

   #[test]
   fn test_eval_binary_addop_value_ok() {
       let text = "41 - 51";
       let mut parser = ExpParser::<std::vec::IntoIter<ExpToken>>::from_str(text);
       let tree = parser.parse_tree();
       assert!(tree.is_some());
       let value = eval(&tree.unwrap());
       assert_eq!(Ok(-10), value)
   }
   
   #[test]
   fn test_eval_terary_mix_value_ok() {
       let text = "4 / 2 + 40";
       let mut parser = ExpParser::<std::vec::IntoIter<ExpToken>>::from_str(text);
       let tree = parser.parse_tree();
       assert!(tree.is_some());
       let value = eval(&tree.unwrap());
       assert_eq!(Ok(42), value)
   }

   #[test]
   fn test_eval_long_mix_value_ok() {
       let text = "11 - 10 * 1 + 4 / 2 * 3 + 10 / 40";
       let mut parser = ExpParser::<std::vec::IntoIter<ExpToken>>::from_str(text);
       let tree = parser.parse_tree();
       assert!(tree.is_some());
       let value = eval(&tree.unwrap());
       assert_eq!(Ok(7), value)
   }
}
