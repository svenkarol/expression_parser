use crate::expressions::lexer::tokenize;
use crate::expressions::lexer::ExpToken;
use crate::expressions::lexer::TokenType;
use std::iter::Peekable;

/// A parser of simple arithmetic expressions using an [Iterator] source.
pub struct ExpParser<T: Iterator<Item = ExpToken>> {
    source: Peekable<T>,
}

pub struct Node {
    children: Vec<Tree>,
    kind: NodeType,
}

impl Node {
    pub const fn new(kind: NodeType) -> Node {
        Node {
            children: Vec::new(),
            kind: kind,
        }
    }

    pub fn add_child(&mut self, new_child: Tree) {
        self.children.push(new_child);
    }
}

pub enum Tree {
    Node(Node),
    Leaf(ExpToken),
}

pub enum NodeType {
    Start,
    Exp,
    Expp,
    Term,
    Termp,
}

impl<T: Iterator<Item = ExpToken>> ExpParser<T> {
    /// Creates a fresh instance from the given iterator.
    pub fn new(source: Peekable<T>) -> Self {
        Self { source }
    }

    /// Performs the parse. Currently this only checks whether the content is an expression or not.
    pub fn parse(&mut self) -> bool {
        self.start().is_some()
    }

    /// Parses the grammar's 'start' symbol and builds a corresponding syntax tree.
    /// Start -> Exp
    fn start(&mut self) -> Option<Tree> {
        match self.match_consume(TokenType::INIT) {
            Some(_) => {
                let mut start = Node::new(NodeType::Start);
                match self.exp() {
                    Some(exp_child) => {
                        start.add_child(exp_child);
                        match self.match_consume(TokenType::END) {
                            Some(_) => Some(Tree::Node(start)),
                            None => None,
                        }
                    }
                    None => None,
                }
            }
            None => None,
        }
    }

    /// Exp -> Term Exp′
    fn exp(&mut self) -> Option<Tree> {
        match self.term() {
            Some(term) => match self.expp() {
                Some(expp) => {
                    let mut exp = Node::new(NodeType::Exp);
                    exp.add_child(term);
                    exp.add_child(expp);
                    Some(Tree::Node(exp))
                }
                None => None,
            },
            None => None,
        }
    }

    /// Term → Num Term′
    fn term(&mut self) -> Option<Tree> {
        match self.match_consume(TokenType::NUM) {
            Some(tk_num) => match self.termp() {
                Some(termp) => {
                    let mut term = Node::new(NodeType::Term);
                    term.add_child(Tree::Leaf(tk_num));
                    term.add_child(termp);
                    Some(Tree::Node(term))
                }
                None => None,
            },
            None => None,
        }
    }

    /// Exp′ -> '–' Term Exp′
    /// Exp′ -> '+' Term Exp′
    /// Exp′ -> 𝜖
    fn expp(&mut self) -> Option<Tree> {
        match self.match_consume(TokenType::ADDOP) {
            Some(tk_add) => match self.term() {
                Some(term) => match self.expp() {
                    Some(expp) => {
                        let mut this_expp = Node::new(NodeType::Expp);
                        this_expp.add_child(Tree::Leaf(tk_add));
                        this_expp.add_child(term);
                        this_expp.add_child(expp);
                        Some(Tree::Node(this_expp))
                    }
                    None => None,
                },
                None => None,
            },
            None => Some(Tree::Node(Node::new(NodeType::Expp))), // Epsilon
        }
    }

    /// Term′ -> '∗' Num Term′
    /// Term′ -> '/' Num Term′
    /// Term′ -> 𝜖
    fn termp(&mut self) -> Option<Tree> {
        match self.match_consume(TokenType::MULOP) {
            Some(tk_mul) => match self.match_consume(TokenType::NUM) {
                Some(tk_num) => match self.termp() {
                    Some(termp) => {
                        let mut this_termp = Node::new(NodeType::Termp);
                        this_termp.add_child(Tree::Leaf(tk_mul));
                        this_termp.add_child(Tree::Leaf(tk_num));
                        this_termp.add_child(termp);
                        Some(Tree::Node(this_termp))
                    }
                    None => None,
                },
                None => None,
            },
            None => Some(Tree::Node(Node::new(NodeType::Expp))), // Epsilon
        }
    }

    /// Checks if the next token corresponds to ttype while skipping though white space and
    /// unknown characters in the token source. Returns immutable reference to the matched
    /// token or None if a token could not be matched.
    fn match_token(&mut self, ttype: TokenType) -> Option<&ExpToken> {
        let mut peek = self.source.peek();

        while let Some(tok) = peek {
            if tok.ttype == TokenType::UNKNOWN || tok.ttype == TokenType::WS {
                _ = self.source.next();
                peek = self.source.peek();
            } else {
                break;
            }
        }

        self.source.peek().filter(|next| next.ttype == ttype)
    }

    /// Consumes (takes) the next token from the token source and returns it. If no such token
    /// is available, None is returned instead.
    fn consume(&mut self) -> Option<ExpToken> {
        self.source.next()
    }

    /// Combines match and consume in a single operation.
    fn match_consume(&mut self, ttype: TokenType) -> Option<ExpToken> {
        let tk = self.match_token(ttype);
        if tk.is_some() {
            self.consume()
        } else {
            None
        }
    }
}

/// Driver function that takes a string slice, tokenizes the slice using [super::lexer::ExpLexer],
/// obtaining a [Vec] of [ExpToken], instantiating an [ExpParser] and performing the parse.
pub fn parse(text: &str) -> bool {
    let tokens = tokenize(text);
    let it = tokens.into_iter().peekable();
    let mut parser = ExpParser::new(it);
    parser.parse()
}

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
                if node.children.len() == 1 {
                    eval(&node.children[0])
                } else {
                    Err(format!(
                        "Expected 1 child of start, found {}.",
                        node.children.len()
                    ))
                }
            }
            _ => Ok(2),
        },
    }
}

#[cfg(test)]
mod tests {
    use crate::expressions::parser::parse;

    #[test]
    fn test_parse_ok() {
        let text = " 4 + 6 * 4 / 10";
        let result = parse(text);
        assert_eq!(result, true);
    }

    #[test]
    fn test_parse_fail() {
        let text = " 4 ++ 6 * 4 / 10";
        let result = parse(text);
        assert_eq!(result, false);
    }

    #[test]
    fn test_parse_empty_fail() {
        let text = "";
        let result = parse(text);
        assert_eq!(result, false);
    }

    #[test]
    fn test_parse_ws_fail() {
        let text = "       \n\t  ";
        let result = parse(text);
        assert_eq!(result, false);
    }

    #[test]
    fn test_parse_number_ok() {
        let text = " 42 ";
        let result = parse(text);
        assert_eq!(result, true);
    }

    #[test]
    fn test_parse_mulop_fail() {
        let text = " - ";
        let result = parse(text);
        assert_eq!(result, false);
    }

    #[test]
    fn test_parse_mulop_one_arg_fail() {
        let text = " -4 ";
        let result = parse(text);
        assert_eq!(result, false);
    }

    #[test]
    fn test_parse_ops_two_args_fail() {
        let text = "4 *-4 ";
        let result = parse(text);
        assert_eq!(result, false);
    }

    #[test]
    fn test_parse_addop_two_args_ok() {
        let text = "4 -4 ";
        let result = parse(text);
        assert_eq!(result, true);
    }
}
