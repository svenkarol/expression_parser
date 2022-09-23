use crate::expressions::lexer::TokenType;
use crate::expressions::lexer::ExpToken;
use crate::expressions::lexer::tokenize;
use std::iter::Peekable;

/// A parser of simple arithmetic expressions using an [Iterator] source.
pub struct ExpParser<'a, T : Iterator<Item = &'a ExpToken>> {
    source: Peekable<T>
}

impl<'a, T : Iterator<Item = &'a ExpToken>> ExpParser<'a, T> {

    /// Creates a fresh instance from the given iterator.
    pub fn new(source: Peekable<T>) -> Self {
        Self { source }
    }
    
    /// Performs the parsing. Currently this only checks whether the content is an expression or not.
    pub fn parse(&mut self) -> bool {
        self.start().is_some()
    }

    /// Function that corresponds to the grammar's start symbol
    fn start(&mut self) -> Option<()> {      
        self.match_token(TokenType::INIT)
            .map(|_|())
            .and(self.consume().map(|_|()))
            .and(self.exp())
            .and(self.match_token(TokenType::END)).map(|_|())
            .and(self.consume().map(|_|()))
    }

    fn exp(&mut self) -> Option<()> { 
        self.term()
            .and_then(|_|self.expp())
    }

    fn term(&mut self) -> Option<()> {
        self.match_token(TokenType::NUM).map(|_|())
            .and(self.consume().map(|_|()))
            .and(self.termp())
    }

    fn expp(&mut self) -> Option<()> {
        if self.match_token(TokenType::ADDOP).is_some() {
            self.consume().map(|_|())
                .and(self.term())
                .and(self.expp())
        }
        else {
            Some(())
        }
    }

    fn termp(&mut self) -> Option<()> {
        if self.match_token(TokenType::MULOP).is_some() {
            self.consume().map(|_|Some(()))
                .and(
                    if self.match_token(TokenType::NUM).is_some() {
                        self.consume().map(|_|Some(()))
                            .and(self.termp())
                    }
                    else {
                        None
                    }
                )
        }
        else {
            Some(())
        }
    }

    fn match_token(&mut self, ttype: TokenType) -> Option<&&ExpToken> {
        let mut peek = self.source.peek();
    
        while let Some(tok) = peek {
            if tok.ttype == TokenType::UNKNOWN || tok.ttype == TokenType::WS {
                _ = self.source.next();
                peek = self.source.peek();
            }
            else {
                break;
            }
        }

        self.source
            .peek()
            .filter(|next| next.ttype == ttype)
    }

    fn consume(&mut self) -> Option<&ExpToken> { self.source.next() }

}

/// Driver function that takes a string slice, tokenizes the slice using [super::lexer::ExpLexer], 
/// obtaining a [Vec] of [ExpToken], instantiating an [ExpParser] and performing the parse.
pub fn parse(text: &str) -> bool {
    let tokens = tokenize(text);
    let peeker = tokens.iter().peekable();
    let mut parser = ExpParser :: new(peeker);
    parser.parse()
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