use std::io::{BufReader, Read, Result};
use utf8_chars::BufReadCharsExt;

#[derive(Debug)]
#[derive(PartialEq)]
pub struct Token<T> {
    pub ttype : T,
    pub txt : Option<String>,
    pub pos : Option<i32>
}

#[derive(Debug)]
#[derive(PartialEq)]
pub enum TokenType { WS, NUM, ADDOP, MULOP, INIT, UNKNOWN, END}

pub type ExpToken = Token<TokenType>;

#[derive(PartialEq)]
enum State { WS, NUM, ADDOP, MULOP, INIT, UNKNOWN }

pub struct ExpLexer<T> {  
    stream : BufReader<T>,
    token_pos : i32,
    next : Option<Token<TokenType>>,
    peek : Result<Option<char>>
}

impl ExpLexer<&[u8]> {
    
    pub fn from_str(text: &str) -> ExpLexer<&[u8]> {
        let stream = BufReader::new(text.as_bytes());
        ExpLexer::new(stream)
    }

    pub fn tokenize(&mut self) -> Vec<Token<TokenType>> {
        let mut tokens:Vec<Token<TokenType>> = Vec::new();
        tokens.push(state_to_token(&State::INIT, String::new(), -1));
 
        while let Some(token) = self.take_token() {
            match &token.ttype {
                &TokenType::END => {
                    tokens.push(token);
                    break;
                }
                _ => { 
                    tokens.push(token);
                 }
            }
        }
        tokens
    }

}

impl<T> ExpLexer<T> where T: Read, T: BufReadCharsExt {
    
    pub fn new(stream: BufReader<T>) -> ExpLexer<T> {
        ExpLexer { 
            stream: stream,
            token_pos: -1, 
            next: None,
            peek: Ok(None)
         }
    }

   
}

pub trait Lexer<T> {
    fn match_token(&mut self, ttype: &T) -> &Option<Token<T>>;

    fn take_token(&mut self) -> Option<Token<T>>;
}

trait ExpLexerHelpers<T> {
    fn next_token(&mut self) -> &Option<Token<TokenType>>;
}

impl<T> ExpLexerHelpers<T> for ExpLexer<T> 
    where T: Read, T: BufReadCharsExt {
    
    fn next_token(&mut self) -> &Option<Token<TokenType>> { 

        if let Ok(None) = self.peek {
            self.peek = self.stream.read_char();
            self.token_pos += 1;
        }

        let mut current = String::new();
        let mut token:Option<Token<TokenType>> = None;
        let mut state = State::INIT;
        let token_pos = self.token_pos;
        
        loop {
            if let Ok(Some(ch)) = self.peek {
                match ch {
                    '+' | '-' => {
                        if state == State::INIT {
                            state = State::ADDOP;
                            current = String::new();
                            current.push(ch);
                        }
                        else {
                            token = Some(state_to_token(&state, current, token_pos));
                            break;
                        }
                    },
                    '*' | '/'  => {
                        if state == State::INIT {
                            state = State::MULOP;
                            current.push(ch);
                        }
                        else {
                            token = Some(state_to_token(&state, current, token_pos));
                            break;
                        }
                    },
                    '0' ..= '9' => {
                        if state == State::INIT {
                            state = State::NUM;
                            current.push(ch);
                        }
                        else if state == State::NUM {
                            current.push(ch);
                        }
                        else {
                            token = Some(state_to_token(&state, current, token_pos));
                            break;
                        }
                    },
                    '\r' | '\n' | ' ' | '\t' => {
                        if state == State::INIT {
                            state = State::WS;
                            current.push(ch);
                        }
                        else if state == State::WS {
                            current.push(ch);
                        }
                        else {
                            token = Some(state_to_token(&state, current, token_pos));
                            break;
                        }
                    },
                    _ => {
                        if state == State::INIT {
                           state = State::UNKNOWN;
                           current.push(ch);
                        }
                        else if state == State::UNKNOWN {
                           current.push(ch);
                        }
                        else {
                           token = Some(state_to_token(&state, current, token_pos)); 
                           break;
                        }
                    }
                }
                self.peek = self.stream.read_char();
                self.token_pos += 1;
            }
            else if let Ok(None) = self.peek {
                if state != State::INIT  {
                    token = Some(state_to_token(&state, current, token_pos)); 
                }
                else {
                    token = Some(Token { ttype: TokenType::END, txt: None, pos: None })  
                }
                break;
            }
            else {
                break;
            }
        }
        self.next = token;
        &self.next
    }
}

impl<T> Lexer<TokenType> for ExpLexer<T> 
    where T: Read, T: BufReadCharsExt {

    fn match_token(&mut self, ttype: &TokenType) -> &Option<Token<TokenType>> {
        if self.next.is_none() && self.peek.is_ok() {
            self.next_token();
        }

        if let Some(tok) = &self.next {
            if tok.ttype.eq(ttype) {
                return &self.next
            }
        }

        &None
    }

    fn take_token(&mut self) -> Option<Token<TokenType>> {
        if self.next.is_none() && self.peek.is_ok() {
            self.next_token();
        }
        
        if self.next.is_some() {
            let mut_temp = self.next.take();
            self.next_token();
            return mut_temp;
        }
        
        None
    }
}

pub fn tokenize(text: &str) -> Vec<Token<TokenType>> {
    ExpLexer::from_str(text).tokenize()
}

fn state_to_token(state: &State, val: String, pos: i32) -> Token<TokenType> {
    match state {
        State::INIT     => Token { ttype: TokenType::INIT, txt: None, pos: None },
        State::ADDOP    => Token { ttype: TokenType::ADDOP, txt: Some(val), pos: Some(pos) },
        State::MULOP    => Token { ttype: TokenType::MULOP, txt: Some(val), pos: Some(pos) },
        State::WS       => Token { ttype: TokenType::WS, txt: Some(val), pos: Some(pos)},
        State::NUM      => Token { ttype: TokenType::NUM, txt: Some(val), pos: Some(pos)},
        State::UNKNOWN  => Token { ttype: TokenType::UNKNOWN, txt: Some(val), pos: Some(pos)}
    }
}

#[cfg(test)]
mod tests {
    use super::{Token, TokenType, ExpLexer, Lexer};
    use std::{io::{BufReader}};
    use pretty_assertions::{assert_eq};

    #[test]
    fn test_explexer_match_empty_string() {
        let text = "";
        let stream = BufReader::new(text.as_bytes());
        let mut lexer = ExpLexer::new(stream);
        
        let token1_matched = lexer.match_token(&TokenType::END);
        assert!(token1_matched.is_some());
        let token1_consumed = lexer.take_token();
        assert!(token1_consumed.is_some());
        assert_eq!(token1_consumed.expect("Should not happen.").ttype, TokenType::END);
        
        let token2_matched = lexer.match_token(&TokenType::END);
        assert!(token2_matched.is_some());
        let token2_consumed = lexer.take_token();
        assert!(token2_consumed.is_some());
        assert_eq!(&token2_consumed.expect("Should not happen.").ttype, &TokenType::END);
    }

    fn test_explexer_match_str(match_str: &str, ttype: &TokenType) {
        let stream = BufReader::new(match_str.as_bytes());
        let mut lexer = ExpLexer::new(stream);
        
        let token1_matched = lexer.match_token(ttype);
        assert!(token1_matched.is_some());
       
        let token1_consumed = lexer.take_token();
        assert!(token1_consumed.is_some());
        
        let token1_val = token1_consumed.expect("Should not happen.");
        assert_eq!(&token1_val.ttype, ttype);
        assert!(token1_val.txt.is_some());
        let text_token = token1_val.txt.as_ref().expect("Should not happen!");
        assert_eq!(text_token, match_str);

        let token2_matched = lexer.match_token(&TokenType::END);
        assert!(token2_matched.is_some());
    }

    #[test]
    fn test_explexer_match_ws_string() {
        test_explexer_match_str("   ", &TokenType::WS)
    }

    #[test]
    fn test_explexer_match_num_string() {
        test_explexer_match_str("1234567", &TokenType::NUM)
    }

    #[test]
    fn test_explexer_match_unknown_string() {
        test_explexer_match_str("%$§&)asdasd", &TokenType::UNKNOWN)
    }

    #[test]
    fn test_explexer_match_addop_string() {
        test_explexer_match_str("-", &TokenType::ADDOP)
    }

    #[test]
    fn test_explexer_match_mulop_string() {
        test_explexer_match_str("/", &TokenType::MULOP)
    }

    #[test]
    fn test_tokenize_long_expression_ok() {
        let text = " 4 +   534 * ac 4";
        let tokens_expected = vec![
            Token { ttype: TokenType::INIT, txt: None, pos: None },  
            Token { ttype: TokenType::WS, txt: Some(" ".to_string()), pos: Some(0) }, 
            Token { ttype: TokenType::NUM, txt: Some("4".to_string()), pos: Some(1) }, 
            Token { ttype: TokenType::WS, txt: Some(" ".to_string()), pos: Some(2) }, 
            Token { ttype: TokenType::ADDOP, txt: Some("+".to_string()), pos: Some(3) },
            Token { ttype: TokenType::WS, txt: Some("   ".to_string()), pos: Some(4) }, 
            Token { ttype: TokenType::NUM, txt: Some("534".to_string()), pos: Some(7) }, 
            Token { ttype: TokenType::WS, txt: Some(" ".to_string()), pos: Some(10) }, 
            Token { ttype: TokenType::MULOP, txt: Some("*".to_string()), pos: Some(11) }, 
            Token { ttype: TokenType::WS, txt: Some(" ".to_string()), pos: Some(12) }, 
            Token { ttype: TokenType::UNKNOWN, txt: Some("ac".to_string()), pos: Some(13) },
            Token { ttype: TokenType::WS, txt: Some(" ".to_string()), pos: Some(15) },
            Token { ttype: TokenType::NUM, txt: Some("4".to_string()), pos: Some(16) },
            Token { ttype: TokenType::END, txt: None, pos: None }] ;
        let tokens_computed = ExpLexer::from_str(text).tokenize();
        assert_eq!(tokens_computed, tokens_expected);
    }

    #[test]
    fn test_tokenize_empty_expression() {
        let text = "";
        let tokens_expected = vec![
            Token { ttype: TokenType::INIT, txt: None, pos: None },
            Token { ttype: TokenType::END, txt: None, pos: None }] ;
        let tokens_computed = ExpLexer::from_str(text).tokenize();
        assert_eq!(tokens_computed, tokens_expected);
    }
}