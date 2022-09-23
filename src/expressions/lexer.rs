use std::io::{BufReader, Read, Result};
use utf8_chars::BufReadCharsExt;

#[derive(Debug)]
#[derive(PartialEq)]
/// Represents tokens recognized by a lexical analyser. Used by the [Lexer] trait.
pub struct Token<T> {
    pub ttype : T,
    pub txt : Option<String>,
    pub pos : Option<i32>
}

#[derive(Debug)]
#[derive(PartialEq)]
/// Kind of tokens that are recognized by an [ExpLexer].
pub enum TokenType { WS, NUM, ADDOP, MULOP, INIT, UNKNOWN, END}

pub type ExpToken = Token<TokenType>;

#[derive(PartialEq)]
enum State { WS, NUM, ADDOP, MULOP, INIT, UNKNOWN }

/// The expression lexer that tokenizes a given [BufReader], e.g., bytes of a [String] or a File.
pub struct ExpLexer<T> {  
    stream : BufReader<T>,
    token_pos : i32,
    next : Option<ExpToken>,
    peek : Result<Option<char>>
}

/// Defaul impl of [ExpLexer] for [&[u8]]
impl ExpLexer<&[u8]> {
    
    /// Constructs an [ExpLexer] from a given [str] slice.
    pub fn from_str(text: &str) -> ExpLexer<&[u8]> {
        let stream = BufReader::new(text.as_bytes());
        ExpLexer::new(stream)
    }

    /// Recognizes all expression tokens in the [Lexer]'s and creates a corresponding [Vec]tor of [ExpToken]s. 
    pub fn tokenize(&mut self) -> Vec<ExpToken> {
        let mut tokens:Vec<ExpToken> = Vec::new();
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

/// Default impl of the [ExpLexer] that provides a constructor function.
impl<T> ExpLexer<T> where T: Read, T: BufReadCharsExt {
    
    /// Creates a fresh [ExpLexer] object from the given [BufReader].
    pub fn new(stream: BufReader<T>) -> ExpLexer<T> {
        ExpLexer { 
            stream: stream,
            token_pos: -1, 
            next: None,
            peek: Ok(None)
         }
    }
}

/// A very basic lexer API that provides two functions for matching (i.e. comparing)
/// an expected token to the next token and for taking the next token from an underlying
/// input source.
pub trait Lexer<T> {

    /// Matches the next token in the input and compares with the given ttype. If the type
    /// is equal to the matched type, the function borrows a reference to the matched token. The 
    /// token remains in the ownership of the lexer. If the types do not match, just &[None] is 
    /// returned. If no more tokens are available, the function returns an END token.
    fn match_token(&mut self, ttype: &T) -> &Option<Token<T>>;

    /// Matches the next token in the input stream and returns the matched token, transferring ownership
    /// to the caller. If no more tokens are available, an END token is returned.
    fn take_token(&mut self) -> Option<Token<T>>;
}

/// This is an internal helper-trait that handles the actual token recognition in a
/// reusable way.
trait ExpLexerHelpers<T> {
    fn next_token(&mut self) -> &Option<ExpToken>;
}

/// The actual implementation of the token recognition.
impl<T> ExpLexerHelpers<T> for ExpLexer<T> 
    where T: Read, T: BufReadCharsExt {
    
    /// Implements the actual recognition of expression tokens.
    fn next_token(&mut self) -> &Option<ExpToken> { 

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

/// Implements the generic [Lexer] API for the [ExpLexer].
impl<T> Lexer<TokenType> for ExpLexer<T> 
    where T: Read, T: BufReadCharsExt {

    /// Matches the next token in the input and compares with the given &[TokenType]. If the type
    /// is equal to the matched type, the function borrows a reference to the matched token. The 
    /// token remains in the ownership of the lexer. If the types do not match, just &[None] is 
    /// returned. If no more tokens are available, the next token will be of type [TokenType::END].
    fn match_token(&mut self, ttype: &TokenType) -> &Option<ExpToken> {
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

    /// Matches the next token in the input stream and returns the matched token, transferring ownership
    /// to the caller. If no more tokens are available, the next token will be of type [TokenType::END].
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

/// Takes a given [str] slice and returns a [vec] of [ExpToken]s.
pub fn tokenize(text: &str) -> Vec<ExpToken> {
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