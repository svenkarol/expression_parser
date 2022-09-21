use std::io::{BufReader, Read, BufRead, Result};
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

#[derive(PartialEq)]
enum State { WS, NUM, ADDOP, MULOP, INIT, UNKNOWN }

pub struct ExpLexer<T> {  
    stream : BufReader<T>,
    token_pos : i32,
    next : Option<Token<TokenType>>,
    peek : Result<Option<char>>
}

impl<T> ExpLexer<T> {
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
    fn match_token(&mut self, ttype: T) -> &Option<Token<T>>;

    fn consume(&mut self) -> Option<Token<T>>;
}

trait ExpLexerHelpers<T> {
    fn next_token(&mut self) -> &Option<Token<TokenType>>;
}

impl<T> ExpLexerHelpers<T> for ExpLexer<T> 
    where T: Read, T: BufReadCharsExt {
    
    fn next_token(&mut self) -> &Option<Token<TokenType>> { 
        
        if let Some(Token { ttype: TokenType::END, txt:_, pos:_ }) = &self.next {
            return &self.next;
        }

        if let Ok(None) = self.peek {
            self.peek = self.stream.read_char()
        }

        let mut current = String::new();
        let mut token:Option<Token<TokenType>> = None;
        let mut state = State::INIT;
        let token_pos = self.token_pos;
        
        loop {
            if let Ok(Some(ch)) = self.peek {
                self.peek = self.stream.read_char();
                self.token_pos += 1;
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
            }
            else if let Ok(None) = self.peek {
                if state != State::INIT  {
                    token = Some(state_to_token(&state, current, token_pos)); 
                    break;
                }
                else if let Some(Token{ttype, txt:_, pos:_}) = &self.next {
                    token = match ttype {
                        TokenType::END => None,
                        _ => Some(Token { ttype: TokenType::END, txt: None, pos: None })
                    }
                }
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
    where T: Read, T: BufReadCharsExt, T: ExpLexerHelpers<T> {

    fn match_token(&mut self, ttype: TokenType) -> &Option<Token<TokenType>> {
        if self.next.is_none() && self.peek.is_ok() {
            self.next_token();
        }

        if let Some(tok) = &self.next {
            if tok.ttype == ttype {
                return &self.next
            }
        }

        &None
    }

    fn consume(&mut self) -> Option<Token<TokenType>> {
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
    let mut tokens:Vec<Token<TokenType>> = Vec::new();
    let mut state = State::INIT;
    let mut start_pos: i32 = -1;
    let mut current = String::new();
    let mut chars = text.chars().enumerate().peekable();

    for (i, ch) in &mut chars {
        match ch {
            '+' | '-' => {
                tokens.push(state_to_token(&state, current, start_pos));
                start_pos = i as i32;
                current = String::new();
                current.push(ch);
                state = State::ADDOP;
            },
            '*' | '/'  => {
                tokens.push(state_to_token(&state, current, start_pos));
                start_pos = i as i32;
                current = String::new();
                current.push(ch);
                state = State::MULOP;
            },
            '0' ..= '9' => {
                if state == State::NUM {
                    current.push(ch);
                }
                else {
                    tokens.push(state_to_token(&state, current, start_pos));
                    start_pos = i as i32;
                    current = String::new();
                    current.push(ch);
                    state = State::NUM;
                }
            },
            '\r' | '\n' | ' ' | '\t' => {
                if state == State::WS {
                    current.push(ch);
                }
                else {
                    tokens.push(state_to_token(&state, current, start_pos));
                    start_pos = i as i32;
                    current = String::new();
                    current.push(ch);
                    state = State::WS;
                }
            },
            _ => {
                if state == State::UNKNOWN {
                    current.push(ch);
                }
                else {
                    tokens.push(state_to_token(&state, current, start_pos));
                    start_pos = i as i32;
                    current = String::new();
                    current.push(ch);
                    state = State::UNKNOWN;    
                }
            }
        }
    }    
    tokens.push(state_to_token(&state, current, start_pos));
    tokens.push(Token { ttype: TokenType::END, txt: None, pos: None });
    tokens
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
    use crate::expressions::lexer::tokenize;
    use crate::expressions::lexer::Token;
    use crate::expressions::lexer::TokenType;

    #[test]
    fn test_tokenize() {
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
        let tokens_computed = tokenize(text);
        assert_eq!(tokens_computed, tokens_expected);
    }
}