#[derive(Debug)]
#[derive(PartialEq)]
pub struct Token {
    pub ttype : TokenType,
    pub txt : Option<String>,
    pub pos : Option<i32>
}

#[derive(Debug)]
#[derive(PartialEq)]
pub enum TokenType { WS, NUM, ADDOP, MULOP, INIT, UNKNOWN, END}

#[derive(PartialEq)]
enum State { WS, NUM, ADDOP, MULOP, INIT, UNKNOWN }

pub fn tokenize(text: &str) -> Vec<Token> {
    let mut tokens:Vec<Token> = Vec::new();
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

fn state_to_token(state: &State, val: String, pos: i32) -> Token {
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