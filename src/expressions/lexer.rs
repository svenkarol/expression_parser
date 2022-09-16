#[derive(Debug)]
#[derive(PartialEq)]
pub enum Token {
    OP(String, i32),
    NUM(String, i32),
    WS(String, i32),
    UNKNOWN(String, i32),
    INIT
}

#[derive(PartialEq)]
enum State { WS, NUM, OP, INIT, UNKNOWN }

pub fn tokenize(text: &str) -> Vec<Token> {
    let mut tokens:Vec<Token> = Vec::new();
    let mut state = State::INIT;
    let mut start_pos: i32 = -1;
    let mut current = String::new();
    let mut chars = text.chars().enumerate().peekable();

    for (i, ch) in &mut chars {
        match ch {
            '*' | '/' | '+' | '-' => {
                tokens.push(state_to_token(&state, current, start_pos));
                start_pos = i as i32;
                current = String::new();
                current.push(ch);
                state = State::OP;
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
    if chars.peek().is_none() {
        tokens.push(state_to_token(&state, current, start_pos));
    }
    tokens
}

fn state_to_token(state: &State, val: String, pos: i32) -> Token {
    match state {
        State::INIT => Token::INIT,
        State::OP => Token::OP(val, pos),
        State::WS => Token::WS(val, pos),
        State::NUM => Token::NUM(val, pos),
        State::UNKNOWN => Token::UNKNOWN(val, pos)
    }
}

#[cfg(test)]
mod tests {
    use crate::expressions::lexer::tokenize;
    use crate::expressions::lexer::Token;

    #[test]
    fn test_tokenize() {
        let text = " 4 +   534 * ac 4";
        let tokens_expected = vec![
            Token::INIT, 
            Token::WS(" ".to_string(), 0), 
            Token::NUM("4".to_string(), 1),
            Token::WS(" ".to_string(), 2),
            Token::OP("+".to_string(), 3),
            Token::WS("   ".to_string(), 4),
            Token::NUM("534".to_string(), 7),
            Token::WS(" ".to_string(), 10),
            Token::OP("*".to_string(), 11),
            Token::WS(" ".to_string(), 12),
            Token::UNKNOWN("ac".to_string(), 13),
            Token::WS(" ".to_string(), 15),
            Token::NUM("4".to_string(), 16)];
        let tokens_computed = tokenize(text);
        assert_eq!(tokens_computed, tokens_expected);
    }
}