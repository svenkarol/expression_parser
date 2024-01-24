extern crate proc_macro;

use std::str::FromStr;

use peekmore::PeekMore;
use proc_macro2::TokenStream;
use proc_macro2::Ident;
use proc_macro2::TokenTree;
use super::combinators::{ParseResult, ParseInput, alt, as_pair, as_pair_opt, la_parser, la_pred, map, one_or_more};
use super::ast::{RhsElement, Terminal, Grammar, Rule, NonTerminal, LT, GT, COLON, ARROW, FollowedBy};

pub trait ParseInputHelpers {
    fn from_tkstream(stream: TokenStream) -> ParseInput;
}

impl ParseInputHelpers for ParseInput {
    fn from_tkstream(stream: TokenStream) -> Self {
        stream.into_iter().peekmore()
    }
}

impl FromStr for Grammar {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let stream: TokenStream = s.parse().unwrap();
        let grammar = grammar(ParseInput::from_tkstream(stream));
        match grammar {
            Ok((_, grammar_value)) => Ok(grammar_value),
            Err(_) => Err(()),
        }
    }
}

impl FromStr for Rule {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let stream: TokenStream = s.parse().unwrap();
        let rule = rule(ParseInput::from_tkstream(stream));
        match rule {
            Ok((_, rule_value)) => Ok(rule_value),
            Err(_) => Err(()),
        }
    }
}

pub fn grammar(input: ParseInput) -> ParseResult<Grammar> {
    let a: fn(&TokenTree) -> bool = |tt| match tt {
        &TokenTree::Ident(_) => true,
        _ => false,
    };
    let b: fn(&TokenTree) -> bool = |tt| match tt {
        &TokenTree::Punct(ref punct) => punct.as_char() == '-',
        _ => false,
    };
    let c: fn(&TokenTree) -> bool = |tt| match tt {
        &TokenTree::Punct(ref punct) => punct.as_char() == '>',
        _ => false,
    };
    let checkers: Vec<fn(&TokenTree) -> bool> = vec![a, b, c];

    let rule_test = la_pred(checkers);

    let grammar_parser = one_or_more(la_parser(rule, rule_test));
    map(grammar_parser, |rules_res| Grammar { rules: rules_res })(input)
}

pub fn rule(input: ParseInput) -> ParseResult<Rule> {
    let a: fn(&TokenTree) -> bool = |tt| match tt {
        &TokenTree::Ident(_) => true,
        _ => false,
    };
    let b: fn(&TokenTree) -> bool = |tt| match tt {
        &TokenTree::Punct(ref punct) => punct.as_char() == ':',
        _ => false,
    };
    let checkers: Vec<fn(&TokenTree) -> bool> = vec![a, b];

    let rhs_element_test = la_pred(checkers);

    let rhs = one_or_more(la_parser(rhselement, rhs_element_test));
    let lhs = ident;
    let arrow = match_punct(ARROW);
    let rule_parser = as_pair(lhs, as_pair(arrow, rhs));
    map(rule_parser, |(lhs_res, (_, rhs_res))| Rule {
        lhs: lhs_res,
        rhs: rhs_res,
    })(input)
}

pub fn nonterminal(input: ParseInput) -> ParseResult<NonTerminal> {
    map(
        as_pair(as_pair(ident, match_punct(COLON)), ident),
        |((ident1, _), ident2)| NonTerminal {
            name: ident2,
            member: ident1,
        },
    )(input)
}

pub fn terminal(input: ParseInput) -> ParseResult<Terminal> {
    map(
        as_pair(
            ident,
            as_pair(
                as_pair(as_pair(match_punct(COLON), match_punct(LT)), ident),
                match_punct(GT),
            ),
        ),
        |(ident1, ((_, ident2), _))| Terminal {
            name: ident2,
            member: ident1,
        },
    )(input)
}

fn try_nonterminal_child(
    input: ParseInput,
) -> ParseResult<((Ident, Vec<TokenTree /*COLON*/>), Option<NonTerminal>)> {
    map(
        as_pair_opt(as_pair(ident, match_punct(COLON)), ident),
        |((ident1, colon), ident2)| match ident2 {
            Some(ident2) => (
                (ident1.clone(), colon),
                Some(NonTerminal {
                    name: ident2,
                    member: ident1,
                }),
            ),
            None => ((ident1, colon), None),
        },
    )(input)
}

fn terminal_child(
    prefix: (Ident, Vec<TokenTree /*COLON*/>),
    input: ParseInput,
) -> ParseResult<Terminal> {
    match as_pair(match_punct(LT), as_pair(ident, match_punct(GT)))(input) {
        Ok((next_input, (_, (ident2, _)))) => Ok((
            next_input,
            Terminal {
                name: ident2,
                member: prefix.0,
            },
        )),
        Err(next_input) => Err(next_input),
    }
}

fn rhselement(input: ParseInput) -> ParseResult<RhsElement> {
    match alt(try_nonterminal_child, terminal_child)(input) {
        Ok((next_input, (alt1, alt2))) => {
            if let Some(nt) = alt1 {
                return Ok((next_input, RhsElement::NonTerminal(nt)));
            } else if let Some(tm) = alt2 {
                return Ok((next_input, RhsElement::Terminal(tm)));
            } else {
                return Err(next_input);
            }
        }
        Err(next_input) => return Err(next_input),
    }
}

pub fn ident(mut input: ParseInput) -> ParseResult<Ident> {
    if let Some(TokenTree::Ident(_)) = input.peek() {
        let Some(TokenTree::Ident(ident)) = input.next() else {
            panic!("This point should never be reached!");
        };
        Ok((input, ident))
    } else {
        Err(input)
    }
}

fn match_punct(
    to_match: &'static [(char, FollowedBy)],
) -> impl Fn(ParseInput) -> ParseResult<Vec<TokenTree>> {
    move |mut input| {
        let range = input.peek_range(0, to_match.len());

        for (is, expected) in range.iter().zip(to_match) {
            let Some(TokenTree::Punct(is_value)) = is else {
                return Err(input);
            };
            if is_value.as_char() != expected.0 {
                return Err(input);
            }
            match expected.1 {
                FollowedBy::Spacing(spacing) => {
                    if is_value.spacing() != spacing {
                        return Err(input);
                    }
                }
                FollowedBy::AnyToken => {}
            }
        }
        let mut result: Vec<TokenTree> = Vec::new();
        for i in 0..range.len() {
            result.insert(i, input.next().unwrap());
        }
        return Ok((input, result));
    }
}

#[cfg(test)]
mod tests {
    #[cfg(test)]
    use super::{
        grammar, ident, match_punct, nonterminal, rhselement, rule, terminal, terminal_child,
        ParseInput, ParseInputHelpers, ARROW,
    };
    #[cfg(test)]
    use crate::rast::astparser::{try_nonterminal_child, RhsElement, Grammar, Rule};
    #[cfg(test)]
    use proc_macro2::{Ident, Span, TokenTree};
    #[cfg(test)]
    use std::str::FromStr;
    
    #[test]
    fn test_rules_from_str_ok() {
        let str_input = "A -> x:B y:C";
        let result = Rule::from_str(str_input);
        assert!(result.is_ok());
        let rule = result.unwrap();
        println!("{}", rule.lhs.to_string());
        assert_eq!(rule.lhs.to_string().as_str(), "A");
        assert_eq!(rule.rhs.len(), 2);
        match &rule.rhs[0] {
            RhsElement::NonTerminal(nt) => {
                assert_eq!(nt.name.to_string().as_str(), "B");
                assert_eq!(nt.member.to_string().as_str(), "x");
            }
            _ => assert!(false),
        }
        match &rule.rhs[1] {
            RhsElement::NonTerminal(nt) => {
                assert_eq!(nt.name.to_string().as_str(), "C");
                assert_eq!(nt.member.to_string().as_str(), "y");
            }
            _ => assert!(false),
        }
    }
    
    #[test]
    fn test_grammar_from_str_ok() {
        let str_input = "
                Onion -> all:The stuff:<i42>
                Cheese -> foo:BarFoo bar:<int> baz:<string>";
        let grammar = Grammar::from_str(str_input);
        assert!(grammar.is_ok());
        assert_eq!(grammar.unwrap().rules.len(), 2);
    }

    #[test]
    fn test_grammar_2_rules_ok() {
        let str_input = "
                Onion -> all:The stuff:<i42>
                Cheese -> foo:BarFoo bar:<int> baz:<string>";
        let stream_input = str_input.parse().unwrap();
        let grammar = grammar(ParseInput::from_tkstream(stream_input));
        assert!(grammar.is_ok());
        let(mut input, grammar_value) = grammar.unwrap();
        assert_eq!(grammar_value.rules.len(), 2);
        assert!(input.peek().is_none());
    }

    
    #[test]
    fn test_grammar_2_rules_ident_left_ok() {
        let str_input = "
                Onion -> all:The stuff:<i42>
                Cheese -> foo:BarFoo bar:<int> baz:<string>
                Beacon
            ";
        let stream_input = str_input.parse().unwrap();
        let grammar = grammar(ParseInput::from_tkstream(stream_input));
        assert!(grammar.is_ok());
        let(mut input, grammar_value) = grammar.unwrap();
        assert_eq!(grammar_value.rules.len(), 2);
        assert!(input.peek().is_some());
        match input.next().unwrap() {
            TokenTree::Ident(ident) => assert_eq!(ident.to_string().as_str(), "Beacon"),
            _ => assert!(false),
        }
    }

    #[test]
    fn test_rule_1_nok() {
        let str_input = "Cheese -> foo:BarFoo Onion -> buzz:BarFoo";
        let stream_input = str_input.parse().unwrap();
        let rule = rule(ParseInput::from_tkstream(stream_input));
        assert!(rule.is_ok());
        let (mut next_input, rule_value) = rule.unwrap();
        assert_eq!(rule_value.lhs.to_string().as_str(), "Cheese");
        assert_eq!(rule_value.rhs.len(), 1);
        match &rule_value.rhs[0] {
            RhsElement::NonTerminal(nt) => {
                assert_eq!(nt.name.to_string().as_str(), "BarFoo");
                assert_eq!(nt.member.to_string().as_str(), "foo");
            }
            _ => assert!(false),
        }
        let next = next_input.peek();
        assert!(next.is_some());
        let next_value = next.unwrap();
        match next_value {
            TokenTree::Ident(ident) => assert_eq!(ident.to_string().as_str(), "Onion"),
            _ => assert!(false),
        }
    }

    #[test]
    fn test_rule_3_ok() {
        let str_input = "Cheese -> foo:BarFoo bar:<int> baz:<string>";
        let stream_input = str_input.parse().unwrap();
        let rule = rule(ParseInput::from_tkstream(stream_input));
        assert!(rule.is_ok());
        let (_, rule_value) = rule.unwrap();
        assert_eq!(rule_value.lhs.to_string().as_str(), "Cheese");
        assert_eq!(rule_value.rhs.len(), 3);
        match &rule_value.rhs[0] {
            RhsElement::NonTerminal(nt) => {
                assert_eq!(nt.name.to_string().as_str(), "BarFoo");
                assert_eq!(nt.member.to_string().as_str(), "foo");
            }
            _ => assert!(false),
        }
        match &rule_value.rhs[1] {
            RhsElement::Terminal(tm) => {
                assert_eq!(tm.name.to_string().as_str(), "int");
                assert_eq!(tm.member.to_string().as_str(), "bar");
            }
            _ => assert!(false),
        }
        match &rule_value.rhs[2] {
            RhsElement::Terminal(tm) => {
                assert_eq!(tm.name.to_string().as_str(), "string");
                assert_eq!(tm.member.to_string().as_str(), "baz");
            }
            _ => assert!(false),
        }
    }

    #[test]
    fn test_rhs_element_nonterminal() {
        let str_input = "foo:BarFoo";
        let stream: proc_macro2::TokenStream = str_input.parse().unwrap();
        let rhs_element = rhselement(ParseInput::from_tkstream(stream));
        assert!(rhs_element.is_ok());
        let (_, rhs_value) = rhs_element.unwrap();
        match rhs_value {
            RhsElement::Terminal(_) => assert!(false),
            RhsElement::NonTerminal(nonterminal) => {
                assert_eq!(nonterminal.name.to_string().as_str(), "BarFoo");
                assert_eq!(nonterminal.member.to_string().as_str(), "foo");
            }
        }
    }

    #[test]
    fn test_rhs_element_terminal() {
        let str_input = "bar:<string>";
        let stream: proc_macro2::TokenStream = str_input.parse().unwrap();
        let rhs_element = rhselement(ParseInput::from_tkstream(stream));
        assert!(rhs_element.is_ok());
        let (_, rhs_value) = rhs_element.unwrap();
        match rhs_value {
            RhsElement::Terminal(terminal) => {
                assert_eq!(terminal.name.to_string().as_str(), "string");
                assert_eq!(terminal.member.to_string().as_str(), "bar");
            }
            RhsElement::NonTerminal(_) => assert!(false),
        }
    }

    #[test]
    fn test_terminal_child() {
        let str_input = "<string>";
        let stream: proc_macro2::TokenStream = str_input.parse().unwrap();
        let prefix = (Ident::new("foo", Span::call_site()), Vec::new());
        let terminal = terminal_child(prefix, ParseInput::from_tkstream(stream));
        assert!(terminal.is_ok());
        let (_, terminal_value) = terminal.unwrap();
        assert_eq!(terminal_value.name.to_string().as_str(), "string");
        assert_eq!(terminal_value.member.to_string().as_str(), "foo");
    }

    #[test]
    fn test_try_nonterminal_child_terminal() {
        let str_input = "bar:<string>";
        let member_expected = "bar";
        let stream: proc_macro2::TokenStream = str_input.parse().unwrap();
        let rhs_element = try_nonterminal_child(ParseInput::from_tkstream(stream));
        assert!(rhs_element.is_ok());
        let rhs_value = rhs_element.unwrap();
        let (member, _) = rhs_value.1 .0;
        assert_eq!(member.to_string().as_str(), member_expected);
        assert!(rhs_value.1 .1.is_none());
    }

    #[test]
    fn test_try_nonterminal_child_nonterminal() {
        let str_input = "bar:Foobar";
        let member_expected = "bar";
        let stream: proc_macro2::TokenStream = str_input.parse().unwrap();
        let rhs_element = try_nonterminal_child(ParseInput::from_tkstream(stream));
        assert!(rhs_element.is_ok());
        let rhs_value = rhs_element.unwrap();
        let (member, _) = rhs_value.1 .0;
        assert_eq!(member.to_string().as_str(), member_expected);
        assert!(rhs_value.1 .1.is_some());
        let nt_value = rhs_value.1 .1.unwrap();
        assert_eq!(nt_value.name.to_string().as_str(), "Foobar");
    }

    #[test]
    fn test_terminal_spacing_ok() {
        let str_input = "foo : <string>";
        let name_expected = "string";
        let member_expected = "foo";
        let stream: proc_macro2::TokenStream = str_input.parse().unwrap();
        let terminal = terminal(ParseInput::from_tkstream(stream));
        assert!(terminal.is_ok());
        let (_, terminal_value) = terminal.unwrap();
        assert_eq!(terminal_value.name.to_string().as_str(), name_expected);
        assert_eq!(terminal_value.member.to_string().as_str(), member_expected);
    }

    #[test]
    fn test_terminal_nospacing_ok() {
        let str_input = "foo:<string>";
        let name_expected = "string";
        let member_expected = "foo";
        let stream: proc_macro2::TokenStream = str_input.parse().unwrap();
        let terminal = terminal(ParseInput::from_tkstream(stream));
        assert!(terminal.is_ok());
        let (_, nt_value) = terminal.unwrap();
        assert_eq!(nt_value.name.to_string().as_str(), name_expected);
        assert_eq!(nt_value.member.to_string().as_str(), member_expected);
    }

    #[test]
    fn test_nonterminal_ok() {
        let nt_str_input = "foo:Bar";
        let name_expected = "Bar";
        let member_expected = "foo";
        let stream: proc_macro2::TokenStream = nt_str_input.parse().unwrap();
        let nonterminal = nonterminal(ParseInput::from_tkstream(stream));
        assert!(nonterminal.is_ok());
        let (_, nt_value) = nonterminal.unwrap();
        assert_eq!(nt_value.name.to_string().as_str(), name_expected);
        assert_eq!(nt_value.member.to_string().as_str(), member_expected);
    }

    #[test]
    fn test_nonterminal_blank_ok() {
        let nt_str_input = "foo :  Bar";
        let name_expected = "Bar";
        let member_expected = "foo";
        let stream: proc_macro2::TokenStream = nt_str_input.parse().unwrap();
        let nonterminal = nonterminal(ParseInput::from_tkstream(stream));
        assert!(nonterminal.is_ok());
        let (_, nt_value) = nonterminal.unwrap();
        assert_eq!(nt_value.name.to_string().as_str(), name_expected);
        assert_eq!(nt_value.member.to_string().as_str(), member_expected);
    }

    #[test]
    fn test_ident_ok() {
        let ident_str = "foo";
        let tk_stream: proc_macro2::TokenStream = ident_str.parse().unwrap();
        let ident = ident(ParseInput::from_tkstream(tk_stream));
        assert!(ident.is_ok());
        let (_, ident_value) = ident.unwrap();
        assert_eq!(ident_value.to_string().as_str(), ident_str);
    }

    #[test]
    fn test_ident_err() {
        let ident_str = "42";
        let tk_stream: proc_macro2::TokenStream = ident_str.parse().unwrap();
        let ident = ident(ParseInput::from_tkstream(tk_stream));
        assert!(ident.is_err());
    }

    #[test]
    fn test_arrow_ok() {
        let arrow_str = "->";
        let tk_stream: proc_macro2::TokenStream = arrow_str.parse().unwrap();
        let arrow = match_punct(ARROW)(ParseInput::from_tkstream(tk_stream));
        assert!(arrow.is_ok());
        let (_, arrow_value) = arrow.unwrap();
        assert_eq!(2, arrow_value.len());
    }

    #[test]
    fn test_arrow_spacing_ok() {
        let arrow_str = "- >";
        let tk_stream: proc_macro2::TokenStream = arrow_str.parse().unwrap();
        let arrow = match_punct(ARROW)(ParseInput::from_tkstream(tk_stream));
        assert!(arrow.is_err());
    }

    #[test]
    fn test_arrow_err() {
        let arrow_str = ">-";
        let tk_stream: proc_macro2::TokenStream = arrow_str.parse().unwrap();
        let arrow = match_punct(ARROW)(ParseInput::from_tkstream(tk_stream));
        assert!(arrow.is_err());
    }
}
