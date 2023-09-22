#![crate_type = "proc-macro"]
extern crate proc_macro;
use peekmore::PeekMore;
use peekmore::PeekMoreIterator;
use proc_macro::token_stream::IntoIter;
use proc_macro::Ident;
use proc_macro::Punct;
use proc_macro::TokenStream;
use proc_macro::TokenTree;
use std::iter::Peekable;
use proc_macro::Spacing;

struct Grammar {
    rules: Vec<Rule>,
}

struct Rule {
    lhs: Ident,
    rhs: RhsElement,
}

struct NonTerminal {
    name: Ident,
    member: Ident,
}

struct Terminal {
    name: Ident,
    member: Ident,
}

enum RhsElement {
    NonTerminal(NonTerminal),
    Terminal(Terminal),
}

#[proc_macro]
pub fn rast_ast(item: TokenStream) -> TokenStream {
    let mut i = 0;
    //let mut it = item.into_iter().collect::<Vec<TokenTree>>();
    let mut it: ParseInput = item.into_iter().peekmore();

    for tk in it {
        println!(
            "{}: {}\n",
            {
                let tmp = i;
                i += 1;
                tmp
            },
            tk
        );
    }
    "pub struct BinExp;".parse().unwrap()
}

const COLON: &[(char,Spacing)] = &[(':', proc_macro::Spacing::Alone)];
const GT: &[(char,Spacing)] = &[('>', proc_macro::Spacing::Alone)];
const LT: &[(char,Spacing)] = &[('<', proc_macro::Spacing::Alone)];

fn nonterminal(mut input: ParseInput) -> impl Fn(ParseInput) -> ParseResult<NonTerminal> {
    map(
        as_pair(as_pair(ident, match_punct(COLON)), ident),
        |((ident1, _), ident2)| NonTerminal {
            name: ident2,
            member: ident1,
        }, 
    )
}

fn terminal(mut input: ParseInput) -> impl Fn(ParseInput) -> ParseResult<Terminal> {
    map(
        as_pair(ident,as_pair(match_punct(LT),as_pair(ident, match_punct(GT)))),
        |(ident1,(_,(ident2,_)))| Terminal {
            name: ident2,
            member: ident1
        }
    )
}

fn ident(mut input: ParseInput) -> ParseResult<Ident> {
    if let Some(TokenTree::Ident(_)) = input.peek() {
        let Some(TokenTree::Ident(ident)) = input.next() else {
            panic!("This point should never be reached!");
        };
        Ok((input, ident))
    } else {
        Err(input)
    }
}

// https://bodil.lol/parser-combinators/
type ParseInput = PeekMoreIterator<IntoIter>;
type ParseResult<R> = Result<(ParseInput, R), ParseInput>;

fn match_punct(
    to_match: &'static [(char,Spacing)],
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
        }
        let mut result: Vec<TokenTree> = Vec::new();
        for i in 0..range.len() {
            result.insert(i, input.next().unwrap());
        }
        return Ok((input, result));
    }
}

fn as_pair<P1, P2, R1, R2>(first: P1, second: P2) -> impl Fn(ParseInput) -> ParseResult<(R1, R2)>
where
    P1: Fn(ParseInput) -> ParseResult<R1>,
    P2: Fn(ParseInput) -> ParseResult<R2>,
{
    move |input| {
        first(input).and_then(|(next_input, resval1)| match second(next_input) {
            Ok((next_input2, resval2)) => Ok((next_input2, (resval1, resval2))),
            Err(err) => Err(err),
        })
    }
}

fn map<P, F, R, Q>(parser: P, map_fn: F) -> impl Fn(ParseInput) -> ParseResult<Q>
where
    P: Fn(ParseInput) -> ParseResult<R>,
    F: Fn(R) -> Q,
{
    move |input| parser(input).map(|(next_input, parse_result)| (next_input, map_fn(parse_result)))
}

fn as_left<P1, P2, R1, R2>(first: P1, second: P2) -> impl Fn(ParseInput) -> ParseResult<R1>
where
    P1: Fn(ParseInput) -> ParseResult<R1>,
    P2: Fn(ParseInput) -> ParseResult<R2>,
{
    map(as_pair(first, second), |(left, _right)| left)
}

fn as_right<P1, P2, R1, R2>(first: P1, second: P2) -> impl Fn(ParseInput) -> ParseResult<R2>
where
    P1: Fn(ParseInput) -> ParseResult<R1>,
    P2: Fn(ParseInput) -> ParseResult<R2>,
{
    map(as_pair(first, second), |(_left, right)| right)
}

fn one_or_more<P, R>(parser: P) -> impl Fn(ParseInput) -> ParseResult<Vec<R>>
where
    P: Fn(ParseInput) -> ParseResult<R>,
{
    move |mut input| {
        let mut result: Vec<R> = Vec::new();
        match parser(input) {
            Ok((next_input, value)) => {
                result.push(value);
                input = next_input;
            }
            Err(next_input) => return Err(next_input),
        }

        loop {
            match parser(input) {
                Ok((next_input, value)) => {
                    result.push(value);
                    input = next_input;
                }
                Err(next_input) => return Ok((next_input, result)),
            }
        }
    }
}

fn zero_or_more<P, R>(parser: P) -> impl Fn(ParseInput) -> ParseResult<Vec<R>>
where
    P: Fn(ParseInput) -> ParseResult<R>,
{
    move |mut input| {
        let mut result: Vec<R> = Vec::new();
        loop {
            match parser(input) {
                Ok((next_input, value)) => {
                    result.push(value);
                    input = next_input;
                }
                Err(next_input) => return Ok((next_input, result)),
            }
        }
    }
}

/*fn pred_one<P,R,C>(parser: P) -> impl Fn(ParseInput) -> ParseResult<Vec<R>>
where
    P: Fn(ParseInput) -> ParseResult<R>,
    C: Fn(R) -> bool
{

}*/
