#![crate_type = "proc-macro"]
extern crate proc_macro;
use peekmore::PeekMore;
use proc_macro::TokenStream;

#[proc_macro]
pub fn rast_ast(item: TokenStream) -> TokenStream {
    let mut i = 0;
    //let mut it = item.into_iter().collect::<Vec<TokenTree>>();
    let mut it: astparser::ParseInput = proc_macro2::TokenStream::from(item).into_iter().peekmore();

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

mod astparser {
    extern crate proc_macro;
    use peekmore::PeekMore;
    use peekmore::PeekMoreIterator;
    use proc_macro2::token_stream::IntoIter;
    use proc_macro2::Ident;
    use proc_macro2::Punct;
    use proc_macro2::Spacing;
    use proc_macro2::TokenStream;
    use proc_macro2::TokenTree;
    use std::iter::Peekable;
    pub struct Grammar {
        rules: Vec<Rule>,
    }

    pub struct Rule {
        lhs: Ident,
        rhs: Vec<RhsElement>,
    }

    pub struct NonTerminal {
        name: Ident,
        member: Ident,
    }

    pub struct Terminal {
        name: Ident,
        member: Ident,
    }

    pub enum RhsElement {
        NonTerminal(NonTerminal),
        Terminal(Terminal),
    }

    const COLON: &[(char, Spacing)] = &[(':', proc_macro2::Spacing::Alone)];
    const GT: &[(char, Spacing)] = &[('>', proc_macro2::Spacing::Alone)];
    const LT: &[(char, Spacing)] = &[('<', proc_macro2::Spacing::Alone)];
    const ARROW: &[(char, Spacing)] = &[
        ('-', proc_macro2::Spacing::Joint),
        ('>', proc_macro2::Spacing::Alone),
    ];

    // https://bodil.lol/parser-combinators/
    pub type ParseInput = PeekMoreIterator<IntoIter>;
    pub type ParseResult<R> = Result<(ParseInput, R), ParseInput>;

    pub trait ParseInputHelpers {
        fn from_tkstream(stream: TokenStream) -> ParseInput;
    }

    impl ParseInputHelpers for ParseInput {
        fn from_tkstream(stream: TokenStream) -> Self {
            stream.into_iter().peekmore()
        }
    }

    pub fn rule(mut input: ParseInput) -> ParseResult<Rule> {
        let rhs = one_or_more(rhselement);
        let lhs = ident;
        let arrow = match_punct(ARROW);
        let rule_parser = as_pair(lhs, as_pair(arrow, rhs));
        map(rule_parser, |(lhs_res, (_, rhs_res))| Rule {
            lhs: lhs_res,
            rhs: rhs_res,
        })(input)
    }

    pub fn nonterminal(mut input: ParseInput) -> ParseResult<NonTerminal> {
        map(
            as_pair(as_pair(ident, match_punct(COLON)), ident),
            |((ident1, _), ident2)| NonTerminal {
                name: ident2,
                member: ident1,
            },
        )(input)
    }

    fn terminal(mut input: ParseInput) -> ParseResult<Terminal> {
        map(
            as_pair(
                ident,
                as_pair(match_punct(LT), as_pair(ident, match_punct(GT))),
            ),
            |(ident1, (_, (ident2, _)))| Terminal {
                name: ident2,
                member: ident1,
            },
        )(input)
    }

    pub fn try_nonterminal_child(
        mut input: ParseInput,
    ) -> ParseResult<((Ident, Vec<TokenTree /*COLON*/>), Option<Ident>)> {
        as_pair_opt(as_pair(ident, match_punct(COLON)), ident)(input)
    }

    pub fn terminal_child(
        mut input: ParseInput,
    ) -> ParseResult<(
        Ident,
        (Vec<TokenTree /*LT*/>, (Ident, Vec<TokenTree /*GT*/>)),
    )> {
        as_pair(
            ident,
            as_pair(match_punct(LT), as_pair(ident, match_punct(GT))),
        )(input)
    }

    pub fn rhselement(mut input: ParseInput) -> ParseResult<RhsElement> {
        return map(
            alt(try_nonterminal_child, terminal_child),
            |((member, _), nt_alt, alt2)| {
                if let Some(nt_name) = nt_alt {
                    RhsElement::NonTerminal(NonTerminal {
                        name: nt_name,
                        member: member,
                    })
                } else {
                    if let Some((_, (_, (t_name, _)))) = alt2 {
                        RhsElement::Terminal(Terminal {
                            name: t_name,
                            member: member,
                        })
                    } else {
                        panic!("This point should never be reached!");
                    }
                }
            },
        )(input);
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

    pub fn match_punct(
        to_match: &'static [(char, Spacing)],
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
                if is_value.spacing() != expected.1 {
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

    fn alt<P1, P2, R1, R2, R3>(
        first: P1,
        second: P2,
    ) -> impl Fn(ParseInput) -> ParseResult<(R1, Option<R2>, Option<R3>)>
    where
        P1: Fn(ParseInput) -> ParseResult<(R1, Option<R2>)>,
        P2: Fn(ParseInput) -> ParseResult<R3>,
    {
        move |input| {
            first(input).and_then(|(next_input, (resval1, resval2))| match resval2 {
                Some(_) => Ok((next_input, (resval1, resval2, None))),
                None => match second(next_input) {
                    Ok((next_input2, resval3)) => {
                        Ok((next_input2, (resval1, resval2, Some(resval3))))
                    }
                    Err(err) => Err(err),
                },
            })
        }
    }

    fn as_pair<P1, P2, R1, R2>(
        first: P1,
        second: P2,
    ) -> impl Fn(ParseInput) -> ParseResult<(R1, R2)>
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

    fn as_pair_opt<P1, P2, R1, R2>(
        first: P1,
        second: P2,
    ) -> impl Fn(ParseInput) -> ParseResult<(R1, Option<R2>)>
    where
        P1: Fn(ParseInput) -> ParseResult<R1>,
        P2: Fn(ParseInput) -> ParseResult<R2>,
    {
        move |input| {
            first(input).and_then(|(next_input, resval1)| match second(next_input) {
                Ok((next_input2, resval2)) => Ok((next_input2, (resval1, Some(resval2)))),
                Err(next_input2) => Ok((next_input2, (resval1, None))),
            })
        }
    }

    fn map<P, F, R, Q>(parser: P, map_fn: F) -> impl Fn(ParseInput) -> ParseResult<Q>
    where
        P: Fn(ParseInput) -> ParseResult<R>,
        F: Fn(R) -> Q,
    {
        move |input| {
            parser(input).map(|(next_input, parse_result)| (next_input, map_fn(parse_result)))
        }
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

    mod tests {
        use super::{ident, ParseInput, ParseInputHelpers, ARROW, match_punct};

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
}
/*fn pred_one<P,R,C>(parser: P) -> impl Fn(ParseInput) -> ParseResult<Vec<R>>
where
    P: Fn(ParseInput) -> ParseResult<R>,
    C: Fn(R) -> bool
{

}*/
