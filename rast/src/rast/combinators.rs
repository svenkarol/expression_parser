use peekmore::PeekMoreIterator;
use proc_macro2::{token_stream::IntoIter, TokenTree};

// https://bodil.lol/parser-combinators/
pub type ParseInput = PeekMoreIterator<IntoIter>;
pub type ParseResult<R> = Result<(ParseInput, R), ParseInput>;

pub fn alt<P1, P2, R1, R2, R3>(
    first: P1,
    second: P2,
) -> impl Fn(ParseInput) -> ParseResult<(Option<R2>, Option<R3>)>
where
    P1: Fn(ParseInput) -> ParseResult<(R1, Option<R2>)>,
    P2: Fn(R1, ParseInput) -> ParseResult<R3>,
{
    move |input| {
        first(input).and_then(|(next_input, (resval1, resval2))| match resval2 {
            Some(_) => Ok((next_input, (resval2, None))),
            None => match second(resval1, next_input) {
                Ok((next_input2, resval3)) => Ok((next_input2, (None, Some(resval3)))),
                Err(err) => Err(err),
            },
        })
    }
}

pub fn as_pair<P1, P2, R1, R2>(
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

pub fn as_pair_opt<P1, P2, R1, R2>(
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

pub fn map<P, F, R, Q>(parser: P, mapper: F) -> impl Fn(ParseInput) -> ParseResult<Q>
where
    P: Fn(ParseInput) -> ParseResult<R>,
    F: Fn(R) -> Q,
{
    move |input| parser(input).map(|(next_input, parse_result)| (next_input, mapper(parse_result)))
}

pub fn as_left<P1, P2, R1, R2>(first: P1, second: P2) -> impl Fn(ParseInput) -> ParseResult<R1>
where
    P1: Fn(ParseInput) -> ParseResult<R1>,
    P2: Fn(ParseInput) -> ParseResult<R2>,
{
    map(as_pair(first, second), |(left, _right)| left)
}

pub fn as_right<P1, P2, R1, R2>(first: P1, second: P2) -> impl Fn(ParseInput) -> ParseResult<R2>
where
    P1: Fn(ParseInput) -> ParseResult<R1>,
    P2: Fn(ParseInput) -> ParseResult<R2>,
{
    map(as_pair(first, second), |(_left, right)| right)
}

pub fn one_or_more<P, R>(parser: P) -> impl Fn(ParseInput) -> ParseResult<Vec<R>>
where
    P: Fn(ParseInput) -> ParseResult<Option<R>>,
{
    move |mut input| {
        let mut result: Vec<R> = Vec::new();
        match parser(input) {
            Ok((next_input, value)) => match value {
                Some(value) => {
                    result.push(value);
                    input = next_input;
                }
                None => return Err(next_input),
            },
            Err(next_input) => return Err(next_input),
        }

        loop {
            match parser(input) {
                Ok((next_input, value)) => match value {
                    Some(value) => {
                        result.push(value);
                        input = next_input;
                    }
                    None => return Ok((next_input, result)),
                },
                Err(next_input) => return Err(next_input),
            }
        }
    }
}

pub fn zero_or_more<P, R>(parser: P) -> impl Fn(ParseInput) -> ParseResult<Vec<R>>
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

pub fn la_pred<P>(la_checks: Vec<P>) -> impl Fn(ParseInput) -> ParseResult<bool>
where
    P: Fn(&TokenTree) -> bool,
{
    move |mut input| {
        let pred_count = la_checks.len();
        let preview = input.peek_range(0, pred_count);
        let check_result = preview.iter().zip(la_checks.iter()).fold(
            preview.len() == pred_count,
            |matched, (a, b)| {
                matched
                    && match a {
                        Some(tt) => b(tt),
                        None => false,
                    }
            },
        );
        Ok((input, check_result))
    }
}

pub fn la_parser<P, Q, R>(parser: P, la_test: Q) -> impl Fn(ParseInput) -> ParseResult<Option<R>>
where
    P: Fn(ParseInput) -> ParseResult<R>,
    Q: Fn(ParseInput) -> ParseResult<bool>,
{
    move |input| {
        match la_test(input) {
            Ok((next_input, value)) => {
                if value {
                    match parser(next_input) {
                        Ok((next_input2, value)) => Ok((next_input2, Some(value))),
                        Err(next_input2) => Err(next_input2),
                    }
                } else {
                    Ok((next_input, None))
                }
            }
            Err(next_input) => Err(next_input),
        }

        //let (next_input,value) = la(input);

        /*and_then(|(next_input,value)|{
            if value {
                match parser(next_input) {
                    Ok((next_input2, value)) => Ok((next_input2, Some(value))),
                    Err(next_input2) => Err(next_input2),
                }
            }
            else {
                Ok((input, None))
            }
        })*/
    }
}
