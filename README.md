# Expression Parser
A toy example of a recursive descent parser and lexer for a small arithmetic expressions language written in Rust. Except parsing, it does not do anything yet. Example expression: `0 + 8 * 15`.

## Grammar
### Tokens
The tokens that the lexer needs to recognize (modulo whitespace).
```
MulOp := '*' | '/'
AddOp := '+' | '-'
Num := ['0' - '9']+
```

### Productions (left recursive)
This is the base grammar.
```
Start → Exp
Exp → Exp + Term
Exp → Exp – Term
Exp → Term
Term → Term ∗ Num
Term → Term / Num
Term → Num
```

### Productions (normalized)
We cannot use left-recursive grammars to build a recursive descent parser. So we just can transform the grammar as follows:
```
Start → Exp
Exp → Term Exp′
Exp′ → '–' Term Exp′
Exp′ → '+' Term Exp′
Exp′ → 𝜖
Term → Num Term′
Term′ → '∗' Num Term′
Term′ → '/' Num Term′
Term′ → 𝜖 
```
This is the grammar that was used to create the toy example.

## Usage
Install Rust and Cargo on your system as described [here](https://www.rust-lang.org/tools/install). Clone this repo, `cd` to the checkout directory and run `cargo test`. This should just compile the sources and run the accompanied unit tests.
