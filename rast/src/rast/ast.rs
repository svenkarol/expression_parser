extern crate proc_macro;

use proc_macro2::Ident;
use proc_macro2::Spacing;

pub struct Grammar {
    pub rules: Vec<Rule>,
}

pub struct Rule {
    pub lhs: Ident,
    pub rhs: Vec<RhsElement>,
}

pub struct NonTerminal {
    pub name: Ident,
    pub member: Ident,
}

pub struct Terminal {
    pub name: Ident,
    pub member: Ident,
}

pub enum RhsElement {
    NonTerminal(NonTerminal),
    Terminal(Terminal),
}

pub enum FollowedBy {
    Spacing(Spacing),
    AnyToken,
}

pub const COLON: &[(char, FollowedBy)] = &[(':', FollowedBy::AnyToken)];
pub const COLON_LT: &[(char, FollowedBy)] = &[
    (':', FollowedBy::Spacing(proc_macro2::Spacing::Joint)),
    ('<', FollowedBy::Spacing(proc_macro2::Spacing::Alone)),
];
pub const GT: &[(char, FollowedBy)] = &[('>', FollowedBy::Spacing(proc_macro2::Spacing::Alone))];
pub const LT: &[(char, FollowedBy)] = &[('<', FollowedBy::Spacing(proc_macro2::Spacing::Alone))];
pub const ARROW: &[(char, FollowedBy)] = &[
    ('-', FollowedBy::Spacing(proc_macro2::Spacing::Joint)),
    ('>', FollowedBy::Spacing(proc_macro2::Spacing::Alone)),
];
