//! # Word
//! 
//! The 'word' module contains the type definitions for terminal and nonterminal symbols and further 
//! defines a simple structure that allows us to express combinations of these symbols as words.

use std::fmt;

// Terminal can only be lowercase.
pub type Terminal = char;
// Nonterminals can only be uppercase.
pub type Nonterminal = char;
pub type Word = Vec<Symbol>;

#[derive(Eq, PartialEq, Hash, Debug, Clone, Copy, PartialOrd, Ord)]
pub enum Symbol {
    Terminal(Terminal),
    Nonterminal(Nonterminal),
    Epsilon,
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Symbol::Terminal(t) => write!(f, "{}", t),
            Symbol::Nonterminal(nt) => write!(f, "{}", nt),
            Symbol::Epsilon => write!(f, "e"),
        }
    }
}