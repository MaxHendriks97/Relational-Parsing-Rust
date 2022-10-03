use std::fmt;

pub type Terminal = char;
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