//! # Word
//! 
//! The 'word' module contains the type definitions for terminal and nonterminal symbols and further 
//! defines a simple structure that allows us to express combinations of these symbols as words.

use std::fmt;
use std::collections::HashSet;

// Terminal can only be lowercase.
pub type Terminal = char;
// Nonterminals can only be uppercase.
pub type Nonterminal = char;

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

pub type Word = Vec<Symbol>;

pub fn print_word(word: &Word, f: &mut fmt::Formatter) -> fmt::Result {
    for symbol in word {
        write!(f, "{}", symbol)?;
    }
    Ok(())
}

pub type Rule = (Nonterminal, Word);

pub fn print_rule(rule: &Rule, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "({}, -> ", rule.0)?;
    print_word(&rule.1, f)?;
    write!(f, ")")
}

pub type Rules = Vec<Rule>;

pub fn print_rules(rules: &Rules, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "[")?;
    let mut peekable = rules.iter().peekable();
    while let Some(rule) = peekable.next() {
        print_rule(rule, f)?;
        if peekable.peek().is_some() {
            write!(f, " ")?;
        }
    }
    write!(f, "]")
}

pub type RulesSet = HashSet<Rules>;

pub fn print_rules_set(rules_set: &RulesSet, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "{{")?;
    for rules in rules_set {
        print_rules(rules, f)?;
    }
    write!(f, "}}")
}