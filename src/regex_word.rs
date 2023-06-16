use core::fmt;
use std::collections::HashSet;

use crate::{Terminal, Nonterminal, Rules};

// Used as an intermediary data structure, keeping track of some additional information while we calculate atomic languages.
#[derive(Eq, PartialEq, Hash, Debug, Clone, PartialOrd, Ord)]
pub enum RegexSymbol {
    Terminal(Terminal),
    Nonterminal(Nonterminal),

    // Placeholder symbol for atomic languages calculated in the future. If these atomic languages will exist,
    // they will be prepended to the rest of the rule following the atomic language symbol.
    AtomicLanguage(Nonterminal, Terminal),

    // Expresses nulled symbols and its associated nulling rule
    Nulled(Rules),
    Epsilon,
}

impl fmt::Display for RegexSymbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RegexSymbol::Terminal(t) => write!(f, "{}", t),
            RegexSymbol::Nonterminal(nt) => write!(f, "{}", nt),
            RegexSymbol::AtomicLanguage(nt, t) => write!(f, "[{}]^({})", nt, t),
            RegexSymbol::Nulled(rules) => write!(f, "{}", rules),
            RegexSymbol::Epsilon => write!(f, "e"),
        }
    }
}

#[derive(Eq, PartialEq, Hash, Debug, Clone, PartialOrd, Ord)]
pub struct RegexWord(Vec<RegexSymbol>);

impl fmt::Display for RegexWord {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for regex_symbol in self.0.iter() {
            write!(f, "{}", regex_symbol)?;
        }
        Ok(())
    }
}

impl RegexWord {
    pub fn new(regex_symbols: Vec<RegexSymbol>) -> RegexWord {
        RegexWord(regex_symbols)
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn equals(&self, other: &RegexWord) -> bool {
        self.0 == other.0
    }

    pub fn contains(&self, regex_symbol: &RegexSymbol) -> bool {
        self.0.contains(regex_symbol)
    }

    pub fn contains_epsilon(&self) -> bool {
        self.contains(&RegexSymbol::Epsilon)
    }

    pub fn first(&self) -> &RegexSymbol {
        &self.0[0]
    }

    pub fn not_first(&self) -> RegexWord {
        RegexWord::new(self.0[1..].to_vec())
    }

    pub fn set_position(&mut self, index: usize, regex_symbol: RegexSymbol) {
        self.0[index] = regex_symbol;
    }

    pub fn extend(&mut self, other: RegexWord) {
        self.0.extend(other.0);
    }

    pub fn concat(&self, other: &RegexWord) -> RegexWord {
        let mut regex_word = self.clone();
        regex_word.extend(other.clone());
        regex_word
    }

    pub fn push(&mut self, regex_symbol: RegexSymbol) {
        self.0.push(regex_symbol);
    }

    pub fn iter(&self) -> RegexWordIterator {
        RegexWordIterator{regex_word: self, index: 0}
    }
}

pub struct RegexWordIterator<'a> {
    regex_word: &'a RegexWord,
    index: usize,
}

impl<'a> Iterator for RegexWordIterator<'a> {
    type Item = &'a RegexSymbol;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.regex_word.len() {
            let regex_symbol = &self.regex_word.0[self.index];
            self.index += 1;
            Some(regex_symbol)
        } else {
            None
        }
    }
}

#[derive(Eq, PartialEq, Hash, Debug, Clone, PartialOrd, Ord)]
pub struct RegexWordRule(RegexWord, Rules);

impl fmt::Display for RegexWordRule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "({} -> {})", self.word(), self.rules())
    }
}

impl RegexWordRule {
    pub fn new(regex_word: RegexWord, rules: Rules) -> RegexWordRule {
        RegexWordRule(regex_word, rules)
    }

    pub fn word(&self) -> &RegexWord {
        &self.0
    }

    pub fn rules(&self) -> &Rules {
        &self.1
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct RegexWordRuleSet(HashSet<RegexWordRule>);

impl RegexWordRuleSet {
    pub fn new() -> RegexWordRuleSet {
        RegexWordRuleSet(HashSet::new())
    }

    pub fn insert(&mut self, regex_word_rule: RegexWordRule) {
        self.0.insert(regex_word_rule);
    }

    pub fn iter(&self) -> std::collections::hash_set::Iter<RegexWordRule> {
        self.0.iter()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn contains(&self, regex_word_rule: &RegexWordRule) -> bool {
        self.0.contains(regex_word_rule)
    }
}

impl fmt::Display for RegexWordRuleSet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{")?;
        let mut peekable = self.iter().peekable();
        while let Some(regex_word_rule) = peekable.next() {
            write!(f, "{}", regex_word_rule)?;
            if peekable.peek().is_some() {
                write!(f, " ")?;
            }
        }
        write!(f, "}}")

    }
}
