//! # Word
//! 
//! The 'word' module contains the type definitions for terminal and nonterminal symbols and further 
//! defines a simple structure that allows us to express combinations of these symbols as words.

use std::fmt;
use std::collections::BTreeSet;

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

#[derive(Eq, PartialEq, Hash, Debug, Clone, PartialOrd, Ord)]
pub struct Word(Vec<Symbol>);

impl fmt::Display for Word {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for symbol in self.0.iter() {
            write!(f, "{}", symbol)?;
        }
        Ok(())
    }
}

impl Word {
    pub fn from(symbols: Vec<Symbol>) -> Word {
        Word(symbols)
    }

    pub fn from_string(string: &str) -> Word {
        let mut symbols = Vec::new();
        for c in string.chars() {
            match c {
                'e' => symbols.push(Symbol::Epsilon),
                _ => symbols.push(
                    if c.is_lowercase() {
                        Symbol::Terminal(c)
                    } else {
                        Symbol::Nonterminal(c)
                    }
                ),
            }
        }
        Word(symbols)
    }

    pub fn from_single(symbol: Symbol) -> Word {
        Word(vec![symbol])
    }

    pub fn iter(&self) -> WordIterator {
        WordIterator{word: self, index: 0}
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn is_e(&self) -> bool {
        self.len() == 1 && self.0[0] == Symbol::Epsilon
    }

    pub fn equals(&self, other: &Word) -> bool {
        self.0 == other.0
    }
}

pub struct WordIterator<'a> {
    word: &'a Word,
    index: usize,
}

impl<'a> Iterator for WordIterator<'a> {
    type Item = Symbol;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.word.0.len() {
            let symbol = self.word.0[self.index];
            self.index += 1;
            Some(symbol)
        } else {
            None
        }
    }
}

#[derive(Eq, PartialEq, Hash, Debug, Clone, PartialOrd, Ord)]
pub struct Rule(Nonterminal, Word);

impl fmt::Display for Rule {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} -> {}", self.0, self.1)
    }
}

impl Rule {
    pub fn from(nt: Nonterminal, word: Word) -> Rule {
        Rule(nt, word)
    }

    pub fn from_str(nt: Nonterminal, word: &str) -> Rule {
        Rule(nt, Word::from_string(word))
    }

    pub fn nt(&self) -> Nonterminal {
        self.0
    }
}

#[derive(Eq, PartialEq, Hash, Debug, Clone, PartialOrd, Ord)]
pub struct Rules(Vec<Rule>);

impl fmt::Display for Rules {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[")?;
        let mut peekable = self.0.iter().peekable();
        while let Some(rule) = peekable.next() {
            write!(f, "{}", rule)?;
            if peekable.peek().is_some() {
                write!(f, ", ")?;
            }
        }
        write!(f, "]")
    }
}


impl Rules {
    pub fn new() -> Rules {
        Rules(Vec::new())
    }

    pub fn from(rules: Vec<Rule>) -> Rules {
        Rules(rules)
    }

    pub fn from_single(nt: Nonterminal, word: Word) -> Rules {
        Rules(vec![Rule::from(nt, word)])
    }

    pub fn from_string_vec(rules: Vec<(Nonterminal, &str)>) -> Rules {
        let mut rule_vec = Vec::new();
        for (nt, word) in rules {
            rule_vec.push(Rule::from_str(nt, word));
        }
        Rules(rule_vec)
    }

    pub fn iter(&self) -> RulesIterator {
        RulesIterator{rules: self, index: 0}
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn equals(&self, other: &Rules) -> bool {
        self.0 == other.0
    }

    pub fn contains(&self, rule: &Rule) -> bool {
        self.0.contains(rule)
    }

    pub fn insert(&mut self, rule: Rule) {
        self.0.insert(0, rule);
    }

    pub fn extend(&mut self, rules: Rules) {
        self.0.extend(rules.0);
    }

    pub fn concat(&self, other: &Rules) -> Rules {
        let mut rules = self.clone();
        rules.extend(other.clone());
        rules
    }

    pub fn concat_rule(&self, other: &Rule) -> Rules {
        let mut rules = self.clone();
        rules.insert(other.clone());
        rules
    }

    pub fn remove(&mut self, index: usize) -> Rule {
        self.0.remove(index)
    }
}

pub struct RulesIterator<'a> {
    rules: &'a Rules,
    index: usize,
}

impl<'a> Iterator for RulesIterator<'a> {
    type Item = Rule;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index < self.rules.0.len() {
            let rule = &self.rules.0[self.index];
            self.index += 1;
            Some(rule.clone())
        } else {
            None
        }
    }
}

#[derive(Eq, PartialEq, Hash, Debug, Clone, PartialOrd, Ord, Default)]
pub struct RulesSet(BTreeSet<Rules>);

impl fmt::Display for RulesSet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{")?;
        let mut peekable = self.0.iter().peekable();
        while let Some(rules) = peekable.next() {
            write!(f, "{}", rules)?;
            if peekable.peek().is_some() {
                write!(f, ", ")?;
            }
        }
        write!(f, "}}")
    }
}

impl RulesSet {
    pub fn new() -> RulesSet {
        RulesSet(BTreeSet::new())
    }

    pub fn from(rules: BTreeSet<Rules>) -> RulesSet {
        RulesSet(rules)
    }

    pub fn from_rules(rules: Rules) -> RulesSet {
        let mut set = RulesSet::new();
        set.insert_rules(rules);
        set
    }

    pub fn from_vec_array(rules: Vec<Vec<(Nonterminal, &str)>>) -> RulesSet {
        let mut set = RulesSet::new();
        for rule_vec in rules {
            set.insert_rules(Rules::from_string_vec(rule_vec));
        }
        set
    }

    pub fn from_vec_slice(rules: &[Vec<(Nonterminal, &str)>]) -> RulesSet {
        let mut set = RulesSet::new();
        for rule_vec in rules {
            set.insert_rules(Rules::from_string_vec(rule_vec.to_vec()));
        }
        set
    }

    pub fn iter(&self) -> std::collections::btree_set::Iter<Rules> {
        self.0.iter()
    }

    pub fn into_iter(self) -> std::collections::btree_set::IntoIter<Rules> {
        self.0.into_iter()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub fn equals(&self, other: &RulesSet) -> bool {
        self.0 == other.0
    }

    pub fn contains(&self, rules: &Rules) -> bool {
        self.0.contains(rules)
    }

    pub fn insert_rules(&mut self, rules: Rules) {
        self.0.insert(rules);
    }

    pub fn insert_rule(&mut self, rule: Rule) {
        self.0.insert(Rules::from(vec![rule]));
    }

    pub fn extend(&mut self, rules_set: RulesSet) {
        self.0.extend(rules_set.0);
    }

    pub fn prepend_rules(&self, rules: &Rules) -> RulesSet {
        if self.is_empty() {
            return RulesSet::from_rules(rules.clone());
        }
        let mut rules_set = RulesSet::new();
        for rules_ in self.iter() {
            rules_set.insert_rules(rules.concat(rules_));
        }
        rules_set
    }

    pub fn prepend_opt_rules(&self, rules: &Option<Rules>) -> RulesSet {
        match rules {
            Some(rules) => self.prepend_rules(rules),
            None => self.clone(),
        }
    }

    pub fn concatenate_rules_set(&self, other: &RulesSet) -> RulesSet {
        let mut rules_set = RulesSet::new();
        for rules in self.iter() {
            rules_set.extend(other.prepend_rules(rules));
        }
        rules_set
    }
}
