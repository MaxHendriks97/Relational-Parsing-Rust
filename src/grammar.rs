//! # Grammar
//! 
//! The `grammar` module defines the data structure which is used to express a context-free grammar.

use core::fmt;
use std::collections::{HashSet, HashMap};

use crate::word::*;
// use crate::finite_state_automaton::*;

pub struct GrammarRules(HashMap<Nonterminal, HashSet<Word>>);

impl fmt::Display for GrammarRules {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (nt, rules) in self.0.iter() {
            write!(f, "{} -> ", nt)?;
            let mut peekable = rules.iter().peekable();
            while let Some(rule) = peekable.next() {
                write!(f, "{}", rule)?;
                if peekable.peek().is_some() {
                    write!(f, " | ")?;
                }
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

impl GrammarRules {
    pub fn new() -> GrammarRules {
        GrammarRules(HashMap::new())
    }

    pub fn iter(&self) -> GrammarRulesIterator {
        GrammarRulesIterator{rules: self, index: 0}
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn contains(&self, nt: &Nonterminal, word: &Word) -> bool {
        match self.0.get(nt) {
            Some(rules) => rules.contains(word),
            None => false,
        }
    }

    pub fn insert_word(&mut self, nt: Nonterminal, word: Word) {
        self.0.entry(nt).or_default().insert(word);
    }

    pub fn insert_word_set(&mut self, nt: Nonterminal, words: HashSet<Word>) {
        self.0.entry(nt).or_default().extend(words);
    }

    pub fn extend_with_words(&mut self, nt: Nonterminal, words: HashSet<Word>) {
        self.0.entry(nt).or_default().extend(words);
    }

    pub fn nt_has_word(&self, nt: Nonterminal, word: &Word) -> bool {
        match self.0.get(&nt) {
            Some(rules) => rules.contains(word),
            None => false,
        }
    }

    pub fn get_words(&self, nt: &Nonterminal) -> Option<&HashSet<Word>> {
        self.0.get(nt)
    }
}

pub struct GrammarRulesIterator<'a> {
    rules: &'a GrammarRules,
    index: usize,
}

impl<'a> Iterator for GrammarRulesIterator<'a> {
    type Item = (&'a Nonterminal, &'a HashSet<Word>);

    fn next(&mut self) -> Option<Self::Item> {
        let index = self.index;
        self.index += 1;
        match self.rules.0.iter().nth(index) {
            Some((nt, rules)) => Some((nt, rules)),
            None => None,
        }
    }
}

pub struct Grammar {
    pub terminals: HashSet<Terminal>,
    pub nonterminals: HashSet<Nonterminal>,
    pub start: Nonterminal,
    pub rules: GrammarRules,
    // pub finite_state_automaton: FiniteStateAutomaton,
}

impl Grammar {
    pub fn new(terminals: HashSet<Terminal>, nonterminals: HashSet<Nonterminal>, start: Nonterminal, rules: GrammarRules) -> Grammar {
        Grammar::check_valid(&terminals, &nonterminals, &rules);
        // let finite_state_automaton = FiniteStateAutomaton::build_fsa(&terminals, start, &rules);
        // Grammar{terminals, nonterminals, start, rules, finite_state_automaton}
        Grammar{terminals, nonterminals, start, rules}
    }

    fn check_valid(terminals: &HashSet<Terminal>, nonterminals: &HashSet<Nonterminal>, rules: &GrammarRules) {
        for (nt, rules_set) in rules.iter() {
            if !nonterminals.contains(nt) {
                println!("{} not in nonterminals", nt);
                panic!()
            }
            for rule in rules_set {
                for symbol in rule.iter() {
                    match symbol {
                        Symbol::Epsilon => {},
                        Symbol::Terminal(t) => {
                            if !terminals.contains(&t) {
                                println!("{} not in terminals", t);
                                panic!()
                            }
                        },
                        Symbol::Nonterminal(nt) => {
                            if !nonterminals.contains(&nt) {
                                println!("{} not in nonterminals", nt);
                                panic!()
                            }
                        },
                    }
                }
            }
        }
    }
}

impl fmt::Display for Grammar {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "terminals: ")?;
        let mut peekable = self.terminals.iter().peekable();
        while let Some(terminal) = peekable.next() {
            write!(f, "{}", terminal)?;
            if peekable.peek().is_some() {
                write!(f, ", ")?;
            }
        }
        write!(f, "\n")?;
        write!(f, "nonterminals: ")?;
        let mut peekable = self.nonterminals.iter().peekable();
        while let Some(nonterminal) = peekable.next() {
            write!(f, "{}", nonterminal)?;
            if peekable.peek().is_some() {
                write!(f, ", ")?;
            }
        }
        write!(f, "\n")?;
        write!(f, "start: {}\n", self.start)?;
        write!(f, "{}", self.rules)?;
        // write!(f, "{}", self.finite_state_automaton)?;
        Ok(())
    }
}
