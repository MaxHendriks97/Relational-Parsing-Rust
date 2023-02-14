//! # Grammar
//! 
//! The `grammar` module defines the data structure which is used to express a context-free grammar.

use std::collections::{HashSet, HashMap};

use crate::word::*;
// use crate::finite_state_automaton::*;

pub struct Grammar {
    pub terminals: HashSet<Terminal>,
    pub nonterminals: HashSet<Nonterminal>,
    pub start: Nonterminal,
    pub rules: HashMap<Nonterminal, HashSet<Word>>,
    // pub finite_state_automaton: FiniteStateAutomaton,
}

impl Grammar {
    pub fn new(terminals: HashSet<Terminal>, nonterminals: HashSet<Nonterminal>, start: Nonterminal, rules: HashMap<Nonterminal, HashSet<Word>>) -> Grammar {
        Grammar::check_valid(&terminals, &nonterminals, &rules);
        // let finite_state_automaton = FiniteStateAutomaton::build_fsa(&terminals, start, &rules);
        // Grammar{terminals, nonterminals, start, rules, finite_state_automaton}
        Grammar{terminals, nonterminals, start, rules}
    }

    fn check_valid(terminals: &HashSet<Terminal>, nonterminals: &HashSet<Nonterminal>, rules: &HashMap<Nonterminal, HashSet<Word>>) {
        for (nt, rules_set) in rules {
            if !nonterminals.contains(nt) {
                println!("{} not in nonterminals", nt);
                panic!()
            }
            for rule in rules_set {
                for symbol in rule {
                    match symbol {
                        Symbol::Epsilon => {},
                        Symbol::Terminal(t) => {
                            if !terminals.contains(t) {
                                println!("{} not in terminals", t);
                                panic!()
                            }
                        },
                        Symbol::Nonterminal(nt) => {
                            if !nonterminals.contains(nt) {
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
