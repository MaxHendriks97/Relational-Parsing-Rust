use std::collections::{HashSet, HashMap};

use crate::word::*;
use crate::finite_state_automaton::*;

pub struct Grammar {
    pub terminals: HashSet<Terminal>,
    pub nonterminals: HashSet<Nonterminal>,
    pub symbols: HashSet<Symbol>,
    pub start: Nonterminal,
    pub rules: HashMap<Nonterminal, HashSet<Word>>,
    pub finite_state_automaton: FiniteStateAutomaton,
}

impl Grammar {
    pub fn new(terminals: HashSet<Terminal>, nonterminals: HashSet<Nonterminal>, start: Nonterminal, rules: HashMap<Nonterminal, HashSet<Word>>) -> Grammar {
        let mut symbols: HashSet<Symbol> = HashSet::new();
        for s in &nonterminals {
            symbols.insert(Symbol::Nonterminal(*s));
        }
        for t in &terminals {
            symbols.insert(Symbol::Terminal(*t));
        }
        let finite_state_automaton = FiniteStateAutomaton::build_fsa(&terminals, start, &rules);
        Grammar{terminals, nonterminals, symbols, start, rules, finite_state_automaton}
    }
}
