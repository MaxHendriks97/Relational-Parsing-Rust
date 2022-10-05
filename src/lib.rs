use std::collections::{HashSet, VecDeque};

mod word;
pub use word::*;
mod grammar;
pub use grammar::*;
mod regex;
pub use regex::*;
mod finite_state_automaton;
pub use finite_state_automaton::*;

type Configuration = Vec<(State, bool)>;
type Language = HashSet<Configuration>;

pub fn prepend(atomic_language: (State, bool), language: &Language) -> Language {
    let mut res: Language = HashSet::new();
    for configuration in language {
        res.insert([[atomic_language].to_vec(), configuration.clone()].concat());
    }
    res
}

pub fn derivative(language: &Language, symbol: Symbol, finite_state_automaton: &FiniteStateAutomaton) -> Language {
    let mut res: Language = HashSet::new();
    for configuration in language {
        let mut curr_configuration: VecDeque<(State, bool)> = configuration.clone().into();
        if let Some((source, accepting)) = curr_configuration.pop_front() {
            if let Some((dest, accepting)) = finite_state_automaton.simulate(&source, symbol) {
                let mut new_configuration = curr_configuration.clone();
                if finite_state_automaton.has_transition(dest) {
                    new_configuration.push_front((*dest, accepting));
                }
                res.insert(new_configuration.into());
            }
            let mut curr = source;
            let mut accepting = accepting;
            while accepting {
                if let Some((new_curr, new_accepting)) = curr_configuration.pop_front() {
                    accepting = new_accepting;
                    if let Some((dest, accepting)) = finite_state_automaton.simulate(&new_curr, symbol) {
                        let mut new_configuration = curr_configuration.clone();
                        if finite_state_automaton.has_transition(dest) {
                            new_configuration.push_front((*dest, accepting));
                        }
                        res.insert(new_configuration.into());
                    }
                    curr = new_curr;
                } else {
                    break;
                }
            }
        }
    }
    

    res
}

pub fn epsilon(language: &Language, finite_state_automaton: &FiniteStateAutomaton) -> bool {
    'lang: for configuration in language {
        for (_, accepting) in configuration {
            if !accepting {
                continue 'lang;
            }
        }
        return true;
    }
    false
}

pub fn g_accepts_string(token_string: Vec<Terminal>, grammar: &Grammar) -> bool {
    let finite_state_automaton: &FiniteStateAutomaton = &grammar.finite_state_automaton;
    let symbols: &HashSet<Symbol> = &grammar.symbols;
    let mut lang: Language = HashSet::from([vec![finite_state_automaton.get_start()]]);
    for token in token_string {
        println!("lang: {:?}", lang);
        let mut new_lang: Language = HashSet::new();
        for symbol in symbols {
            if let Some((atomic, accepting)) = finite_state_automaton.get_atomic(*symbol, token) {
                if finite_state_automaton.has_transition(atomic) {
                    new_lang.extend(prepend((*atomic, accepting), &derivative(&lang, *symbol, &finite_state_automaton)));
                } else {
                    new_lang.extend(derivative(&lang, *symbol, &finite_state_automaton));
                }
            }
        }
        lang = new_lang;
    }
    println!("lang: {:?}", lang);
    epsilon(&lang, finite_state_automaton)
}

