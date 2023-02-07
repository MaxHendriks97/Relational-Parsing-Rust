//! # Parse no memo
//! 
//! The `parse_no_memo` data structure contains a non-memoizing variant of the data structure and methods contained in the `parse` module.
use std::collections::{HashSet, BTreeMap};

use crate::*;

pub struct ParseRoundNoMemo {
    deriv: Option<Language>,
    prep_deriv: Option<Language>,
    prep: Option<Language>,
}

impl ParseRoundNoMemo {
    pub fn new() -> ParseRoundNoMemo {
        ParseRoundNoMemo{deriv: None, prep_deriv: None, prep: None}
    }

    pub fn register(self, language_list: &mut LanguageList, finite_state_automaton: &FiniteStateAutomaton) -> Result<(), ParseError> {
        if let (Some(prep_deriv), Some(mut prep)) = (self.prep_deriv, self.prep) {
            
            if prep_deriv.has_edges() {
                if let Some(mut deriv) = self.deriv {
                    for ((state, depth), rules_set) in deriv.take_edges() {
                        prep.extend_edge((state, depth + 1), rules_set);
                    }
                    prep.extend_completed_parses(deriv.take_completed_parses());
                    if deriv.is_final() {
                        prep.set_final();
                    }
                }
                language_list.insert_new_language(prep_deriv);
            } else {
                if let Some(mut deriv) = self.deriv {
                    for (edge, rules_set) in deriv.take_edges() {
                        prep.extend_edge(edge, rules_set);
                    }
                    prep.extend_completed_parses(deriv.take_completed_parses());
                    if deriv.is_final() {
                        prep.set_final();
                    }
                }
            }

            if prep.has_edges() || prep.is_final() {
                let lowest_depth = prep.find_lowest_depth();
                if lowest_depth > 1 {
                    prep.adjust_lowest_depth(lowest_depth);
                    for _ in 1..lowest_depth {
                        language_list.pop_lang();
                    }
                }
                ParseRound::e_sim(&mut prep, &language_list, finite_state_automaton);
                language_list.insert_new_language(prep);
            }
            Ok(())
        } else {
            if let Some(mut lang) = self.deriv {
                if lang.has_edges() {
                    let lowest_depth = lang.find_lowest_depth();
                    if lowest_depth > 1 {
                        lang.adjust_lowest_depth(lowest_depth);
                        for _ in 1..lowest_depth {
                            language_list.pop_lang();
                        }
                    }
                }

                language_list.insert_new_language(lang);
                Ok(())
            } else {
                return Err(ParseError);
            }
        }
    }

    pub fn derive(&mut self, curr_lang: &Language, language_list: &LanguageList, terminal: Terminal, finite_state_automaton: &FiniteStateAutomaton) {
        let mut deriv: Language = Language::new();

        for ((start_state, end_depth), applied_rules_set) in curr_lang.edges_ref() {
            if let Some(destinations) = finite_state_automaton.simulate(start_state, Symbol::Terminal(terminal)) {
                for (end_state, new_rules, end_state_accepting) in destinations {
                    let res_rules_set: HashSet<Rules> = prepend_rules_to_rules_set(new_rules, &applied_rules_set);
                    let dest_language: &Language = language_list.get(*end_depth).unwrap();

                    if end_state_accepting {
                        if dest_language.is_final() {
                            deriv.extend_completed_parses(res_rules_set.clone().into_iter());
                            deriv.set_final();
                        } else {
                            for ((state, depth), rules_set) in dest_language.edges_ref() {
                                deriv.extend_edge((*state, *depth + *end_depth), concatenate_rules_sets(&res_rules_set, rules_set));
                            }
                        }
                    }
                    if finite_state_automaton.has_edge(end_state) {
                        deriv.extend_edge((*end_state, *end_depth), res_rules_set.clone());
                    }

                }
            }
        }

        if deriv.has_edges() || deriv.is_final() {
            self.deriv = Some(deriv);
        }
    }

    pub fn prep_derive(&mut self, curr_lang: &Language, language_list: &LanguageList, nonterminal: Nonterminal, finite_state_automaton: &FiniteStateAutomaton) -> bool {
        let prep_deriv: &mut Language = self.prep_deriv.get_or_insert(Language::new());
        let mut ret: bool = false;

        for ((start_state, end_depth), applied_rules_set) in curr_lang.edges_ref() {
            if let Some(destinations) = finite_state_automaton.simulate(start_state, Symbol::Nonterminal(nonterminal)) {
                ret = true;
                
                for (end_state, new_rules, end_state_accepting) in destinations {
                    let res_rules_set: RulesSet = prepend_rules_to_rules_set(new_rules, &applied_rules_set);
                    let dest_language: &Language = language_list.get(*end_depth).unwrap();

                    if end_state_accepting {
                        if dest_language.not_final() {
                            for ((state, depth), rules_set) in dest_language.edges_ref() {
                                prep_deriv.extend_edge((*state, *depth + *end_depth), concatenate_rules_sets(&res_rules_set, rules_set));
                            }
                        }
                    }
                    if finite_state_automaton.has_edge(end_state) {
                        prep_deriv.extend_edge((*end_state, *end_depth), res_rules_set.clone());
                    }

                }
            }
        }

        ret
    }

    pub fn prepend(&mut self, atomic: (&State, &HashSet<Rules>, bool), language_list: &LanguageList, finite_state_automaton: &FiniteStateAutomaton) {
        if let Some(prep_deriv) = &self.prep_deriv {
            let mut prep: Language = Language::new();

            let edge: Edge = (*atomic.0, 1);
            if finite_state_automaton.has_edge(atomic.0) {
                match (atomic.1.is_empty(), atomic.2 && (prep_deriv.is_final() || language_list.get(1).map_or(false, |l| l.is_final()))) {
                    (true, true) => {
                        prep.insert_edge(edge, None);
                        prep.set_final();
                    },
                    (true, false) => {
                        prep.insert_edge(edge, None);
                    },
                    (false, true) => {
                        for rules in atomic.1 {
                            prep.insert_edge(edge, Some(rules.clone()));
                            prep.insert_completed_parse(rules.clone());
                        }
                        prep.set_final();
                    },
                    (false, false) => {
                        for rules in atomic.1 {
                            prep.insert_edge(edge, Some(rules.clone()));
                        }
                    },
                }
            }

            if atomic.2 {
                for rules in atomic.1 {
                    for ((state, depth), applied_rules_set) in prep_deriv.edges_ref() {
                        prep.extend_edge((*state, depth + 1), prepend_rules_to_rules_set(rules, applied_rules_set));
                    }
                }
            }

            self.prep = Some(prep);
        }
    }

}

#[derive(Debug, PartialEq, Eq)]
pub struct ParseError;

fn parse(token_string: &Vec<Terminal>, grammar: &Grammar) -> Result<Language, ParseError> {
    let finite_state_automaton: &FiniteStateAutomaton = &grammar.finite_state_automaton;
    let mut language_list: LanguageList = LanguageList::new();

    let (start_state, start_accepting) = finite_state_automaton.get_start();
    language_list.insert_new_language(Language::new_from(BTreeMap::from([((start_state, 1), HashSet::new())]), HashSet::new(), start_accepting));

    for token in token_string {
        if let Some(curr_lang) = language_list.pop_lang() {
            //println!("Next token: {}", token);

            let mut curr: ParseRoundNoMemo = ParseRoundNoMemo::new();
            curr.derive(&curr_lang, &language_list, *token, finite_state_automaton);

            for nonterminal in &grammar.nonterminals {
                if let Some(atomic) = finite_state_automaton.get_atomic(Symbol::Nonterminal(*nonterminal), *token) {
                    //println!("Found atomic: [{}]^({})", nonterminal, token);
                    let prep_deriv = curr.prep_derive(&curr_lang, &language_list, *nonterminal, finite_state_automaton);
                    if prep_deriv {
                        curr.prepend(atomic, &language_list, finite_state_automaton);
                    }
                }
            }

            curr.register(&mut language_list, finite_state_automaton)?;
        } else {
            return Err(ParseError);
        }

        //println!("{}", language_list);
    }


    if let Some(mut last_lang) = language_list.pop_lang() {
        ParseRound::e_sim(&mut last_lang, &language_list, finite_state_automaton);
        //println!("{}", language_list);
        //println!("Last: {}", last_lang);
        //println!("{}", memoize);
        Ok(last_lang)
    } else {
        //println!("{}", language_list);
        Err(ParseError)
    }
}

pub fn g_accepts_string_no_memo(token_string: &Vec<Terminal>, grammar: &Grammar) -> bool {
    match parse(token_string, grammar) {
        Ok(last_lang) => {last_lang.is_final()},
        Err(_) => {false}
    }
}

pub fn find_parses_no_memo(token_string: &Vec<Terminal>, grammar: &Grammar) -> Result<CompletedParses, ParseError> {
    match parse(token_string, grammar) {
        Ok(mut last_lang) => {
            if last_lang.is_final() && last_lang.has_completed_parses() {
                Ok(last_lang.take_completed_parses().collect())
            } else {
                Err(ParseError)
            }
        },
        Err(e) => {Err(e)},
    }
}