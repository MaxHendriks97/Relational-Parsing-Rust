//! # Parse no memo
//! 
//! The `parse_no_memo` data structure contains a non-memoizing variant of the data structure and methods contained in the `parse` module.
use std::collections::BTreeMap;

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
                    for (mut edge, rules_set) in deriv.take_edges().into_iter() {
                        edge.add_to_depth(1);
                        prep.extend_edge(edge, rules_set);
                    }
                    prep.extend_completed_parses(deriv.take_completed_parses());
                    if deriv.is_final() {
                        prep.set_final();
                    }
                }
                language_list.insert_new_language(prep_deriv);
            } else {
                if let Some(deriv) = self.deriv {
                    prep.extend(deriv);
                }
            }

            if prep.has_edges() || prep.is_final() {
                let lowest_depth = prep.find_lowest_depth();
                if lowest_depth.0 > 1 {
                    prep.adjust_lowest_depth(lowest_depth);
                    for _ in 1..lowest_depth.0 {
                        language_list.pop_lang();
                    }
                }
                ParseRoundNoMemo::e_sim(&mut prep, &language_list, finite_state_automaton);
                language_list.insert_new_language(prep);
            }
            Ok(())
        } else {
            if let Some(mut lang) = self.deriv {
                if lang.has_edges() {
                    let lowest_depth = lang.find_lowest_depth();
                    if lowest_depth.0 > 1 {
                        lang.adjust_lowest_depth(lowest_depth);
                        for _ in 1..lowest_depth.0 {
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

    pub fn e_sim(lang: &mut Language, language_list: &LanguageList, finite_state_automaton: &FiniteStateAutomaton) {
        let mut to_simulate: Vec<(language_list::Edge, RulesSet)> = lang.edges_ref().clone().into_iter().collect();

        while let Some((edge, applied_rules_set)) = to_simulate.pop() {
            if let Some(destinations) = finite_state_automaton.simulate(&edge.state(), Symbol::Epsilon) {
                for (end_state, new_rules, end_state_accepting) in destinations {
                    let dest_language: &Language = language_list.get(edge.depth()).unwrap();
                    let res_rules_set: RulesSet  = applied_rules_set.prepend_rules(&new_rules.clone().unwrap());
                    if end_state_accepting {
                        if dest_language.is_final() {
                            lang.set_final();
                            lang.extend_completed_parses(res_rules_set.clone());
                        } else {
                            for (dest_edge, rules_set) in dest_language.edges_ref().iter() {
                                let new_res_rules_set: RulesSet = res_rules_set.concatenate_rules_set(rules_set);
                                let res_edge: language_list::Edge = language_list::Edge::new(dest_edge.state(), dest_edge.depth() + edge.depth());
                                lang.extend_edge(res_edge, new_res_rules_set.clone());
                                to_simulate.push((res_edge, new_res_rules_set));
                            }
                        }
                    }
                    if finite_state_automaton.has_edge(end_state) {
                        let new_edge: language_list::Edge = language_list::Edge::new(*end_state, edge.depth());
                        lang.extend_edge(new_edge, res_rules_set.clone());
                        to_simulate.push((new_edge, res_rules_set));
                    }
                }
            }
        }
    }

    pub fn derive(&mut self, curr_lang: &Language, language_list: &LanguageList, terminal: Terminal, finite_state_automaton: &FiniteStateAutomaton) {
        let mut deriv: Language = Language::new();

        //(start_state, end_depth)
        for (edge, applied_rules_set) in curr_lang.edges_ref().iter() {
            if let Some(destinations) = finite_state_automaton.simulate(&edge.state(), Symbol::Terminal(terminal)) {
                for (end_state, opt_new_rules, end_state_accepting) in destinations {
                    let res_rules_set: RulesSet = applied_rules_set.prepend_opt_rules(opt_new_rules);
                    let dest_language: &Language = language_list.get(edge.depth()).unwrap();

                    if end_state_accepting {
                        if dest_language.is_final() {
                            deriv.extend_completed_parses(res_rules_set.clone());
                            deriv.set_final();
                        } else {
                            //(state, depth)
                            for (edge_dest, rules_set) in dest_language.edges_ref().iter() {
                                deriv.extend_edge(language_list::Edge::new(edge_dest.state(), edge_dest.depth() + edge.depth()), res_rules_set.concatenate_rules_set(rules_set));
                                //deriv.extend_edge(language_list::Edge::new(edge_dest.state(), edge_dest.depth() + edge.depth()), RulesSet::from_rules(Rules::from_string_vec(vec![('S', "test")])));
                                //prep.insert_edge(edge, Some(Rules::from_string_vec(vec![('S', "test")])));
                            }
                        }
                    }
                    if finite_state_automaton.has_edge(end_state) {
                        deriv.extend_edge(language_list::Edge::new(*end_state, edge.depth()), res_rules_set.clone());
                    }

                }
            }
        }

        if deriv.has_edges() || deriv.is_final() {
            self.deriv = Some(deriv);
        }
    }

    pub fn prep_derive(&mut self, curr_lang: &Language, language_list: &LanguageList, nonterminal: Nonterminal, finite_state_automaton: &FiniteStateAutomaton) -> bool {
        //println!("Deriving by: {}", nonterminal);
        let prep_deriv: &mut Language = self.prep_deriv.get_or_insert(Language::new());
        //println!("prep_deriv: {}", prep_deriv);
        let mut ret: bool = false;

        //(start_state, end_depth)
        for (edge, applied_rules_set) in curr_lang.edges_ref().iter() {
            if let Some(destinations) = finite_state_automaton.simulate(&edge.state(), Symbol::Nonterminal(nonterminal)) {
                //println!("destinations: {:?}", destinations);
                ret = true;
                
                for (end_state, opt_new_rules, end_state_accepting) in destinations {
                    //println!("opt_new_rules: {:?}", opt_new_rules);
                    let res_rules_set: RulesSet = applied_rules_set.prepend_opt_rules(opt_new_rules);
                    let dest_language: &Language = language_list.get(edge.depth()).unwrap();

                    if end_state_accepting {
                        if dest_language.not_final() {
                            //(state, depth)
                            for (end_edge, rules_set) in dest_language.edges_ref().iter() {
                                prep_deriv.extend_edge(language_list::Edge::new(end_edge.state(), end_edge.depth() + edge.depth()), res_rules_set.concatenate_rules_set(rules_set));
                            }
                        }
                    }
                    if finite_state_automaton.has_edge(end_state) {
                        prep_deriv.extend_edge(language_list::Edge::new(*end_state, edge.depth()), res_rules_set.clone());
                    }

                }
            }
        }

        ret
    }

    pub fn prepend(&mut self, atomic: (&State, &RulesSet, bool), language_list: &LanguageList, finite_state_automaton: &FiniteStateAutomaton) {
        if let Some(prep_deriv) = &self.prep_deriv {
            let mut prep: Language = Language::new();

            let target_depth: Depth = Depth::new(1);
            let edge: language_list::Edge = language_list::Edge::new(*atomic.0, target_depth);
            if finite_state_automaton.has_edge(atomic.0) {
                match (atomic.1.is_empty(), atomic.2 && (prep_deriv.is_final() || !prep_deriv.has_edges() && language_list.get(target_depth).map_or(false, |l| l.is_final()))) {
                    (true, true) => {
                        prep.insert_edge(edge, None);
                        prep.set_final();
                    },
                    (true, false) => {
                        prep.insert_edge(edge, None);
                    },
                    (false, true) => {
                        for rules in atomic.1.iter() {
                            prep.insert_edge(edge, Some(rules.clone()));
                            prep.insert_completed_parse(rules.clone());
                        }
                        prep.set_final();
                    },
                    (false, false) => {
                        for rules in atomic.1.iter() {
                            prep.insert_edge(edge, Some(rules.clone()));
                        }
                    },
                }
            }

            if atomic.2 {
                if atomic.1.is_empty() {
                    for (edge, applied_rules_set) in prep_deriv.edges_ref().iter() {
                        prep.extend_edge(language_list::Edge::new(edge.state(), edge.depth() + target_depth), applied_rules_set.clone());
                    }
                } else {
                    for rules in atomic.1.iter() {
                        for (edge, applied_rules_set) in prep_deriv.edges_ref().iter() {
                            prep.extend_edge(language_list::Edge::new(edge.state(), edge.depth() + target_depth), applied_rules_set.prepend_rules(rules));
                        }
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
    let start_edges: language_list::Edges = language_list::Edges::from(BTreeMap::from([(language_list::Edge::new(start_state, Depth::new(1)), RulesSet::new())]));
    language_list.insert_new_language(Language::new_from(start_edges, RulesSet::new(), start_accepting));

    for token in token_string {
        if let Some(curr_lang) = language_list.pop_lang() {
            println!("Next token: {}", token);

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

        println!("{}", language_list);
    }


    if let Some(mut last_lang) = language_list.pop_lang() {
        ParseRoundNoMemo::e_sim(&mut last_lang, &language_list, finite_state_automaton);
        println!("{}", language_list);
        //println!("{:?}", language_list);
        println!("Last: {}", last_lang);
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

pub fn find_parses_no_memo(token_string: &Vec<Terminal>, grammar: &Grammar) -> Result<RulesSet, ParseError> {
    match parse(token_string, grammar) {
        Ok(mut last_lang) => {
            if last_lang.is_final() && last_lang.has_completed_parses() {
                Ok(last_lang.take_completed_parses())
            } else {
                Err(ParseError)
            }
        },
        Err(e) => {Err(e)},
    }
}
