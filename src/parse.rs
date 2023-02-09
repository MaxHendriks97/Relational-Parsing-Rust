//! # parse
//! 
//! The `parse` module contains the ParseRound data structure used during parsing and contains all methods necessary for parsing.
//! This module memoizes during parsing.
use std::collections::{HashSet, HashMap, BTreeMap};

use crate::*;

pub struct ParseRound {
    deriv: Option<Language>,
    prep_deriv: Option<Language>,
    prep: Option<Language>,
    memo: MemoBuilder,
}

impl ParseRound {
    pub fn new() -> ParseRound {
        ParseRound{deriv: None, prep_deriv: None, prep: None, memo: MemoBuilder::new()}
    }

    pub fn register(mut self, language_list: &mut LanguageList, finite_state_automaton: &FiniteStateAutomaton) -> Result<Memo, ParseError> {
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
                        self.memo.increase_pop();
                        language_list.pop_lang();
                    }
                }
                ParseRound::e_sim(&mut prep, &language_list, finite_state_automaton);
                language_list.insert_new_language(prep);
            }
            Ok(self.memo.build_memo())
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
                Ok(self.memo.build_memo())
            } else {
                return Err(ParseError);
            }
        }
    }

    pub fn e_sim(lang: &mut Language, language_list: &LanguageList, finite_state_automaton: &FiniteStateAutomaton) {
        let mut to_simulate: Vec<((State, Depth), HashSet<Rules>)> = lang.edges_ref().clone().into_iter().collect();

        while let Some(((source_state, dest_depth), applied_rules_set)) = to_simulate.pop() {
            if let Some(destinations) = finite_state_automaton.simulate(&source_state, Symbol::Epsilon) {
                for (end_state, new_rules, end_state_accepting) in destinations {
                    let dest_language: &Language = language_list.get(dest_depth).unwrap();
                    let res_rules_set: HashSet<Rules> = prepend_rules_to_rules_set(new_rules, &applied_rules_set);
                    if end_state_accepting {
                        if dest_language.is_final() {
                            lang.set_final();
                            lang.extend_completed_parses(res_rules_set.clone().into_iter());
                        } else {
                            for ((state, depth), rules_set) in dest_language.edges_ref() {
                                let new_res_rules_set: HashSet<Rules> = concatenate_rules_sets(&res_rules_set, rules_set);
                                lang.extend_edge((*state, dest_depth + *depth), new_res_rules_set.clone());
                                to_simulate.push(((*state, dest_depth + *depth), new_res_rules_set));
                            }
                        }
                    }
                    if finite_state_automaton.has_edge(end_state) {
                        lang.extend_edge((*end_state, dest_depth), res_rules_set.clone());
                        to_simulate.push(((*end_state, dest_depth), res_rules_set));
                    }
                }
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
                            self.memo.insert_deriv_accepting((*start_state, *end_depth), new_rules.clone());
                            deriv.set_final();
                        } else {
                            for ((state, depth), rules_set) in dest_language.edges_ref() {
                                deriv.extend_edge((*state, *depth + *end_depth), concatenate_rules_sets(&res_rules_set, rules_set));
                                self.memo.insert_deriv_accepting((*start_state, *end_depth), new_rules.clone());
                            }
                        }
                    }
                    if finite_state_automaton.has_edge(end_state) {
                        deriv.extend_edge((*end_state, *end_depth), res_rules_set.clone());
                        self.memo.insert_deriv_memo((*start_state, *end_depth), (*end_state, *end_depth), end_state_accepting, new_rules.clone());
                    }

                }
            }
        }

        if deriv.has_edges() || deriv.is_final() {
            self.deriv = Some(deriv);
        }
    }

    pub fn prep_derive(&mut self, curr_lang: &Language, language_list: &LanguageList, nonterminal: Nonterminal, finite_state_automaton: &FiniteStateAutomaton) -> (bool, MemPart) {
        let prep_deriv: &mut Language = self.prep_deriv.get_or_insert(Language::new());
        let mut edge_to_edges: MemPart = HashMap::new();
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
                        if new_rules.is_empty() {
                            edge_to_edges.entry((*start_state, *end_depth)).or_default().entry(((*end_state, *end_depth), end_state_accepting)).or_default(); 
                        } else {
                            edge_to_edges.entry((*start_state, *end_depth)).or_default().entry(((*end_state, *end_depth), end_state_accepting)).or_default().insert(new_rules.clone()); 
                        }
                    }

                }
            }
        }

        (ret, edge_to_edges)
    }

    pub fn prepend(&mut self, atomic: (&State, &HashSet<Rules>, bool), language_list: &LanguageList, finite_state_automaton: &FiniteStateAutomaton, prep_deriv_mempart: MemPart) {
        if let Some(prep_deriv) = &self.prep_deriv {
            let mut prep: Language = Language::new();

            let memo: Edge = (*atomic.0, 1);
            if finite_state_automaton.has_edge(atomic.0) {
                match (atomic.1.is_empty(), atomic.2 && (prep_deriv.is_final() || language_list.get(1).map_or(false, |l| l.is_final()))) {
                    (true, true) => {
                        prep.insert_edge(memo, None);
                        prep.set_final();
                        self.memo.insert_prepend_edges(memo, atomic.2, None);
                    },
                    (true, false) => {
                        prep.insert_edge(memo, None);
                        self.memo.insert_prepend_edges(memo, atomic.2, None);
                    },
                    (false, true) => {
                        for rules in atomic.1 {
                            prep.insert_edge(memo, Some(rules.clone()));
                            prep.insert_completed_parse(rules.clone());
                            self.memo.insert_prepend_edges(memo, atomic.2, Some(rules.clone()));
                        }
                        prep.set_final();
                    },
                    (false, false) => {
                        for rules in atomic.1 {
                            prep.insert_edge(memo, Some(rules.clone()));
                            self.memo.insert_prepend_edges(memo, atomic.2, Some(rules.clone()));
                        }
                    },
                }
            }

            if atomic.2 {
                for rules in atomic.1 {
                    for (memedge, edges) in &prep_deriv_mempart {
                        for (((state, depth), accepting), mem_rules) in edges {
                            prep.extend_edge((*state, depth + 1), prepend_rules_to_rules_set(rules, prep_deriv.get_edge_rules(&(*state, *depth)).unwrap()));
                            self.memo.extend_prepend_memo(*memedge, (*state, depth + 1), *accepting, prepend_rules_to_rules_set(rules, mem_rules));
                        }
                    }
                }
            }

            self.prep = Some(prep);
        }
        self.memo.concat_prep_deriv_memo(prep_deriv_mempart);
    }

}

pub fn apply_memo(memo: &Memo, mut curr_lang: Language, language_list: &mut LanguageList, finite_state_automaton: &FiniteStateAutomaton) {
    //println!("Memoized");

    let mut fin: bool = false;

    for _ in 0..memo.get_pops() {
        language_list.pop_lang();
    }

    if let Some(memo) = memo.get_extra_lang_memo() {
        let mut edges: Edges = BTreeMap::new();

        for (edge, rules) in curr_lang.edges_ref() {
            if let Some(new_edges) = memo.get(&edge) {
                for ((new_edge, accepting), new_rules) in new_edges {
                    let res_rules_set: RulesSet = concatenate_rules_sets(new_rules, rules);
                    if *accepting {
                        if let Some(dest_language) = language_list.get(new_edge.1) {
                            if dest_language.not_final() {
                                for ((state, depth), rules_set) in dest_language.edges_ref() {
                                    edges.entry((*state, *depth + new_edge.1)).or_default().extend(concatenate_rules_sets(&res_rules_set, rules_set));
                                }
                            } else {
                                fin = true;
                            }
                        }
                    }
                    edges.insert(*new_edge, res_rules_set);
                }

            }
        }

        language_list.insert_new_language(Language::new_from(edges, HashSet::new(), false));
    }

    let mut edges: Edges = BTreeMap::new();
    let mut completed_parses: CompletedParses = HashSet::new();

    for (edge, rules) in curr_lang.take_edges() {
        if let Some((new_edges, opt_acc)) = memo.get_new_memo_edge(&edge) {
            for ((new_edge, accepting), new_rules) in new_edges {
                let res_rules_set: RulesSet = concatenate_rules_sets(new_rules, &rules);
                if *accepting {
                    if let Some(dest_language) = language_list.get(new_edge.1) {
                        if dest_language.not_final() {
                            for ((state, depth), rules_set) in dest_language.edges_ref() {
                                edges.entry((*state, *depth + new_edge.1)).or_default().extend(concatenate_rules_sets(&res_rules_set, rules_set));
                            }
                        } else {
                            completed_parses.extend(res_rules_set.clone());
                            fin = true;
                        }
                    }
                }
                edges.entry((new_edge.0, new_edge.1)).or_default().extend(res_rules_set);
            }
            if let Some((depth, new_rules)) = opt_acc {
                if let Some(dest_lang) = language_list.get(*depth) {
                    let res_rules_set: RulesSet = concatenate_rules_sets(new_rules, &rules);
                    if dest_lang.is_final() {
                        completed_parses.extend(res_rules_set.clone());
                        fin = true;
                    }
                    for ((state, dest_depth), rules_set) in dest_lang.edges_ref() {
                        edges.entry((*state, *dest_depth + *depth)).or_default().extend(concatenate_rules_sets(&res_rules_set, rules_set));
                    }
                }
            }
        }
    }

    for ((edge, accepting), rules) in memo.get_extra_edges() {
        //println!("Extra edge: ({}, {}), new_rules: {:?}", edge.0, edge.1, rules);
        if *accepting {
            if let Some(dest_language) = language_list.get(edge.1) {
                if dest_language.not_final() {
                    for ((state, depth), rules_set) in dest_language.edges_ref() {
                        edges.entry((*state, *depth + edge.1)).or_default().extend(concatenate_rules_sets(rules, rules_set));
                    }
                } else {
                    completed_parses.extend(rules.clone());
                    fin = true;
                }
            }
        }
        edges.insert(*edge, rules.clone());
    }

    let mut final_lang: Language = Language::new_from(edges, completed_parses, fin);
    ParseRound::e_sim(&mut final_lang, language_list, finite_state_automaton);

    language_list.insert_new_language(final_lang);
}

#[derive(Debug, PartialEq, Eq)]
pub struct ParseError;

pub fn parse(token_string: &Vec<Terminal>, grammar: &Grammar, memoize: &mut Memoize) -> Result<Language, ParseError> {
    let finite_state_automaton: &FiniteStateAutomaton = &grammar.finite_state_automaton;
    let mut language_list: LanguageList = LanguageList::new();

    let (start_state, start_accepting) = finite_state_automaton.get_start();
    language_list.insert_new_language(Language::new_from(BTreeMap::from([((start_state, 1), HashSet::new())]), HashSet::new(), start_accepting));

    for token in token_string {
        if let Some(curr_lang) = language_list.pop_lang() {
            //println!("Next token: {}", token);

            //if let Some(memo) = memoize.get_memo(curr_lang.make_mem_edges(), token) {
            if let Some(memo) = memoize.get_memo(curr_lang.edges_ref().keys().copied().collect(), *token) {
                apply_memo(memo, curr_lang, &mut language_list, finite_state_automaton);
            } else {
                let mut curr: ParseRound = ParseRound::new();
                curr.derive(&curr_lang, &language_list, *token, finite_state_automaton);

                for nonterminal in &grammar.nonterminals {
                    if let Some(atomic) = finite_state_automaton.get_atomic(Symbol::Nonterminal(*nonterminal), *token) {
                        //println!("Found atomic: [{}]^({})", nonterminal, token);
                        let prep_deriv = curr.prep_derive(&curr_lang, &language_list, *nonterminal, finite_state_automaton);
                        if prep_deriv.0 {
                            curr.prepend(atomic, &language_list, finite_state_automaton, prep_deriv.1);
                        }
                    }
                }

                match curr.register(&mut language_list, finite_state_automaton) {
                    Ok(memo) => {
                        memoize.memoize(curr_lang.make_mem_edges(), *token, memo);
                    },
                    Err(e) => {
                        return Err(e);
                    }
                }
            }
            
            //println!("End lang_list: {}", language_list);
            //println!("{}", memoize);

        } else {
            return Err(ParseError);
        }
        
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

pub fn g_accepts_string(token_string: &Vec<Terminal>, grammar: &Grammar, memoize: &mut Memoize) -> bool {
    match parse(token_string, grammar, memoize) {
        Ok(last_lang) => {last_lang.is_final()},
        Err(_) => {false}
    }
}

pub fn find_parses(token_string: &Vec<Terminal>, grammar: &Grammar, memoize: &mut Memoize) -> Result<CompletedParses, ParseError> {
    match parse(token_string, grammar, memoize) {
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

pub fn parse_count_memo(token_string: &Vec<Terminal>, grammar: &Grammar, memoize: &mut Memoize) -> (Result<Language, ParseError>, usize) {
    let finite_state_automaton: &FiniteStateAutomaton = &grammar.finite_state_automaton;
    let mut language_list: LanguageList = LanguageList::new();

    let (start_state, start_accepting) = finite_state_automaton.get_start();
    language_list.insert_new_language(Language::new_from(BTreeMap::from([((start_state, 1), HashSet::new())]), HashSet::new(), start_accepting));

    let mut memcount: usize = 0;

    for token in token_string {
        if let Some(curr_lang) = language_list.pop_lang() {
            if let Some(memo) = memoize.get_memo(curr_lang.edges_ref().keys().copied().collect(), *token) {
                apply_memo(memo, curr_lang, &mut language_list, finite_state_automaton);
                memcount += 1;
            } else {
                let mut curr: ParseRound = ParseRound::new();
                curr.derive(&curr_lang, &language_list, *token, finite_state_automaton);

                for nonterminal in &grammar.nonterminals {
                    if let Some(atomic) = finite_state_automaton.get_atomic(Symbol::Nonterminal(*nonterminal), *token) {
                        let prep_deriv = curr.prep_derive(&curr_lang, &language_list, *nonterminal, finite_state_automaton);
                        if prep_deriv.0 {
                            curr.prepend(atomic, &language_list, finite_state_automaton, prep_deriv.1);
                        }
                    }
                }

                match curr.register(&mut language_list, finite_state_automaton) {
                    Ok(memo) => {
                        memoize.memoize(curr_lang.make_mem_edges(), *token, memo);
                    },
                    Err(e) => {
                        return (Err(e), memcount);
                    }
                }
            }
        } else {
            return (Err(ParseError), memcount);
        }
        
    }

    if let Some(mut last_lang) = language_list.pop_lang() {
        ParseRound::e_sim(&mut last_lang, &language_list, finite_state_automaton);
        (Ok(last_lang), memcount)
    } else {
        (Err(ParseError), memcount)
    }
}

pub fn find_parses_count_memo(token_string: &Vec<Terminal>, grammar: &Grammar, memoize: &mut Memoize) -> (Result<CompletedParses, ParseError>, usize) {
    let (parse, memcount) = parse_count_memo(token_string, grammar, memoize);
    match parse {
        Ok(mut last_lang) => {
            if last_lang.is_final() && last_lang.has_completed_parses() {
                (Ok(last_lang.take_completed_parses().collect()), memcount)
            } else {
                (Err(ParseError), memcount)
            }
        },
        Err(e) => {(Err(e), memcount)}
    }
}

pub fn prepend_rules_to_rules_set(rules: &Rules, rules_set: &HashSet<Rules>) -> HashSet<Rules> {
    if rules_set.is_empty() && !rules.is_empty() {
        HashSet::from([rules.clone()])
    } else if rules.is_empty() {
        rules_set.clone()
    } else {
        let mut res: HashSet<Rules> = HashSet::new();
        for srules in rules_set {
            res.insert([rules.clone(), srules.clone()].concat());
        }
        res
    }
}

pub fn concatenate_rules_sets(first: &HashSet<Rules>, second: &HashSet<Rules>) -> HashSet<Rules> {
    if first.is_empty() {
        second.clone()
    } else if second.is_empty() {
        first.clone()
    } else {
        let mut res = HashSet::new();
        for frules in first {
            for srules in second {
                res.insert([frules.clone(), srules.clone()].concat());
            }
        }
        res
    }
}

