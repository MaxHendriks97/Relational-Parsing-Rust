use std::collections::{HashSet, BTreeSet, HashMap, BTreeMap, VecDeque};
use std::fmt;

mod word;
pub use word::*;
mod grammar;
pub use grammar::*;
mod regex;
pub use regex::*;
mod finite_state_automaton;
pub use finite_state_automaton::*;

type LangIdent = usize;
type Edges = BTreeSet<(State, LangIdent, Option<Rules>)>;
type CompletedParses = Option<BTreeSet<Rules>>;

#[derive(Debug)]
pub struct Language {
    id: LangIdent,
    edges: Edges,
    completed_parses: CompletedParses,
    fin: bool,
}

impl fmt::Display for Language {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "id: {}, final: {}\n", self.id, self.fin)?;
        if self.fin && self.completed_parses.is_some() {
            write!(f, "completed parses:\n")?;
            for rules in self.completed_parses.as_ref().unwrap() {
                write!(f, "  ")?;
                print_rules(&rules, f)?;
                write!(f, "\n")?;
            }
        }
        for (state, lang, opt_rules) in &self.edges {
            write!(f, "  ({}, {})", state, lang)?;
            if let Some(rules) = opt_rules {
                write!(f, " rules: ")?;
                print_rules(rules, f)?;
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

impl Language {
    pub fn new(id: LangIdent, edges: BTreeSet<(State, LangIdent, Option<Rules>)>, completed_parses: Option<BTreeSet<Rules>>, fin: bool) -> Language {
        Language {id, edges, completed_parses, fin}
    }

    pub fn extend_completed_parse(&mut self, other_completed_parse: CompletedParses) {
        if let Some(other_parses) = other_completed_parse {
            if let Some(parses) = self.completed_parses.as_mut() {
                parses.extend(other_parses);
            }
            else {
                self.completed_parses = Some(other_parses);
            }
        }
    }
}

#[derive(Debug)]
pub struct LanguageList {
    languages: Vec<Language>,
    highest_id: LangIdent,
}

impl fmt::Display for LanguageList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for language in &self.languages {
            write!(f, "{}\n", language)?;
        }
        Ok(())
    }
}

impl LanguageList {
    pub fn new() -> LanguageList {
        let mut languages: Vec<Language> = Vec::new();
        languages.push(Language{id: 0, edges: BTreeSet::new(), completed_parses: None, fin: true});
        LanguageList {languages, highest_id: 0}
    }

    pub fn get(&self, ident: LangIdent) -> Option<&Language> {
        self.languages.get(ident)
    }

    pub fn get_next_id(&self) -> LangIdent {
        self.highest_id + 1
    }

    pub fn insert_new_language(&mut self, lang: Language) {
        self.highest_id += 1;
        self.languages.push(lang);
    }

    pub fn curr_lang(&self) -> &Language {
        &self.languages[self.highest_id]
    }

    //pub fn insert_new_language(&mut self, edges: BTreeSet<(State, LangIdent, Option<Rules>)>, fin: bool) -> LangIdent {
    //    self.highest_id += 1;
    //    self.languages.push(Language{id: self.highest_id, edges, fin});
    //    self.highest_id
    //}

    //pub fn get_lang_ident_or_insert(&mut self, edges: BTreeSet<(State, LangIdent, Option<Rules>)>, fin: bool) -> usize {
    //    if let Some(lang_ident) = self.languages.iter().position(|l| l.fin == fin && l.edges == edges) {
    //        lang_ident
    //    } else {
    //        self.insert_new_language(edges, fin)
    //    }
    //}
}

pub struct ParseRound {
    deriv_language: Option<Language>,
    prep_deriv_language: Option<Language>,
    prep_language: Option<Language>,
}

impl ParseRound {
    pub fn new() -> ParseRound {
        //let deriv_language = Language::new(language_list.highest_id+1, None, false);
        //let prep_language = Language::new(language_list.highest_id+2, None, false);
        ParseRound{deriv_language: None, prep_deriv_language: None, prep_language: None}
    }

    pub fn register(self, language_list: &mut LanguageList, finite_state_automaton: &FiniteStateAutomaton) -> Result<(), &'static str> {
        //do e-simulation
        if let (Some(mut prep_deriv_lang), Some(mut prep_lang)) = (self.prep_deriv_language, self.prep_language) {
            if let Some(deriv_lang) = self.deriv_language {
                //ParseRound::e_sim(&mut deriv_lang, language_list, finite_state_automaton);
                //println!("deriv after e_sim: {}", deriv_lang);
                prep_lang.edges.extend(deriv_lang.edges);
                prep_lang.extend_completed_parse(deriv_lang.completed_parses);
                if deriv_lang.fin {
                    prep_lang.fin = true;
                }
            }
            ParseRound::prep_deriv_e_sim(&mut prep_deriv_lang, language_list, finite_state_automaton);
            println!("prep_deriv at end: {}", prep_deriv_lang);
            language_list.insert_new_language(prep_deriv_lang);

            ParseRound::e_sim(&mut prep_lang, language_list, finite_state_automaton);
            println!("prep at end: {}", prep_lang);
            language_list.insert_new_language(prep_lang);
        } else {
            if let Some(mut lang) = self.deriv_language {
                ParseRound::e_sim(&mut lang, language_list, finite_state_automaton);
                println!("deriv after e_sim: {}", lang);
                language_list.insert_new_language(lang);
            } else {
                return Err("Derivation failed");
            }
        }
        Ok(())
    }

    pub fn prep_deriv_e_sim(lang: &mut Language, language_list: &LanguageList, finite_state_automaton: &FiniteStateAutomaton) {
        let mut to_simulate: Vec<(State, LangIdent, Option<Rules>)> = lang.edges.clone().into_iter().collect();

        'sim: while let Some((source_state, dest_lang, opt_applied_rules)) = to_simulate.pop() {
            if let Some(destinations) = finite_state_automaton.simulate(&source_state, Symbol::Epsilon) {
                for (end_state, opt_new_rules, end_state_accepting) in destinations {
                    let dest_language: &Language = language_list.get(dest_lang).unwrap();
                    if !dest_language.fin {
                        continue;
                    }
                    if !finite_state_automaton.has_transition(end_state) {
                        if end_state_accepting {
                            for (state, dest_lang, opt_rules) in &dest_language.edges {
                                let opt_concat_rules = concatenate_opt_rules(&concatenate_opt_rules(opt_new_rules, &opt_applied_rules), opt_rules);
                                lang.edges.insert((*state, *dest_lang, opt_concat_rules.clone()));
                                to_simulate.push((*state, *dest_lang, opt_concat_rules));
                            }
                        }
                    } else if end_state_accepting {
                        if let Some(rules) = concatenate_opt_rules(opt_new_rules, &opt_applied_rules) {
                            lang.edges.insert((*end_state, dest_lang, Some(rules.clone())));
                            to_simulate.push((*end_state, dest_lang, Some(rules)));
                        } else {
                            lang.edges.insert((*end_state, dest_lang, None));
                            to_simulate.push((*end_state, dest_lang, None));
                        }
                        for (state, dest_lang, opt_rules) in &dest_language.edges {
                            let opt_concat_rules = concatenate_opt_rules(&concatenate_opt_rules(opt_new_rules, &opt_applied_rules), opt_rules);
                            lang.edges.insert((*state, *dest_lang, opt_concat_rules.clone()));
                            to_simulate.push((*state, *dest_lang, opt_concat_rules));
                        }
                    }
                }
            }
        }
    }

    pub fn e_sim(lang: &mut Language, language_list: &LanguageList, finite_state_automaton: &FiniteStateAutomaton) {
        let mut to_simulate: Vec<(State, LangIdent, Option<Rules>)> = lang.edges.clone().into_iter().collect();

        while let Some((source_state, dest_lang, opt_applied_rules)) = to_simulate.pop() {
            if let Some(destinations) = finite_state_automaton.simulate(&source_state, Symbol::Epsilon) {
                for (end_state, opt_new_rules, end_state_accepting) in destinations {
                    let dest_language: &Language = language_list.get(dest_lang).unwrap();
                    if !finite_state_automaton.has_transition(end_state) {
                        if end_state_accepting {
                            for (state, dest_lang, opt_rules) in &dest_language.edges {
                                let opt_concat_rules = concatenate_opt_rules(&concatenate_opt_rules(opt_new_rules, &opt_applied_rules), opt_rules);
                                lang.edges.insert((*state, *dest_lang, opt_concat_rules.clone()));
                                to_simulate.push((*state, *dest_lang, opt_concat_rules));
                            }
                        }
                        if dest_language.fin {
                            lang.fin = true;
                            if let Some(rules) = concatenate_opt_rules(opt_new_rules, &opt_applied_rules) {
                                lang.completed_parses.get_or_insert(BTreeSet::new()).insert(rules);
                            }
                        }
                    } else if end_state_accepting {
                        if let Some(rules) = concatenate_opt_rules(opt_new_rules, &opt_applied_rules) {
                            lang.completed_parses.get_or_insert(BTreeSet::new()).insert(rules.clone());
                            lang.edges.insert((*end_state, dest_lang, Some(rules.clone())));
                            to_simulate.push((*end_state, dest_lang, Some(rules)));
                        } else {
                            lang.edges.insert((*end_state, dest_lang, None));
                            to_simulate.push((*end_state, dest_lang, None));
                        }
                        if dest_language.fin {
                            lang.fin = true;
                        }
                        for (state, dest_lang, opt_rules) in &dest_language.edges {
                            let opt_concat_rules = concatenate_opt_rules(&concatenate_opt_rules(opt_new_rules, &opt_applied_rules), opt_rules);
                            lang.edges.insert((*state, *dest_lang, opt_concat_rules.clone()));
                            to_simulate.push((*state, *dest_lang, opt_concat_rules));
                        }
                    }
                }
            }
        }
    }

    pub fn derivative(&mut self, curr_lang: &Language, language_list: &LanguageList, symbol: Symbol, finite_state_automaton: &FiniteStateAutomaton) -> (Edges, CompletedParses, bool) {
        let mut edges: Edges = BTreeSet::new();
        let mut completed_parses: CompletedParses = None;
        let mut fin: bool = false;

        for (start_state, dest_lang, opt_applied_rules) in &curr_lang.edges {
            if let Some(destinations) = finite_state_automaton.simulate(start_state, symbol) {
                for (end_state, opt_new_rules, end_state_accepting) in destinations {
                    let dest_language: &Language = language_list.get(*dest_lang).unwrap();
                    if !finite_state_automaton.has_transition(end_state) {
                        if end_state_accepting {
                            for (state, lang, opt_rules) in &dest_language.edges {
                                edges.insert((*state, *lang, concatenate_opt_rules(&concatenate_opt_rules(opt_new_rules, opt_applied_rules), opt_rules)));
                            }
                        }
                        if dest_language.fin {
                            fin = true;
                            if let Some(rules) = concatenate_opt_rules(opt_new_rules, opt_applied_rules) {
                                println!("Writing completed parses");
                                completed_parses.get_or_insert(BTreeSet::new()).insert(rules);
                            }
                        }
                    } else if end_state_accepting {
                        if let Some(rules) = concatenate_opt_rules(opt_new_rules, opt_applied_rules) {
                            println!("Writing completed parses");
                            completed_parses.get_or_insert(BTreeSet::new()).insert(rules.clone());
                            edges.insert((*end_state, *dest_lang, Some(rules)));
                        } else {
                            edges.insert((*end_state, *dest_lang, None));
                        }
                        if dest_language.fin {
                            fin = true;
                        }
                        for (state, lang, opt_rules) in &dest_language.edges {
                            edges.insert((*state, *lang, concatenate_opt_rules(&concatenate_opt_rules(opt_new_rules, opt_applied_rules), opt_rules)));
                        }
                    } else {
                        edges.insert((*end_state, *dest_lang, concatenate_opt_rules(opt_new_rules, opt_applied_rules)));
                    }
                }
            }
        }

        (edges, completed_parses, fin)
        
    }

    pub fn derive(&mut self, language_list: &LanguageList, terminal: Terminal, finite_state_automaton: &FiniteStateAutomaton) {
        let curr_lang = language_list.curr_lang();
        let (edges, completed_parses, fin) = self.derivative(curr_lang, language_list, Symbol::Terminal(terminal), finite_state_automaton);
        if !edges.is_empty() || fin{
            self.deriv_language = Some(Language::new(curr_lang.id + 1, edges, completed_parses, fin));
        }
    }

    pub fn prep_derive(&mut self, language_list: &LanguageList, nonterminal: Nonterminal, finite_state_automaton: &FiniteStateAutomaton) -> bool {
        let curr_lang = language_list.curr_lang();
        let (edges, completed_parses, fin) = self.derivative(curr_lang, language_list, Symbol::Nonterminal(nonterminal), finite_state_automaton);

        if !edges.is_empty() || fin {
            if let Some(mut lang) = self.prep_deriv_language.as_mut() {
                lang.edges.extend(edges);
                lang.extend_completed_parse(completed_parses);
                if fin {
                    lang.fin = true;
                }
                //ParseRound::e_sim(lang, language_list, finite_state_automaton);
            } else {
                let mut res_lang = Language::new(curr_lang.id + 1, edges, completed_parses, fin);
                //ParseRound::e_sim(&mut res_lang, language_list, finite_state_automaton);
                self.prep_deriv_language = Some(res_lang);
            }
            true
        } else {
            false
        }
    }

    pub fn prepend(&mut self, atomic: (&State, &Option<HashSet<Rules>>, bool)) {
        if let Some(prep_deriv_language) = &self.prep_deriv_language {
            let mut edges: Edges = BTreeSet::new();
            let mut fin: bool = false;
            let mut completed_parses: CompletedParses = None;

            if let Some(rules_set) = &atomic.1 {
                if prep_deriv_language.edges.is_empty() && prep_deriv_language.fin {
                    for rules in rules_set {
                        edges.insert((*atomic.0, 0, Some(rules.clone())));
                    }
                } else {
                    for rules in rules_set {
                        edges.insert((*atomic.0, prep_deriv_language.id, Some(rules.clone())));
                    }
                }
            } else {
                if prep_deriv_language.edges.is_empty() && prep_deriv_language.fin {
                    edges.insert((*atomic.0, 0, None));
                } else {
                    edges.insert((*atomic.0, prep_deriv_language.id, None));
                }
            }

            if atomic.2 {
                if let Some(rules_set) = atomic.1 {
                    for rules in rules_set {
                        for (state, lang, opt_rules) in prep_deriv_language.edges.clone() {
                            edges.insert((state, lang, concatenate_opt_rules(&Some(rules.clone()), &opt_rules)));
                        }
                        if let Some(comp_parse_set) = &prep_deriv_language.completed_parses {
                            for parse in comp_parse_set {
                                completed_parses.get_or_insert(BTreeSet::new()).insert([rules.clone(), parse.clone()].concat());
                            }
                        }
                    }
                }
                if prep_deriv_language.fin {
                    fin = true;
                    if let Some(comp_parse_set) = &prep_deriv_language.completed_parses {
                        completed_parses.get_or_insert(BTreeSet::new()).extend(comp_parse_set.clone());
                    }
                    edges.extend(prep_deriv_language.edges.clone());
                }
            }

            self.prep_language = Some(Language::new(prep_deriv_language.id + 1, edges, completed_parses, fin));
        }
    }

}

fn epsilon(language_list: LanguageList) -> bool {
    language_list.curr_lang().fin
}

fn concatenate_opt_rules(first: &Option<Rules>, second: &Option<Rules>) -> Option<Rules> {
    match (first, second) {
        (None, None) => None,
        (res@Some(_), None) => res.clone(),
        (None, res@Some(_)) => res.clone(),
        (Some(first_rules), Some(second_rules)) => Some([first_rules.clone(), second_rules.clone()].concat()),
    }
}

pub fn g_accepts_string(token_string: Vec<Terminal>, grammar: &Grammar) -> bool {
    let finite_state_automaton: &FiniteStateAutomaton = &grammar.finite_state_automaton;
    let mut language_list: LanguageList = LanguageList::new();

    let (start_state, start_accepting) = finite_state_automaton.get_start();
    language_list.insert_new_language(Language::new(language_list.highest_id+1, BTreeSet::from([(start_state, 0, None)]), None, start_accepting));

    for token in token_string {
        println!("Next token: {}", token);
        let mut curr: ParseRound = ParseRound::new();

        curr.derive(&language_list, token, finite_state_automaton);

        for nonterminal in &grammar.nonterminals {
            if let Some(atomic) = finite_state_automaton.get_atomic(Symbol::Nonterminal(*nonterminal), token) {
                println!("Found atomic: [{}]^({})", nonterminal, token);
                if curr.prep_derive(&language_list, *nonterminal, finite_state_automaton) {
                    curr.prepend(atomic);
                }
            }
        }

        if let Some(deriv) = &curr.deriv_language {
            println!("Got deriv: {}", deriv);
        } else {
            println!("No deriv made");
        }

        if let Some(prep_deriv) = &curr.prep_deriv_language {
            println!("Got prep_deriv: {}", prep_deriv);
        } else {
            println!("No prep_deriv_made");
        }
        
        if let Some(prep) = &curr.prep_language {
            println!("Got prep: {}", prep);
        } else {
            println!("No prep made");
        }

        match curr.register(&mut language_list, finite_state_automaton) {
            Ok(_) => {},
            Err(e) => {
                println!("{}", e);
                return false;
            }
        }
        
    }
//
    println!("{}", language_list);
//    let final_lang = calculate_final_language(lang, &language_list, finite_state_automaton);
//    println!("Final: {}", final_lang);
    epsilon(language_list)
}

//fn edges_with_simulated_epsilon(language: &Language, language_list: &LanguageList, finite_state_automaton: &FiniteStateAutomaton) -> BTreeSet<(State, LangIdent, Option<Rules>)> {
//    let mut res: BTreeSet<(State, LangIdent, Option<Rules>)> = BTreeSet::new();
//
//    let mut to_process: Vec<(State, LangIdent, Option<Rules>)> = language.edges.clone().into_iter().collect();
//    //println!("To_process: {:?}", to_process);
//    while let Some((source_state, dest_lang, opt_applied_rules)) = to_process.pop() {
//        if let Some(destinations) = finite_state_automaton.simulate(&source_state, Symbol::Epsilon) {
//            for (dest_state, opt_rules, accepting_state) in destinations {
//                to_process.push((*dest_state, dest_lang, concatenate_opt_rules(opt_rules, &opt_applied_rules)));
//                if accepting_state {
//                    let dest_lang = language_list.get(dest_lang).unwrap();
//                    for (source_state, target_lang, oldest_rules) in &dest_lang.edges {
//                        to_process.push((*source_state, *target_lang, 
//                            concatenate_opt_rules(oldest_rules, opt_rules)
//                        ));
//                    }
//                }
//            }
//        }
//        if finite_state_automaton.has_transition(&source_state) {
//            res.insert((source_state, dest_lang, opt_applied_rules));
//        }
//    }
//    //println!("Res: {:?}", res);
//    println!("Simulated: {:?}", res);
//    res
//}
//
//fn calculate_final_language(curr_lang: LangIdent, language_list: &LanguageList, finite_state_automaton: &FiniteStateAutomaton) -> Language {
//    let language = language_list.get(curr_lang).unwrap();
//    let mut edges: BTreeSet<(State, LangIdent, Option<Rules>)> = BTreeSet::new();
//    let fin: bool = true;
//    let mut accepting: bool = language.accepting;
//
//    println!();
//    println!("calculating final");
//    let mut to_process: Vec<(State, LangIdent, Option<Rules>)> = language.edges.clone().into_iter().collect();
//    while let Some((source_state, dest_lang, opt_applied_rules)) = to_process.pop() {
//        if language_list.get(dest_lang).unwrap().accepting {
//            accepting = true;
//        }
//        println!("Currently processing: ({},{})", source_state, dest_lang);
//        if finite_state_automaton.is_accepting(&source_state) {
//            for (dest_state, dest_lang, opt_rules) in &language_list.get(dest_lang).unwrap().edges {
//                to_process.push((*dest_state, *dest_lang, opt_rules.clone()));
//                    //concatenate_opt_rules(opt_rules, &opt_applied_rules)));
//            }
//        }
//        if let Some(destinations) = finite_state_automaton.simulate(&source_state, Symbol::Epsilon) {
//            for (dest_state, opt_rules, accepting_state) in destinations {
//                to_process.push((*dest_state, dest_lang, concatenate_opt_rules(opt_rules, &opt_applied_rules)));
//                //if accepting_state {
//                //    let dest_lang = language_list.get(dest_lang).unwrap();
//                //    if dest_lang.accepting {
//                //        accepting = true;
//                //    }
//                //    for (source_state, target_lang, opt_rules) in &dest_lang.edges {
//                //        to_process.push((*source_state, *target_lang, 
//                //            concatenate_opt_rules(opt_rules, &opt_applied_rules)
//                //        ));
//                //    }
//                //}
//            }
//        }
//        if finite_state_automaton.is_accepting(&source_state) && dest_lang == 0 {
//            edges.insert((source_state, dest_lang, opt_applied_rules.clone()));
//        }
//    }
//
//    Language::new(0, Some(edges), fin, accepting)
//}
//
//fn derivative(curr_lang: LangIdent, language_list: &mut LanguageList, symbol: Symbol, finite_state_automaton: &FiniteStateAutomaton) -> (BTreeSet<(State, LangIdent, Option<Rules>)>, bool, bool) {
//    //println!("Symb: {}", symbol);
//    let language = language_list.get(curr_lang).unwrap();
//    let mut new_edges: BTreeSet<(State, LangIdent, Option<Rules>)> = BTreeSet::new();
//    let mut fin: bool = true;
//    let mut accepting: bool = false;
//
//    //for (source_state, dest_lang, opt_applied_rules )in &language.edges {
//    for (source_state, dest_lang, opt_applied_rules) in edges_with_simulated_epsilon(language, language_list, finite_state_automaton) {
//        if let Some(destinations) = finite_state_automaton.simulate(&source_state, symbol) {
//            for (dest_state, opt_rules, accepting_state) in destinations {
//                if finite_state_automaton.has_transition(dest_state) {
//                    fin = false;
//                }
//                if !accepting_state || accepting_state && language_list.get(dest_lang).unwrap().fin {
//                    new_edges.insert((*dest_state, dest_lang, concatenate_opt_rules(opt_rules, &opt_applied_rules)));
//                }
//                if accepting_state {
//                    let dest_lang = language_list.get(dest_lang).unwrap();
//                    for (source_state, target_lang, oldest_rules) in &dest_lang.edges {
//                        if finite_state_automaton.has_transition(source_state) {
//                            new_edges.insert((*source_state, *target_lang, 
//                                concatenate_opt_rules(opt_rules, oldest_rules)
//                            ));
//                        }
//                    }
//                    //new_edges.extend(dest_lang.edges.clone());
//                    if dest_lang.accepting {
//                        accepting = true;
//                    }
//                }
//            }
//        }
//    }
//
//    (new_edges, fin, accepting)
//
//    //if !new_edges.is_empty() {
//    //    Some(language_list.get_lang_ident_or_insert(new_edges, fin, accepting))
//    //} else {
//    //    None
//    //}
//}
//
//fn prepend(deriv_edges: &BTreeSet<(State, LangIdent, Option<Rules>)>, deriv_fin: bool, deriv_accepting: bool, base_deriv_edges: &BTreeSet<(State, LangIdent, Option<Rules>)>, deriv_id: LangIdent, language_list: &mut LanguageList, atomic: (&usize, &Option<HashSet<Rules>>, bool), finite_state_automaton: &FiniteStateAutomaton) -> (BTreeSet<(State, LangIdent, Option<Rules>)>, bool, bool) {
//    let (dest_state, opt_rules_list, accepting) = atomic;
//    let mut new_edges: BTreeSet<(State, LangIdent, Option<Rules>)> = BTreeSet::new();
//    let mut fin: bool = false;
//    let mut new_accepting: bool = false;
//
//    println!("Base: {:?}", base_deriv_edges);
//
//    if accepting && deriv_accepting {
//        for (source_state, target_lang, opt_rules) in deriv_edges {
//            if finite_state_automaton.has_transition(dest_state) {
//                if let Some(rules_list) = opt_rules_list {
//                    for rules in rules_list {
//                        new_edges.insert((*dest_state, *target_lang, 
//                            opt_rules.as_ref().map_or_else(|| Some(rules.clone()), |applied_rules| Some([rules.clone(), applied_rules.clone()].concat()))
//                        ));
//                    }
//                } else {
//                    new_edges.insert((*dest_state, *target_lang, 
//                        opt_rules.as_ref().map(|applied_rules| applied_rules.clone())
//                    ));
//                }
//            }
//        }
//    }
//    for (source_state, target_lang, opt_rules) in deriv_edges {
//        if let Some(rules_list) = opt_rules_list {
//            if finite_state_automaton.has_transition(dest_state) {
//                for rules in rules_list {
//                    if deriv_fin {
//                        new_edges.insert((*dest_state, *target_lang, 
//                            opt_rules.as_ref().map_or_else(|| Some(rules.clone()), |applied_rules| Some([rules.clone(), applied_rules.clone()].concat()))
//                        ));
//                    } else {
//                        new_edges.insert((*dest_state, deriv_id, 
//                            opt_rules.as_ref().map_or_else(|| Some(rules.clone()), |applied_rules| Some([rules.clone(), applied_rules.clone()].concat()))
//                        ));
//                    }
//                }
//            }
//        } else {
//            if finite_state_automaton.has_transition(dest_state) {
//                if deriv_fin {
//                    new_edges.insert((*dest_state, *target_lang, 
//                        opt_rules.as_ref().map(|applied_rules| applied_rules.clone())
//                    ));
//                } else {
//                    new_edges.insert((*dest_state, deriv_id, 
//                        opt_rules.as_ref().map(|applied_rules| applied_rules.clone())
//                    ));
//                }
//            }
//        }
//    }
//
//    //if finite_state_automaton.has_transition(dest_state) {
//    //    if let Some(rules_list) = opt_rules_list {
//    //        for rules in rules_list {
//    //            new_edges.insert((*dest_state, deriv_id, Some(rules.clone())));
//    //        }
//    //    } else {
//    //        new_edges.insert((*dest_state, deriv_id, None));
//    //    }
//    //}
//
//    if accepting {
//        if deriv_accepting {
//            new_accepting = true;
//        }
//    }
//
//    (new_edges, fin, new_accepting)
//}
//
//pub fn g_accepts_string(token_string: Vec<Terminal>, grammar: &Grammar) -> bool {
//    let finite_state_automaton: &FiniteStateAutomaton = &grammar.finite_state_automaton;
//    let mut language_list: LanguageList = LanguageList::new();
//
//    let (start_state, start_accepting) = finite_state_automaton.get_start();
//    let mut lang: LangIdent = language_list.insert_new_language(BTreeSet::from([(start_state, 0, None)]), false, start_accepting);
//
//    for token in token_string {
//        println!("lang: {}", language_list.get(lang).unwrap());
//        println!("Next token: {}", token);
//
//        let (base_deriv_edges, mut deriv_fin, mut deriv_accepting) = derivative(lang, &mut language_list, Symbol::Terminal(token), &finite_state_automaton);
//        let mut prepend_edges: BTreeSet<(State, LangIdent, Option<Rules>)> = BTreeSet::new();
//        let mut prepend_fin: bool = true;
//        let mut prepend_accepting: bool = false;
//
//        let mut deriv_edges: Edges = BTreeSet::new();
//        for nonterminal in &grammar.nonterminals {
//            if let Some(atomic) = finite_state_automaton.get_atomic(Symbol::Nonterminal(*nonterminal), token) {
//                println!("Found atomic: [{}]^({})", nonterminal, token);
//                let (new_edges, new_fin, new_accepting) = derivative(lang, &mut language_list, Symbol::Nonterminal(*nonterminal), &finite_state_automaton);
//                println!("Deriv: {:?}", new_edges);
//
//                if !new_edges.is_empty() {
//                    let (new_prepend_edges, new_prepend_fin, new_prepend_accepting) = prepend(&new_edges, new_fin, new_accepting, &base_deriv_edges, language_list.get_next_id(), &mut language_list, atomic, finite_state_automaton);
//                    println!("Prep: {:?}", new_prepend_edges);
//                    if !new_prepend_edges.is_empty() {
//                        deriv_edges.extend(new_edges);
//                        if !new_fin {
//                            deriv_fin = false;
//                        }
//                        if new_accepting {
//                            deriv_accepting = true;
//                        }
//                        prepend_edges.extend(new_prepend_edges);
//                        if !new_prepend_fin {
//                            prepend_fin = false;
//                        }
//                        if new_prepend_accepting {
//                            prepend_accepting = true;
//                        }
//                    }
//                }
//            }
//        }
//        deriv_edges.extend(base_deriv_edges);
//        println!("new deriv edges: {:?}", deriv_edges);
//        println!("new prepend edges: {:?}", prepend_edges);
//
//        if !deriv_edges.is_empty() {
//            lang = language_list.insert_new_language(deriv_edges, deriv_fin, deriv_accepting);
//        }
//        if !prepend_edges.is_empty() {
//            lang = language_list.insert_new_language(prepend_edges, prepend_fin, prepend_accepting);
//        }
//        
//    }
//
//    println!("{}", language_list);
//    let final_lang = calculate_final_language(lang, &language_list, finite_state_automaton);
//    println!("Final: {}", final_lang);
//    epsilon(final_lang)
//}

//type Configuration = (VecDeque<(State, bool)>, Rules);
//type Language = HashSet<Configuration>;
//
//pub fn print_language(language: &Language) {
//    println!("lang:");
//    for (configuration, applied_rules) in language {
//        print!("    [");
//        for (state, accepting) in configuration {
//            print!("({}, {})  ", state, accepting);
//        }
//        for (nonterminal, rule) in applied_rules {
//            print!("({} -> ", nonterminal);
//            for symbol in rule {
//                print!("{}", symbol);
//            }
//            print!(")");
//        }
//        println!("]");
//    }
//}
//
//pub fn prepend(finite_state_automaton: &FiniteStateAutomaton, symbol: Symbol, token: Terminal, language: &Language) -> Language {
//    let mut res: Language = HashSet::new();
//    if let Some((atomic, opt_rule_set, accepting)) = finite_state_automaton.get_atomic(symbol, token) {
//        match (finite_state_automaton.has_transition(atomic), opt_rule_set) {
//            (true, None) => {
//                for (curr_conf, applied_rules) in language {
//                    let mut new_conf = curr_conf.clone();
//                    new_conf.push_front((*atomic, accepting));
//                    res.insert((new_conf, applied_rules.clone()));
//                    //res.extend(derivative(&(new_conf, applied_rules), Symbol::Epsilon, finite_state_automaton));
//                    res.extend(one_state_derivative(curr_conf, applied_rules, *atomic, Symbol::Epsilon, finite_state_automaton));
//                }
//            },
//            (false, Some(rule_set)) => {
//                for (curr_conf, applied_rules) in language {
//                    for rules in rule_set {
//                        res.insert((curr_conf.clone(), [rules.clone(), applied_rules.clone()].concat()));
//                    }
//                }
//            },
//            (true, Some(rule_set)) => {
//                for (curr_conf, applied_rules) in language {
//                    for rules in rule_set {
//                        let mut new_conf = curr_conf.clone();
//                        new_conf.push_front((*atomic, accepting));
//                        res.insert((new_conf, [rules.clone(), applied_rules.clone()].concat()));
//                    }
//                }
//            },
//            _ => res = language.clone(),
//        }
//    }
//    res
//}
//
//pub fn one_state_derivative(curr_configuration: &VecDeque<(State, bool)>, rules: &Rules, curr_state: State, symbol: Symbol, finite_state_automaton: &FiniteStateAutomaton) -> Language {
//    let mut res: Language = HashSet::new();
//
//    if let Some(destinations) = finite_state_automaton.simulate(&curr_state, symbol) {
//        for (dest, opt_rules, accepting) in destinations {
//            let mut new_configuration = curr_configuration.clone();
//            if finite_state_automaton.has_transition(dest) {
//                new_configuration.push_front((*dest, accepting));
//            }
//            if let Some(new_rules) = opt_rules {
//                let mut applied_rules = new_rules.clone();
//                applied_rules.extend(rules.clone());
//                //res.extend(one_state_derivative(curr_configuration, &applied_rules, *dest, Symbol::Epsilon, finite_state_automaton));
//                res.insert((new_configuration.into(), applied_rules));
//            } else {
//                //res.extend(one_state_derivative(curr_configuration, rules, *dest, Symbol::Epsilon, finite_state_automaton));
//                res.insert((new_configuration.into(), rules.clone()));
//            }
//        }
//    }
//
//    res
//}
//
//pub fn derive_e(language: &Language, finite_state_automaton: &FiniteStateAutomaton) -> Language {
//    let mut res: Language = language.clone();
//    let mut new_res = derivative(language, Symbol::Epsilon, finite_state_automaton);
//    while !new_res.is_empty() {
//        res.extend(new_res.clone());
//        new_res = derivative(&new_res, Symbol::Epsilon, finite_state_automaton);
//    }
//    res
//}
//
//pub fn derivative(language: &Language, symbol: Symbol, finite_state_automaton: &FiniteStateAutomaton) -> Language {
//    let mut res: Language = HashSet::new();
//    for (curr_conf, rules) in language {
//        let mut curr_configuration: VecDeque<(State, bool)> = curr_conf.clone().into();
//
//        while let Some((state, accepting)) = curr_configuration.pop_front() {
//            res.extend(one_state_derivative(&curr_configuration, rules, state, symbol, finite_state_automaton));
//            if !accepting {
//                break;
//            }
//        }
//    }
//
//    res
//}
//
//pub fn epsilon(language: &Language, finite_state_automaton: &FiniteStateAutomaton) -> bool {
//    'lang: for (configuration, _) in language {
//        for (_, accepting) in configuration {
//            if !accepting {
//                continue 'lang;
//            }
//        }
//        return true;
//    }
//    false
//}
//
//
//
//pub fn g_accepts_string(token_string: Vec<Terminal>, grammar: &Grammar) -> bool {
//    let finite_state_automaton: &FiniteStateAutomaton = &grammar.finite_state_automaton;
//    let symbols: &HashSet<Symbol> = &grammar.symbols;
//    let mut lang: Language = HashSet::from([(VecDeque::from([finite_state_automaton.get_start()]), Vec::new())]);
//    for token in token_string {
//        print_language(&lang);
//        //println!("lang: {:?}", lang);
//        let mut new_lang: Language = HashSet::new();
//        for symbol in symbols {
//            new_lang.extend(prepend(finite_state_automaton, *symbol, token, &derivative(&derive_e(&lang, finite_state_automaton), *symbol, finite_state_automaton)))
//            //if let Some((atomic, opt_rule_set, accepting)) = finite_state_automaton.get_atomic(*symbol, token) {
//            //    if finite_state_automaton.has_transition(atomic) {
//            //        new_lang.extend(prepend(Some((*atomic, accepting)), opt_rule_set, &derivative(&lang, *symbol, &finite_state_automaton)));
//            //    } else {
//            //        new_lang.extend(prepend(None, opt_rule_set, &derivative(&lang, *symbol, &finite_state_automaton)));
//            //    }
//            //}
//        }
//        lang = new_lang;
//    }
//    lang = derive_e(&lang, finite_state_automaton);
//    print_language(&lang);
//    //println!("lang: {:?}", lang);
//    epsilon(&lang, finite_state_automaton)
//}

