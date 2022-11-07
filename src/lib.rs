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
type Edges = HashMap<(State, LangIdent), HashSet<Rules>>;
type CompletedParses = HashSet<Rules>;

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
        if self.fin && !self.completed_parses.is_empty() {
            write!(f, "completed parses:\n")?;
            for rules in &self.completed_parses {
                write!(f, "        ")?;
                print_rules(&rules, f)?;
                write!(f, "\n")?;
            }
        }
        for ((state, lang), rules_set) in &self.edges {
            write!(f, "  ({}, {})", state, lang)?;
            if !rules_set.is_empty() {
                write!(f, " rules:\n")?;
                for rules in rules_set {
                    write!(f, "        ")?;
                    print_rules(rules, f)?;
                    write!(f, "\n")?;
                }
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

impl Language {
    pub fn new(id: LangIdent, edges: Edges, completed_parses: CompletedParses, fin: bool) -> Language {
        Language {id, edges, completed_parses, fin}
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
        languages.push(Language{id: 0, edges: HashMap::new(), completed_parses: HashSet::new(), fin: true});
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

}

pub struct ParseRound {
    deriv_language: Option<Language>,
    prep_deriv_language: Option<Language>,
    keep_prep_deriv: bool,
    prep_language: Option<Language>,
}

impl ParseRound {
    pub fn new() -> ParseRound {
        ParseRound{deriv_language: None, prep_deriv_language: None, keep_prep_deriv: false, prep_language: None}
    }

    pub fn register(self, language_list: &mut LanguageList, finite_state_automaton: &FiniteStateAutomaton) -> Result<(), &'static str> {
        if let (Some(mut prep_deriv_lang), Some(mut prep_lang)) = (self.prep_deriv_language, self.prep_language) {
            if let Some(deriv_lang) = self.deriv_language {
                prep_lang.edges.extend(deriv_lang.edges);
                prep_lang.completed_parses.extend(deriv_lang.completed_parses);
                if deriv_lang.fin {
                    prep_lang.fin = true;
                }
            }
            if self.keep_prep_deriv {
                ParseRound::prep_deriv_e_sim(&mut prep_deriv_lang, language_list, finite_state_automaton);
                println!("prep_deriv at end: {}", prep_deriv_lang);
                language_list.insert_new_language(prep_deriv_lang);
            }

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

    fn prepend_rules_to_rules_set(rules: &Rules, rules_set: &HashSet<Rules>) -> HashSet<Rules> {
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

    fn concatenate_rules_sets(first: &HashSet<Rules>, second: &HashSet<Rules>) -> HashSet<Rules> {
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

    fn prep_deriv_e_sim(lang: &mut Language, language_list: &LanguageList, finite_state_automaton: &FiniteStateAutomaton) {
        let mut to_simulate: Vec<((State, LangIdent), HashSet<Rules>)> = lang.edges.clone().into_iter().collect();

        while let Some(((source_state, dest_lang), applied_rules_set)) = to_simulate.pop() {
            if let Some(destinations) = finite_state_automaton.simulate(&source_state, Symbol::Epsilon) {
                for (end_state, new_rules, end_state_accepting) in destinations {
                    let dest_language: &Language = language_list.get(dest_lang).unwrap();
                    if !dest_language.fin {
                        continue;
                    }
                    if !finite_state_automaton.has_transition(end_state) {
                        if end_state_accepting {
                            for ((state, dest_lang), rules_set) in &dest_language.edges {
                                let res_rules_set: HashSet<Rules> = ParseRound::prepend_rules_to_rules_set(new_rules, &ParseRound::concatenate_rules_sets(&applied_rules_set, rules_set));
                                lang.edges.entry((*state, *dest_lang)).or_default().extend(res_rules_set.clone());
                                to_simulate.push(((*state, *dest_lang), res_rules_set));
                            }
                        }
                    } else if end_state_accepting {
                        let res_rules_set: HashSet<Rules> = ParseRound::prepend_rules_to_rules_set(new_rules, &applied_rules_set);
                        lang.edges.entry((*end_state, dest_lang)).or_default().extend(res_rules_set.clone());
                        to_simulate.push(((*end_state, dest_lang), res_rules_set.clone()));

                        for ((state, dest_lang), rules_set) in &dest_language.edges {
                            let new_res_rules: HashSet<Rules> = ParseRound::concatenate_rules_sets(&res_rules_set, rules_set);
                            lang.edges.entry((*state, *dest_lang)).or_default().extend(new_res_rules.clone());
                            to_simulate.push(((*state, *dest_lang), new_res_rules));
                        }
                    }
                }
            }
        }
    }

    pub fn e_sim(lang: &mut Language, language_list: &LanguageList, finite_state_automaton: &FiniteStateAutomaton) {
        let mut to_simulate: Vec<((State, LangIdent), HashSet<Rules>)> = lang.edges.clone().into_iter().collect();

        while let Some(((source_state, dest_lang), applied_rules_set)) = to_simulate.pop() {
            if let Some(destinations) = finite_state_automaton.simulate(&source_state, Symbol::Epsilon) {
                for (end_state, new_rules, end_state_accepting) in destinations {
                    let dest_language: &Language = language_list.get(dest_lang).unwrap();
                    if !finite_state_automaton.has_transition(end_state) {
                        if end_state_accepting {
                            for ((state, dest_lang), rules_set) in &dest_language.edges {
                                let res_rules_set: HashSet<Rules> = ParseRound::concatenate_rules_sets(&ParseRound::prepend_rules_to_rules_set(new_rules, &applied_rules_set), rules_set);
                                lang.edges.entry((*state, *dest_lang)).or_default().extend(res_rules_set.clone());
                                to_simulate.push(((*state, *dest_lang), res_rules_set));
                            }
                        }
                        if dest_language.fin {
                            lang.fin = true;
                            lang.completed_parses.extend(ParseRound::prepend_rules_to_rules_set(new_rules, &applied_rules_set));
                        }
                    } else if end_state_accepting {
                        let res_rules_set: HashSet<Rules> = ParseRound::prepend_rules_to_rules_set(new_rules, &applied_rules_set);
                        lang.completed_parses.extend(res_rules_set.clone());
                        lang.edges.entry((*end_state, dest_lang)).or_default().extend(res_rules_set.clone());
                        to_simulate.push(((*end_state, dest_lang), res_rules_set.clone()));
                        if dest_language.fin {
                            lang.fin = true;
                        }
                        for ((state, dest_lang), rules_set) in &dest_language.edges {
                            let new_res_rules_set: HashSet<Rules> = ParseRound::concatenate_rules_sets(&res_rules_set, rules_set);
                            lang.edges.entry((*state, *dest_lang)).or_default().extend(new_res_rules_set.clone());
                            to_simulate.push(((*state, *dest_lang), new_res_rules_set));
                        }
                    }
                }
            }
        }
    }

    pub fn derivative(&mut self, curr_lang: &Language, language_list: &LanguageList, symbol: Symbol, finite_state_automaton: &FiniteStateAutomaton) -> (Edges, CompletedParses, bool) {
        let mut edges: Edges = HashMap::new();
        let mut completed_parses: CompletedParses = HashSet::new();
        let mut fin: bool = false;

        for ((start_state, dest_lang), applied_rules_set) in &curr_lang.edges {
            if let Some(destinations) = finite_state_automaton.simulate(start_state, symbol) {
                for (end_state, new_rules, end_state_accepting) in destinations {
                    let dest_language: &Language = language_list.get(*dest_lang).unwrap();
                    let res_rules_set: HashSet<Rules> = ParseRound::prepend_rules_to_rules_set(new_rules, &applied_rules_set);
                    if !finite_state_automaton.has_transition(end_state) {
                        if end_state_accepting {
                            for ((state, lang), rules_set) in &dest_language.edges {
                                edges.entry((*state, *lang)).or_default().extend(ParseRound::concatenate_rules_sets(&res_rules_set, rules_set));
                            }
                        }
                        if dest_language.fin {
                            fin = true;
                            completed_parses.extend(res_rules_set);
                        }
                    } else if end_state_accepting {
                        completed_parses.extend(res_rules_set.clone());
                        edges.entry((*end_state, *dest_lang)).or_default().extend(res_rules_set.clone());
                        if dest_language.fin {
                            fin = true;
                        }
                        for ((state, lang), rules_set) in &dest_language.edges {
                            edges.entry((*state, *lang)).or_default().extend(ParseRound::concatenate_rules_sets(&res_rules_set, rules_set));
                        }
                    } else {
                        edges.entry((*end_state, *dest_lang)).or_default().extend(res_rules_set);
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
                lang.completed_parses.extend(completed_parses);
                if fin {
                    lang.fin = true;
                }
            } else {
                let res_lang = Language::new(curr_lang.id + 1, edges, completed_parses, fin);
                self.prep_deriv_language = Some(res_lang);
            }
            true
        } else {
            false
        }
    }

    pub fn prepend(&mut self, atomic: (&State, &HashSet<Rules>, bool), finite_state_automaton: &FiniteStateAutomaton) {
        if let Some(prep_deriv_language) = &self.prep_deriv_language {
            let mut edges: Edges = HashMap::new();
            let mut fin: bool = false;
            let mut completed_parses: CompletedParses = HashSet::new();

            if finite_state_automaton.has_transition(atomic.0) {
                if !atomic.1.is_empty() {
                    if prep_deriv_language.edges.is_empty() && prep_deriv_language.fin {
                        for rules in atomic.1 {
                            edges.entry((*atomic.0, 0)).or_default().insert(rules.clone());
                        }
                    } else {
                        for rules in atomic.1 {
                            edges.entry((*atomic.0, prep_deriv_language.id)).or_default().insert(rules.clone());
                            self.keep_prep_deriv = true;
                        }
                    }
                } else {
                    if prep_deriv_language.edges.is_empty() && prep_deriv_language.fin {
                        edges.entry((*atomic.0, 0)).or_default();
                    } else {
                        edges.entry((*atomic.0, prep_deriv_language.id)).or_default();
                        self.keep_prep_deriv = true;
                    }
                }
            }

            if atomic.2 {
                for rules in atomic.1 {
                    for ((state, lang), applied_rules_set) in prep_deriv_language.edges.clone() {
                        edges.entry((state, lang)).or_default().extend(ParseRound::prepend_rules_to_rules_set(rules, &applied_rules_set));
                    }
                    for parse in &prep_deriv_language.completed_parses {
                        completed_parses.insert([rules.clone(), parse.clone()].concat());
                    }
                }
                if prep_deriv_language.fin {
                    fin = true;
                    completed_parses.extend(prep_deriv_language.completed_parses.clone());
                    edges.extend(prep_deriv_language.edges.clone());
                }
            }

            if self.keep_prep_deriv {
                self.prep_language = Some(Language::new(prep_deriv_language.id + 1, edges, completed_parses, fin));
            } else {
                self.prep_language = Some(Language::new(prep_deriv_language.id, edges, completed_parses, fin));
            }
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
    language_list.insert_new_language(Language::new(language_list.highest_id+1, HashMap::from([((start_state, 0), HashSet::new())]), HashSet::new(), start_accepting));

    for token in token_string {
        println!("Next token: {}", token);
        let mut curr: ParseRound = ParseRound::new();

        curr.derive(&language_list, token, finite_state_automaton);

        for nonterminal in &grammar.nonterminals {
            if let Some(atomic) = finite_state_automaton.get_atomic(Symbol::Nonterminal(*nonterminal), token) {
                println!("Found atomic: [{}]^({})", nonterminal, token);
                if curr.prep_derive(&language_list, *nonterminal, finite_state_automaton) {
                    curr.prepend(atomic, finite_state_automaton);
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

    println!("{}", language_list);
    epsilon(language_list)
}
