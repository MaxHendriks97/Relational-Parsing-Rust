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

type Depth = usize;
type Edges = HashMap<(State, Depth), HashSet<Rules>>;
type CompletedParses = HashSet<Rules>;

#[derive(Debug)]
pub struct Language {
    edges: Edges,
    completed_parses: CompletedParses,
    fin: bool,
}

impl fmt::Display for Language {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "final: {}\n", self.fin)?;
        if self.fin && !self.completed_parses.is_empty() {
            write!(f, "completed parses:\n")?;
            for rules in &self.completed_parses {
                write!(f, "        ")?;
                print_rules(&rules, f)?;
                write!(f, "\n")?;
            }
        }
        for ((state, depth), rules_set) in &self.edges {
            write!(f, "  ({}, {})", state, depth)?;
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
    pub fn new(edges: Edges, completed_parses: CompletedParses, fin: bool) -> Language {
        Language {edges, completed_parses, fin}
    }

    fn find_lowest_depth(&self) -> Depth {
        let mut res: Depth = usize::max_value();
        for ((_, depth), _) in &self.edges {
            if *depth < res {
                res = *depth;
            }
        }
        res
    }

    fn adjust_lowest_depth(&mut self, lowest_depth: Depth) {
        let old_edges: Edges = std::mem::take(&mut self.edges);
        for ((source_state, dest_depth), rules_set) in old_edges {
            self.edges.insert((source_state, dest_depth - lowest_depth + 1), rules_set);
        }
    }
}

#[derive(Debug)]
pub struct LanguageList {
    languages: Vec<Language>,
}

impl fmt::Display for LanguageList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (index, language) in self.languages.iter().enumerate() {
            write!(f, "id: {} {}\n", index, language)?;
        }
        Ok(())
    }
}

impl LanguageList {
    pub fn new() -> LanguageList {
        let mut languages: Vec<Language> = Vec::new();
        languages.push(Language{edges: HashMap::new(), completed_parses: HashSet::new(), fin: true});
        LanguageList {languages}
    }

    pub fn get(&self, depth: &Depth) -> Option<&Language> {
        self.languages.get(self.languages.len() - depth)
    }

    pub fn insert_new_language(&mut self, lang: Language) {
        self.languages.push(lang);
    }

    pub fn pop_curr_lang(&mut self) -> Option<Language> {
        self.languages.pop()
    }

}

pub struct ParseRound {
    deriv: Option<Language>,
    prep_deriv: Option<Language>,
    prep: Option<Language>,
}

impl ParseRound {
    pub fn new() -> ParseRound {
        ParseRound{deriv: None, prep_deriv: None, prep: None}
    }

    pub fn register(self, language_list: &mut LanguageList, finite_state_automaton: &FiniteStateAutomaton) -> Result<(), &'static str> {
        if let Some(deriv) = &self.deriv {
            println!("Got deriv: {}", deriv);
        } else {
            println!("No deriv made");
        }

        if let Some(prep_deriv) = &self.prep_deriv {
            println!("Got prep_deriv: {}", prep_deriv);
        } else {
            println!("No prep_deriv_made");
        }
        
        if let Some(prep) = &self.prep {
            println!("Got prep: {}", prep);
        } else {
            println!("No prep made");
        }

        if let (Some(prep_deriv), Some(mut prep)) = (self.prep_deriv, self.prep) {
            if let Some(deriv) = self.deriv {
                for ((source_state, dest_depth), rules) in deriv.edges {
                    prep.edges.entry((source_state, dest_depth + 1)).or_default().extend(rules);
                }
                prep.completed_parses.extend(deriv.completed_parses);
                if deriv.fin {
                    prep.fin = true;
                }
            }

            if !prep_deriv.edges.is_empty() {
                language_list.insert_new_language(prep_deriv);
            }

            let lowest_depth = prep.find_lowest_depth();
            if lowest_depth > 1 {
                prep.adjust_lowest_depth(lowest_depth);
                for _ in 1..lowest_depth {
                    language_list.languages.pop();
                }
            }
            language_list.insert_new_language(prep);
        } else {
            if let Some(mut lang) = self.deriv {
                if !lang.edges.is_empty() {
                    let lowest_depth = lang.find_lowest_depth();
                    if lowest_depth > 1 {
                        lang.adjust_lowest_depth(lowest_depth);
                        for _ in 1..lowest_depth {
                            language_list.languages.pop();
                        }
                    }
                }

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

    pub fn e_sim(lang: &mut Language, language_list: &LanguageList, finite_state_automaton: &FiniteStateAutomaton) {
        let mut to_simulate: Vec<((State, Depth), HashSet<Rules>)> = lang.edges.clone().into_iter().collect();

        while let Some(((source_state, dest_depth), applied_rules_set)) = to_simulate.pop() {
            if let Some(destinations) = finite_state_automaton.simulate(&source_state, Symbol::Epsilon) {
                for (end_state, new_rules, end_state_accepting) in destinations {
                    let dest_language: &Language = language_list.get(&dest_depth).unwrap();
                    let res_rules_set: HashSet<Rules> = ParseRound::prepend_rules_to_rules_set(new_rules, &applied_rules_set);
                    if end_state_accepting {
                        if dest_language.fin {
                            lang.fin = true;
                            lang.completed_parses.extend(res_rules_set.clone());
                        } else {
                            for ((state, depth), rules_set) in &dest_language.edges {
                                let new_res_rules_set: HashSet<Rules> = ParseRound::concatenate_rules_sets(&res_rules_set, rules_set);
                                lang.edges.entry((*state, dest_depth + *depth)).or_default().extend(new_res_rules_set.clone());
                                to_simulate.push(((*state, dest_depth + *depth), new_res_rules_set));
                            }
                        }
                    }
                    if finite_state_automaton.has_transition(end_state) {
                        lang.edges.entry((*end_state, dest_depth)).or_default().extend(res_rules_set.clone());
                        to_simulate.push(((*end_state, dest_depth), res_rules_set));
                    }
                }
            }
        }
    }

    pub fn derivative(&mut self, curr_lang: &Language, language_list: &LanguageList, symbol: Symbol, finite_state_automaton: &FiniteStateAutomaton) -> (Edges, CompletedParses, bool) {
        let mut edges: Edges = HashMap::new();
        let mut completed_parses: CompletedParses = HashSet::new();
        let mut fin: bool = false;

        for ((start_state, end_depth), applied_rules_set) in &curr_lang.edges {
            if let Some(destinations) = finite_state_automaton.simulate(start_state, symbol) {
                for (end_state, new_rules, end_state_accepting) in destinations {
                    let res_rules_set: HashSet<Rules> = ParseRound::prepend_rules_to_rules_set(new_rules, &applied_rules_set);
                    let dest_language: &Language = language_list.get(end_depth).unwrap();

                    if end_state_accepting {
                        if dest_language.fin {
                            completed_parses.extend(res_rules_set.clone());
                            fin = true;
                        } else {
                            for ((state, depth), rules_set) in &dest_language.edges {
                                edges.entry((*state, *depth + *end_depth)).or_default().extend(ParseRound::concatenate_rules_sets(&res_rules_set, rules_set));
                            }
                        }
                    }
                    if finite_state_automaton.has_transition(end_state) {
                        edges.entry((*end_state, *end_depth)).or_default().extend(res_rules_set);
                    }

                }
            }
        }

        (edges, completed_parses, fin)
        
    }

    pub fn derive(&mut self, curr_lang: &Language, language_list: &LanguageList, terminal: Terminal, finite_state_automaton: &FiniteStateAutomaton) {
        let (edges, completed_parses, fin) = self.derivative(curr_lang, language_list, Symbol::Terminal(terminal), finite_state_automaton);
        if !edges.is_empty() || fin{
            self.deriv = Some(Language::new(edges, completed_parses, fin));
        }
    }

    pub fn prep_derive(&mut self, curr_lang: &Language, language_list: &LanguageList, nonterminal: Nonterminal, finite_state_automaton: &FiniteStateAutomaton) -> bool {
        let (edges, completed_parses, fin) = self.derivative(curr_lang, language_list, Symbol::Nonterminal(nonterminal), finite_state_automaton);

        if !edges.is_empty() || fin {
            if let Some(mut lang) = self.prep_deriv.as_mut() {
                lang.edges.extend(edges);
                lang.completed_parses.extend(completed_parses);
                if fin {
                    lang.fin = true;
                }
            } else {
                let res_lang = Language::new(edges, completed_parses, fin);
                self.prep_deriv = Some(res_lang);
            }
            true
        } else {
            false
        }
    }

    pub fn prepend(&mut self, atomic: (&State, &HashSet<Rules>, bool), language_list: &LanguageList, finite_state_automaton: &FiniteStateAutomaton) {
        if let Some(prep_deriv) = &self.prep_deriv {
            let mut edges: Edges = HashMap::new();
            let mut fin: bool = false;
            let mut completed_parses: CompletedParses = HashSet::new();

            if finite_state_automaton.has_transition(atomic.0) {
                match (atomic.1.is_empty(), prep_deriv.edges.is_empty() && prep_deriv.fin) {
                    (true, true) => {edges.entry((*atomic.0, language_list.languages.len())).or_default();},
                    (true, false) => {edges.entry((*atomic.0, 1)).or_default();},
                    (false, true) => {
                        for rules in atomic.1 {
                            edges.entry((*atomic.0, language_list.languages.len())).or_default().insert(rules.clone());
                        }
                    },
                    (false, false) => {
                        for rules in atomic.1 {
                            edges.entry((*atomic.0, 1)).or_default().insert(rules.clone());
                        }
                    },
                }
            }

            if atomic.2 {
                for rules in atomic.1 {
                    for ((state, depth), applied_rules_set) in prep_deriv.edges.clone() {
                        edges.entry((state, depth + 1)).or_default().extend(ParseRound::prepend_rules_to_rules_set(rules, &applied_rules_set));
                    }
                    for parse in &prep_deriv.completed_parses {
                        completed_parses.insert([rules.clone(), parse.clone()].concat());
                    }
                }
                if prep_deriv.fin {
                    fin = true;
                    completed_parses.extend(prep_deriv.completed_parses.clone());
                    completed_parses.extend(atomic.1.clone());
                    edges.extend(prep_deriv.edges.clone());
                }
            }

            self.prep = Some(Language::new(edges, completed_parses, fin));
        }
    }

}

fn epsilon(lang: &Language) -> bool {
    lang.fin
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
    language_list.insert_new_language(Language::new(HashMap::from([((start_state, 1), HashSet::new())]), HashSet::new(), start_accepting));

    for token in token_string {
        if let Some(mut curr_lang) = language_list.pop_curr_lang() {
            println!("Next token: {}", token);
            println!("Curr lang: {}", curr_lang);
            println!("Curr lang_list: {}", language_list);
            let mut curr: ParseRound = ParseRound::new();

            ParseRound::e_sim(&mut curr_lang, &language_list, finite_state_automaton);

            curr.derive(&curr_lang, &language_list, token, finite_state_automaton);

            for nonterminal in &grammar.nonterminals {
                if let Some(atomic) = finite_state_automaton.get_atomic(Symbol::Nonterminal(*nonterminal), token) {
                    println!("Found atomic: [{}]^({})", nonterminal, token);
                    if curr.prep_derive(&curr_lang, &language_list, *nonterminal, finite_state_automaton) {
                        curr.prepend(atomic, &language_list, finite_state_automaton);
                    }
                }
            }


            match curr.register(&mut language_list, finite_state_automaton) {
                Ok(_) => {},
                Err(e) => {
                    println!("{}", e);
                    return false;
                }
            }
            println!("End lang_list: {}", language_list);

        } else {
            println!("Derivation failed");
            return false;
        }
        
    }

    if let Some(mut last_lang) = language_list.pop_curr_lang() {
        ParseRound::e_sim(&mut last_lang, &language_list, finite_state_automaton);
        println!("{}", language_list);
        println!("Last: {}", last_lang);
        epsilon(&last_lang)
    } else {
        println!("{}", language_list);
        println!("Derivation failed");
        return false;
    }
}
