use std::collections::{HashSet, BTreeSet, HashMap, VecDeque};
use std::mem;

mod word;
pub use word::*;
mod grammar;
pub use grammar::*;
mod regex;
pub use regex::*;
mod finite_state_automaton;
pub use finite_state_automaton::*;

//type LangIdent = u32;
//
//#[derive(Debug)]
//struct Language {
//    id: LangIdent,
//    edges: BTreeSet<(State, LangIdent)>,
//    fin: bool,
//}
//
//#[derive(Debug)]
//struct ParseResult {
//    languages: HashMap<LangIdent, Language>,
//    highest_id: LangIdent,
//    curr: Language,
//    memoized: HashMap<(Language, Terminal), Language>,
//}
//
//impl ParseResult {
//    pub fn new(finite_state_automaton: &FiniteStateAutomaton) -> ParseResult {
//        let mut languages: HashMap<LangIdent, Language> = HashMap::new();
//        languages.insert(0, Language{id: 0, edges: BTreeSet::new(), fin: true});
//        ParseResult{languages, highest_id: 1,
//            curr: Language{id: 1, edges: BTreeSet::from([(0, 0)]), fin: finite_state_automaton.is_accepting(&0)},
//            memoized: HashMap::new(),
//        }
//    }
//
//    fn make_new_lang_curr(&mut self, new_lang: Language) {
//        self.languages.insert(self.curr.id, mem::replace(&mut self.curr, new_lang));
//    }
//
//    fn build_and_add_new_lang(&mut self, edges: BTreeSet<(State, LangIdent)>, fin: bool) {
//        self.highest_id += 1;
//        self.make_new_lang_curr(
//            Language{
//                id: self.highest_id, edges, fin
//            }
//        );
//    }
//
//    pub fn prepend(&mut self, atomic_language: (State, bool)) {
//        if atomic_language.1 {
//            let mut edges: BTreeSet<(State, LangIdent)> = BTreeSet::new();
//            for (_, target) in &self.curr.edges {
//                edges.insert((atomic_language.0, *target));
//            }
//            self.build_and_add_new_lang(edges, self.curr.fin);
//        } else {
//            self.build_and_add_new_lang(BTreeSet::from([(atomic_language.0, self.curr.id)]), false);
//        }
//    }
//
//    pub fn derivative(&mut self, symbol: Symbol, finite_state_automaton: &FiniteStateAutomaton) {
//        let mut new_edges: BTreeSet<(State, LangIdent)> = BTreeSet::new();
//        let mut fin: bool = false;
//        for (state, lang) in &self.curr.edges {
//            if let Some((new_state, accepting)) = finite_state_automaton.simulate(state, symbol) {
//                if finite_state_automaton.has_transition(new_state) {
//                    if accepting {
//                        self.languages.get(lang).and_then(|target| {
//                            if target.fin {
//                                fin = true;
//                            }
//                            for (_, dest_lang) in &target.edges {
//                                new_edges.insert((*new_state, *dest_lang));
//                            }
//                            Some(())
//                        });
//                    } else {
//                        new_edges.insert((*new_state, *lang));
//                    }
//                }
//            }
//        }
//        if !new_edges.is_empty() {
//            self.build_and_add_new_lang(new_edges, fin);
//        }
//    }
//
//    pub fn epsilon(&self) -> bool {
//        self.curr.fin
//    }
//
//}
//
//pub fn g_accepts_string(token_string: Vec<Terminal>, grammar: &Grammar) -> bool {
//    let finite_state_automaton: &FiniteStateAutomaton = &grammar.finite_state_automaton;
//    let symbols: &HashSet<Symbol> = &grammar.symbols;
//    let mut parse_result: ParseResult = ParseResult::new(&finite_state_automaton);
//    for token in token_string {
//        println!("lang: {:?}, token: {}", parse_result.curr, token);
//        for symbol in symbols {
//            if let Some((atomic, accepting)) = finite_state_automaton.get_atomic(*symbol, token) {
//                parse_result.derivative(*symbol, finite_state_automaton);
//                if finite_state_automaton.has_transition(atomic) {
//                    parse_result.prepend((*atomic, accepting));
//                }
//            }
//        }
//    }
//    parse_result.epsilon()
//}

type Configuration = Vec<(State, bool)>;
type Language = HashSet<Configuration>;

pub fn prepend(atomic_language: (State, bool), language: &Language) -> Language {
    let mut res: Language = HashSet::new();
    for configuration in language {
        res.insert([[atomic_language].to_vec(), configuration.clone()].concat());
    }
    res
}

pub fn head(configuration: &Configuration) -> &[(State, bool)] {
    let mut end: usize = 0;
    while let Some((_, accepting)) = configuration.get(end) {
        end += 1;
        if !*accepting {
            break;
        }
    }
    &configuration[0..end]
}

pub fn derivative(language: &Language, symbol: Symbol, finite_state_automaton: &FiniteStateAutomaton) -> Language {
    let mut res: Language = HashSet::new();
    for configuration in language {
        let mut curr_configuration: VecDeque<(State, bool)> = configuration.clone().into();

        while let Some((state, accepting)) = curr_configuration.pop_front() {
            let mut new_configuration = curr_configuration.clone();
            if let Some((dest, accepting)) = finite_state_automaton.simulate(&state, symbol) {
                if finite_state_automaton.has_transition(dest) {
                    new_configuration.push_front((*dest, accepting));
                }
                res.insert(new_configuration.into());
            }
            if !accepting {
                break;
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

