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

type Configuration = (VecDeque<(State, bool)>, Rules);
type Language = HashSet<Configuration>;

pub fn print_language(language: &Language) {
    println!("lang:");
    for (configuration, applied_rules) in language {
        print!("    [");
        for (state, accepting) in configuration {
            print!("({}, {})  ", state, accepting);
        }
        for (nonterminal, rule) in applied_rules {
            print!("({} -> ", nonterminal);
            for symbol in rule {
                print!("{}", symbol);
            }
            print!(")");
        }
        println!("]");
    }
}

pub fn prepend(finite_state_automaton: &FiniteStateAutomaton, symbol: Symbol, token: Terminal, language: &Language) -> Language {
    let mut res: Language = HashSet::new();
    if let Some((atomic, opt_rule_set, accepting)) = finite_state_automaton.get_atomic(symbol, token) {
        match (finite_state_automaton.has_transition(atomic), opt_rule_set) {
            (true, None) => {
                for (curr_conf, applied_rules) in language {
                    let mut new_conf = curr_conf.clone();
                    new_conf.push_front((*atomic, accepting));
                    res.insert((new_conf, applied_rules.clone()));
                    //res.extend(derivative(&(new_conf, applied_rules), Symbol::Epsilon, finite_state_automaton));
                    res.extend(one_state_derivative(curr_conf, applied_rules, *atomic, Symbol::Epsilon, finite_state_automaton));
                }
            },
            (false, Some(rule_set)) => {
                for (curr_conf, applied_rules) in language {
                    for rules in rule_set {
                        res.insert((curr_conf.clone(), [rules.clone(), applied_rules.clone()].concat()));
                    }
                }
            },
            (true, Some(rule_set)) => {
                for (curr_conf, applied_rules) in language {
                    for rules in rule_set {
                        let mut new_conf = curr_conf.clone();
                        new_conf.push_front((*atomic, accepting));
                        res.insert((new_conf, [rules.clone(), applied_rules.clone()].concat()));
                    }
                }
            },
            _ => res = language.clone(),
        }
    }
    res
}

pub fn one_state_derivative(curr_configuration: &VecDeque<(State, bool)>, rules: &Rules, curr_state: State, symbol: Symbol, finite_state_automaton: &FiniteStateAutomaton) -> Language {
    let mut res: Language = HashSet::new();

    if let Some(destinations) = finite_state_automaton.simulate(&curr_state, symbol) {
        for (dest, opt_rules, accepting) in destinations {
            let mut new_configuration = curr_configuration.clone();
            if finite_state_automaton.has_transition(dest) {
                new_configuration.push_front((*dest, accepting));
            }
            if let Some(new_rules) = opt_rules {
                let mut applied_rules = new_rules.clone();
                applied_rules.extend(rules.clone());
                //res.extend(one_state_derivative(curr_configuration, &applied_rules, *dest, Symbol::Epsilon, finite_state_automaton));
                res.insert((new_configuration.into(), applied_rules));
            } else {
                //res.extend(one_state_derivative(curr_configuration, rules, *dest, Symbol::Epsilon, finite_state_automaton));
                res.insert((new_configuration.into(), rules.clone()));
            }
        }
    }

    res
}

pub fn derive_e(language: &Language, finite_state_automaton: &FiniteStateAutomaton) -> Language {
    let mut res: Language = language.clone();
    let mut new_res = derivative(language, Symbol::Epsilon, finite_state_automaton);
    while !new_res.is_empty() {
        res.extend(new_res.clone());
        new_res = derivative(&new_res, Symbol::Epsilon, finite_state_automaton);
    }
    res
}

pub fn derivative(language: &Language, symbol: Symbol, finite_state_automaton: &FiniteStateAutomaton) -> Language {
    let mut res: Language = HashSet::new();
    for (curr_conf, rules) in language {
        let mut curr_configuration: VecDeque<(State, bool)> = curr_conf.clone().into();

        while let Some((state, accepting)) = curr_configuration.pop_front() {
            res.extend(one_state_derivative(&curr_configuration, rules, state, symbol, finite_state_automaton));
            if !accepting {
                break;
            }
        }
    }

    res
}

pub fn epsilon(language: &Language, finite_state_automaton: &FiniteStateAutomaton) -> bool {
    'lang: for (configuration, _) in language {
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
    let mut lang: Language = HashSet::from([(VecDeque::from([finite_state_automaton.get_start()]), Vec::new())]);
    for token in token_string {
        print_language(&lang);
        //println!("lang: {:?}", lang);
        let mut new_lang: Language = HashSet::new();
        for symbol in symbols {
            new_lang.extend(prepend(finite_state_automaton, *symbol, token, &derivative(&derive_e(&lang, finite_state_automaton), *symbol, finite_state_automaton)))
            //if let Some((atomic, opt_rule_set, accepting)) = finite_state_automaton.get_atomic(*symbol, token) {
            //    if finite_state_automaton.has_transition(atomic) {
            //        new_lang.extend(prepend(Some((*atomic, accepting)), opt_rule_set, &derivative(&lang, *symbol, &finite_state_automaton)));
            //    } else {
            //        new_lang.extend(prepend(None, opt_rule_set, &derivative(&lang, *symbol, &finite_state_automaton)));
            //    }
            //}
        }
        lang = new_lang;
    }
    lang = derive_e(&lang, finite_state_automaton);
    print_language(&lang);
    //println!("lang: {:?}", lang);
    epsilon(&lang, finite_state_automaton)
}

