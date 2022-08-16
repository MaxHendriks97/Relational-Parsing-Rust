use std::collections::HashSet;
use std::collections::HashMap;
use std::collections::hash_map::Entry::{Occupied, Vacant};
use std::fmt;

#[derive(Eq, PartialEq, Hash, Debug, Clone, Copy, PartialOrd, Ord)]
enum Symbol {
    Terminal(Terminal),
    Nonterminal(Nonterminal),
    AtomicLanguage(char, Terminal, Operator),
    Empty,
}

#[derive(Eq, PartialEq, Hash, Debug, Clone, Copy, PartialOrd, Ord)]
enum Operator {
    ONE,
    STAR,
    PLUS,
    OPT,
}

type Terminal = char;
type Nonterminal = char;
type AtomicLanguage<'a> = (char, Terminal, Operator, &'a Language);
type Word = Vec<Symbol>;
type Language = HashSet<Word>;

#[derive(Debug)]
struct Grammar{
    terminals: HashSet<Terminal>,
    nonterminals: HashSet<Nonterminal>,
    start: Nonterminal,
    rules: HashMap<Nonterminal, Vec<Word>>,
    symbols: HashSet<Symbol>,
    atomics: HashMap<(Symbol, Terminal), (Operator, Language)>,
}

impl Grammar {
    pub fn new(terminals: HashSet<Terminal>, nonterminals: HashSet<Nonterminal>, start: Nonterminal, rules: HashMap<Nonterminal, Vec<Word>>) -> Grammar {
        let mut symbols: HashSet<Symbol> = HashSet::new();
        for s in &nonterminals {
            symbols.insert(Symbol::Nonterminal(*s));
        }
        for t in &terminals {
            symbols.insert(Symbol::Terminal(*t));
        }
        let atomics = Grammar::to_atomics(start, &rules, &symbols, &terminals);
        return Grammar{terminals, nonterminals, start, rules, symbols, atomics};
    }

    fn to_atomics(start: Nonterminal, rules: &HashMap<Nonterminal, Vec<Word>>, symbols: &HashSet<Symbol>, terminals: &HashSet<Terminal>) -> HashMap<(Symbol, Terminal), (Operator, Language)> {
        let mut new_rules: HashMap<char, Vec<Vec<Symbol>>> = HashMap::new();
        let mut res: HashMap<(Symbol, Terminal), (Operator, Language)> = HashMap::new();
        for (nt, words) in rules {
            for word in words {
                // Now check if word contains nullable nonterminal, if so, add a nulled rule
                let mut nullable_positions: Vec<usize> = Vec::new();
                for (pos, symbol) in word.iter().enumerate() {
                    match symbol {
                        Symbol::Nonterminal(nt) => {
                            if Grammar::nullable_nonterminal(rules, &nt) {
                                nullable_positions.push(pos);
                            }
                        },
                        _ => {},
                    }
                }

                let nullable_combinations: Vec<Vec<usize>> = Grammar::powerset(&nullable_positions);
                for comb in nullable_combinations {
                    let mut rev = comb.clone();
                    rev.sort();
                    rev.reverse();
                    let mut new_word = word.clone();
                    for index in rev {
                        new_word.remove(index);
                    }
                    if new_word.len() > 0 {
                        new_rules.entry(*nt).or_insert_with(Vec::new).push(new_word);
                    }
                }
            }
        }
        res.insert((Symbol::Nonterminal(' '), ' '), (Operator::ONE, HashSet::from([vec![Symbol::Nonterminal(start)]])));
        let mut changed: bool = true;
        while changed {
            changed = false;
            for symbol in symbols {
                for terminal in terminals {
                    if let Some(lang) = Grammar::derive(&new_rules, *symbol, *terminal, &res) {
                        if res.insert((*symbol, *terminal), lang) == None {
                            changed = true;
                        }
                    }
                }
            }
        }

        return res
    }

    fn derive(rules: &HashMap<Nonterminal, Vec<Word>>, symbol: Symbol, terminal: Terminal, curr_atomics: &HashMap<(Symbol, Terminal), (Operator, Language)>) -> Option<(Operator, Language)> {
        match symbol {
            Symbol::Nonterminal(s) => {
                let mut res: (Operator, Language) = (Operator::ONE, HashSet::new());
                let mut rule_started_with_own_nonterminal: bool = false;
                let mut rule_started_with_terminal: bool = false;
                for word in rules.get(&s)? {
                    //let mut to_insert: Language = HashSet::new();
                    match word[0] {
                        Symbol::Terminal(s) => {
                            if s == terminal {
                                rule_started_with_terminal = true;
                                if word.len() == 1 {
                                    match res.0 {
                                        Operator::ONE => {res.0 = Operator::OPT},
                                        Operator::PLUS => {res.0 = Operator::STAR},
                                        _ => {},
                                    }
                                }
                                if word.len() > 1 {
                                    //to_insert.insert(word[1..].to_vec());
                                    res.1.insert(word[1..].to_vec());
                                }
                            }
                        },
                        Symbol::Nonterminal(_) => {
                            //First symbol is nonterminal, check if any rule with that nonterminal contains the specified terminal
                            if word[0] == symbol {
                                match res.0 {
                                    Operator::ONE => {res.0 = Operator::PLUS},
                                    Operator::OPT => {res.0 = Operator::STAR},
                                    _ => {},
                                }
                                rule_started_with_own_nonterminal = true;
                                for word2 in Grammar::find_rule_with_terminal(&rules, s, terminal, &mut vec![]) {
                                    if word[0] == symbol {
                                        res.1.insert(word[1..].to_vec());
                                    } else {
                                        res.1.insert([&word2[1..], &word[1..]].concat());
                                    }
                                    rule_started_with_terminal = true;
                                }
                            } else {
                                if let Some(atomic) = curr_atomics.get(&(word[0], terminal)) {
                                    if atomic.1.is_empty() {
                                        rule_started_with_terminal = true;
                                        if word.len() > 1 {
                                            res.1.insert(word[1..].to_vec());
                                        }
                                    } else {
                                        for word2 in &atomic.1 {
                                            //if word[0] == symbol && word2[0] == Symbol::Terminal(terminal) {
                                            //    //match res.0 {
                                            //    //    Operator::ONE => res.0 = Operator::PLUS,
                                            //    //    Operator::OPT => res.0 = Operator::STAR,
                                            //    //    _ => {},
                                            //    //}
                                            //    //rule_started_with_own_nonterminal = true;
                                            //    rule_started_with_terminal = true;
                                            //    res.1.insert(word[1..].to_vec());
                                            //} else

                                            res.1.insert([&word2[..], &word[1..]].concat());
                                            rule_started_with_terminal = true;
                                        }
                                    }
                                }
                            }
                            
                                //for word2 in rules.get(&s)? {
                                //    if word2[0] != word[0] {
                                //        if let Symbol::Nonterminal(ns) = word2[0] {
                                //            //Grammar::find_rule_with_terminal(&rules, ns, )
                                //        }
                                //    }
                                //}
                                //res.1.insert(word[1..].to_vec());
                            
                        }
                        _ => {},
                    }
                    //res.1.extend(to_insert.clone());
                    //for word in to_insert {
                    
                }
                if !rule_started_with_terminal {
                    None
                } else {
                    if rule_started_with_own_nonterminal {
                        let old_res = res;
                        let mut res = (old_res.0, HashSet::new());
                        for mut word in old_res.1 {
                            word.push(Symbol::AtomicLanguage(s, terminal, Operator::STAR));
                            res.1.insert(word);
                        }
                        Some(res)
                    } else {
                        Some(res)
                    }
                }
            },
            Symbol::Terminal(s) => {
                if s == terminal {
                    Some((Operator::ONE, HashSet::new()))
                } else {
                    None
                }
            },
            _ => None,
        }
    }

    fn find_rule_with_terminal(rules: &HashMap<Nonterminal, Vec<Word>>, nonterminal: Nonterminal, terminal: Terminal, checked: &mut Vec<Nonterminal>) -> Language {
        let mut res: Language = HashSet::new();

        let mut found: bool = false;

        if let Some(rule_list) = rules.get(&nonterminal) {
            for word in rule_list {
                match word[0] {
                    Symbol::Terminal(s) => {
                        if s == terminal {
                            res.insert(word.clone());
                        }
                    },
                    Symbol::Nonterminal(s) => {
                        if s != nonterminal && !checked.contains(&s) {
                            // We've found another nonterminal, check if this nonterminal has any words starting with terminal
                            checked.push(s);
                            let partial_res: Language = Grammar::find_rule_with_terminal(rules, s, terminal, checked);
                            for word2 in partial_res {
                                found = true;
                                res.insert([&word2[..], &word[1..]].concat());
                            }
                        }
                    },
                    _ => {},
                }
            }
            if found {
                for word in rule_list {
                    if word[0] == Symbol::Nonterminal(nonterminal) {
                        res.insert(word.clone());
                    }
                }
            }
        }

        return res
    }

    fn powerset<T>(s: &[T]) -> Vec<Vec<T>> where T: Clone {
        (0..2usize.pow(s.len() as u32)).map(|i| {
            s.iter().enumerate().filter(|&(t, _)| (i >> t) % 2 == 1)
                .map(|(_, element)| element.clone())
                .collect()
        }).collect()
    }

    fn nullable_nonterminal(rules: &HashMap<Nonterminal, Vec<Word>>, nonterminal: &Nonterminal) -> bool {
        if let Some(rule) = rules.get(nonterminal) {
            for word in rule {
                if *word == vec![Symbol::Empty] {
                    return true;
                }
            }
        }
        false
    }

    pub fn get_atomic_with_symb_term(&self, symbol: Symbol, terminal: Terminal) -> (char, Terminal, Option<&(Operator, HashSet<Vec<Symbol>>)>) {
        match symbol {
            Symbol::Terminal(t) => (t, terminal, self.atomics.get(&(symbol, terminal))),
            Symbol::Nonterminal(nt) => (nt, terminal, self.atomics.get(&(symbol, terminal))),
            _ => (' ', terminal, None)
        }
    }

    pub fn get_atomic(&self, symbol: Symbol, terminal: Terminal) -> Option<&(Operator, HashSet<Word>)> {
        self.atomics.get(&(symbol, terminal))
    }

    pub fn get_start(&self) -> Symbol {
        Symbol::Nonterminal(self.start)
    }

    pub fn get_symbols(&self) -> &HashSet<Symbol> {
        &self.symbols
    }

}

fn prepend(al: &AtomicLanguage, l: &Language) -> Language {
    let mut res: Language = HashSet::new();

    if al.3.len() > 0 {
        for lang in l {
            match al.2 {
                Operator::ONE => {
                    res.insert([[Symbol::AtomicLanguage(al.0, al.1, Operator::ONE)].to_vec(), lang.clone()].concat());
                    //for atomic in al.3 {
                    //    if *atomic == vec![Symbol::Empty] {
                    //        res.insert(lang.clone());
                    //    } else {
                    //        res.insert([atomic.clone(), lang.clone()].concat());
                    //    }
                    //}
                },
                Operator::OPT => {
                    res.insert([[Symbol::AtomicLanguage(al.0, al.1, Operator::OPT)].to_vec(), lang.clone()].concat());
                    //for atomic in al.3 {
                    //    if *atomic != vec![Symbol::Empty] {
                    //        res.insert([atomic.clone(), lang.clone()].concat());
                    //    }
                    //    res.insert(lang.clone());
                    //}
                },
                Operator::PLUS => {
                    for atomic in al.3 {
                        if *atomic == vec![Symbol::Empty] {
                            res.insert(lang.clone());
                        } else {
                            res.insert([atomic.clone(), [Symbol::AtomicLanguage(al.0, al.1, Operator::STAR)].to_vec(), lang.clone()].concat());
                        }
                    }
                },
                Operator::STAR => {
                    for atomic in al.3 {
                        if *atomic == vec![Symbol::Empty] {
                            res.insert(lang.clone());
                        } else {
                            res.insert([[Symbol::AtomicLanguage(al.0, al.1, Operator::STAR)].to_vec(), lang.clone()].concat());
                        }
                    }
                },
            }
            //for atomic in al.3 {
            //    if *atomic == vec![Symbol::Empty] {
            //        res.insert(lang.clone());
            //    } else {
            //        res.insert([atomic.clone(), lang.clone()].concat());
            //    }
            //}
        }
    } else {
        res = l.clone();
    }
    res
}

fn word_derivative(w: &Word, s: Symbol) -> Option<Word> {
    if w.starts_with(&[s]) {
        Some(w.clone()[1..].to_vec())
    } else {
        None
    }
}

fn derivative(l: &Language, s: Symbol, g: &Grammar) -> Language {
    let mut res: Language = HashSet::new();
    for word in l {
        match word[0] {
            Symbol::AtomicLanguage(c, terminal, operator) => {
                let opt_al: Option<&(Operator, Language)>;
                if g.terminals.contains(&c) {
                    opt_al = g.get_atomic(Symbol::Terminal(c), terminal);
                } else {
                    opt_al = g.get_atomic(Symbol::Nonterminal(c), terminal);
                }
                if let Some((_, language)) = opt_al {
                    match operator {
                        Operator::ONE => {
                            for atomic_word in language {
                                if let Some(w) = word_derivative(&atomic_word, s) {
                                    res.insert([&w[..], &word[1..]].concat());
                                }
                            }
                        },
                        Operator::STAR => {
                            for atomic_word in language {
                                if let Some(w) = word_derivative(&atomic_word, s) {
                                    //res.insert([&w[..], &[Symbol::AtomicLanguage(c, terminal, Operator::STAR)], &word[1..]].concat());
                                    res.insert([&w[..], &word[1..]].concat());
                                }
                                if word.len() > 1 {
                                    if let Symbol::Terminal(_) = s {
                                        match word[1] {
                                            Symbol::AtomicLanguage(..) => {
                                                res.extend(derivative(&HashSet::from([word[1..].to_vec()]), s, g));
                                            },
                                            _ => {
                                                if let Some(w) = word_derivative(&word[1..].to_vec(), s) {
                                                    res.insert(w);
                                                }
                                            },
                                        }
                                    }
                                }
                                
                            }
                        },
                        Operator::PLUS => {
                            for atomic_word in language {
                                if let Some(w) = word_derivative(&atomic_word, s) {
                                    res.insert([&w[..], &word[1..]].concat());
                                }
                            }
                        },
                        Operator::OPT => {
                            for atomic_word in language {
                                if let Some(w) = word_derivative(&atomic_word, s) {
                                    res.insert([&w[..], &word[1..]].concat());
                                }
                                if word.len() > 1 {
                                    match word[1] {
                                        Symbol::AtomicLanguage(..) => {
                                            res.extend(derivative(&HashSet::from([word[1..].to_vec()]), s, g));
                                        },
                                        _ => {
                                            if let Some(w) = word_derivative(&word[1..].to_vec(), s) {
                                                res.insert(w);
                                            }
                                        },
                                    }
                                }
                            }
                        },
                    }
                }
            },
            Symbol::Empty => {},
            _ => {if let Some(w) = word_derivative(&word, s) {
                res.insert(w);
            }},
        }
    }

    res
}

fn epsilon(l: &Language) -> bool {
    for word in l {
        let mut i: usize = 0;
        while let Symbol::AtomicLanguage(..) = word[i] {
            i += 1;
        }
        if word[i] == Symbol::Empty {
            return true
        }
    }
    false
}

fn g_generates_string(g: &Grammar, terminals: String) -> bool {
    let mut lang: Language = prepend(&(' ', ' ', Operator::ONE, &HashSet::from([vec![g.get_start()]])), &HashSet::from([vec![Symbol::Empty]])); // characters here are iffy, think of a nicer solution
    println!("\nStart lang: {:?}", lang);
    for terminal in terminals.chars() {
        let mut new_lang: Language = HashSet::new();
        println!("terminal: {}", terminal);
        for symbol in g.get_symbols() { //Check if I can reduce this to only terminals
            if let Some((operator, language)) = g.get_atomic(*symbol, terminal) {
                if let Symbol::Nonterminal(c) | Symbol::Terminal(c) = symbol {
                    new_lang.extend(prepend(&(*c, terminal, *operator, language), &derivative(&lang, *symbol, g)));
                }
            }
        }
        lang = new_lang;
        println!("End round lang");
        print_lang(&lang);
    }
    epsilon(&lang)
}

fn print_lang(lang: &Language) {
    println!("{{");
    for word in lang {
        println!("{:?}", word);
    }
    println!("}}\n");
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_prepend() {
        let al: Language = HashSet::from([vec![Symbol::Terminal('a')], vec![Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')]]);
        let l: Language = HashSet::from([vec![Symbol::Terminal('c'), Symbol::Terminal('a')], vec![Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')]]);

        assert_eq!(prepend(&(' ', ' ', Operator::ONE, &al), &l), HashSet::from([vec![Symbol::Terminal('a'), Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')], vec![Symbol::Terminal('a'), Symbol::Terminal('c'), Symbol::Terminal('a')], vec![Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c'), Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')], vec![Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c'), Symbol::Terminal('c'), Symbol::Terminal('a')]]));
    }

    #[test]
    fn test_derivative() {
        let lang: Language = HashSet::from([vec![Symbol::Terminal('a')], vec![Symbol::Terminal('b')], vec![Symbol::Terminal('a'), Symbol::Terminal('b')], vec![Symbol::Terminal('a'), Symbol::Nonterminal('S')]]);
        let g: Grammar = setup_empty_grammar();
        assert_eq!(derivative(&lang, Symbol::Terminal('a'), &g), HashSet::from([vec![], vec![Symbol::Terminal('b')], vec![Symbol::Nonterminal('S')]]));
    }

    fn basic_relational_parsing_example_grammar() -> Grammar {
        let terminals: HashSet<Terminal> = HashSet::from(['a', 'b', 'c']);
        let nonterminals: HashSet<Nonterminal> = HashSet::from(['S']);
        let start: Nonterminal = 'S';
        let mut rules: HashMap<Nonterminal, Vec<Word>> = HashMap::new();
        rules.insert('S', vec![
            vec![Symbol::Terminal('a')],
            vec![Symbol::Nonterminal('S'), Symbol::Terminal('a')],
            vec![Symbol::Nonterminal('S'), Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')]
        ]);
        Grammar::new(terminals, nonterminals, start, rules)
    }

    fn e_rule_relational_parsing_example_grammar() -> Grammar {
        let terminals: HashSet<Terminal> = HashSet::from(['a', 'b', 'c']);
        let nonterminals: HashSet<Nonterminal> = HashSet::from(['S']);
        let start: Nonterminal = 'S';
        let mut rules: HashMap<Nonterminal, Vec<Word>> = HashMap::new();
        rules.insert('S', vec![
            vec![Symbol::Terminal('a')],
            vec![Symbol::Nonterminal('S'), Symbol::Terminal('a')],
            vec![Symbol::Nonterminal('S'), Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')],
            vec![Symbol::Empty]
        ]);
        Grammar::new(terminals, nonterminals, start, rules)
    }

    fn three_rule_grammar() -> Grammar {
        let terminals: HashSet<Terminal> = HashSet::from(['a', 'b']);
        let nonterminals: HashSet<Nonterminal> = HashSet::from(['S']);
        let start: Nonterminal = 'S';
        let mut rules: HashMap<Nonterminal, Vec<Word>> = HashMap::new();
        rules.insert('S', vec![
            vec![Symbol::Terminal('a'), Symbol::Nonterminal('S'), Symbol::Terminal('b')],
            vec![Symbol::Nonterminal('S'), Symbol::Terminal('a'), Symbol::Terminal('b')],
            vec![Symbol::Terminal('a'), Symbol::Terminal('a'), Symbol::Terminal('a')],
        ]);
        Grammar::new(terminals, nonterminals, start, rules)
    }

    fn difficult_bottom_up_grammar() -> Grammar {
        let terminals: HashSet<Terminal> = HashSet::from(['a', '+', '-']);
        let nonterminals: HashSet<Nonterminal> = HashSet::from(['S', 'E', 'F', 'Q']);
        let start: Nonterminal = 'S';
        let mut rules:  HashMap<Nonterminal, Vec<Word>> = HashMap::new();
        rules.insert('S', vec![vec![Symbol::Nonterminal('E')]]);
        rules.insert('E', vec![
            vec![Symbol::Nonterminal('E'), Symbol::Nonterminal('Q'), Symbol::Nonterminal('F')],
            vec![Symbol::Nonterminal('F')]
        ]);
        rules.insert('F', vec![vec![Symbol::Terminal('a')]]);
        rules.insert('Q', vec![
            vec![Symbol::Terminal('+')],
            vec![Symbol::Terminal('-')],
        ]);
        Grammar::new(terminals, nonterminals, start, rules)
    }

    fn odd_number_of_a_grammar() -> Grammar {
        let terminals: HashSet<Terminal> = HashSet::from(['a']);
        let nonterminals: HashSet<Nonterminal> = HashSet::from(['S']);
        let start: Nonterminal = 'S';
        let mut rules: HashMap<Nonterminal, Vec<Word>> = HashMap::new();
        rules.insert('S', vec![
            vec![Symbol::Terminal('a'), Symbol::Nonterminal('S'), Symbol::Terminal('a')],
            vec![Symbol::Terminal('a')]
        ]);
        Grammar::new(terminals, nonterminals, start, rules)
    }

    fn direct_left_recursive_grammar() -> Grammar {
        let terminals: HashSet<Terminal> = HashSet::from(['a']);
        let nonterminals: HashSet<Nonterminal> = HashSet::from(['A']);
        let start: Nonterminal = 'A';
        let mut rules: HashMap<Nonterminal, Vec<Word>> = HashMap::new();
        rules.insert('A', vec![
            vec![Symbol::Nonterminal('A'), Symbol::Terminal('a')],
            vec![Symbol::Empty]
        ]);
        Grammar::new(terminals, nonterminals, start, rules)
    }
    
    fn indirect_left_recursive_grammar() -> Grammar {
        let terminals: HashSet<Terminal> = HashSet::from(['a']);
        let nonterminals: HashSet<Nonterminal> = HashSet::from(['A', 'B']);
        let start: Nonterminal = 'A';
        let mut rules: HashMap<Nonterminal, Vec<Word>> = HashMap::new();
        rules.insert('A', vec![
            vec![Symbol::Nonterminal('B'), Symbol::Terminal('a')],
            vec![Symbol::Terminal('a')],
        ]);
        rules.insert('B', vec![
            vec![Symbol::Nonterminal('A'), Symbol::Terminal('b')],
            vec![Symbol::Terminal('b')],
        ]);
        Grammar::new(terminals, nonterminals, start, rules) 
    }
    fn direct_right_recursive_grammar() -> Grammar {
        let terminals: HashSet<Terminal> = HashSet::from(['a']);
        let nonterminals: HashSet<Nonterminal> = HashSet::from(['A']);
        let start: Nonterminal = 'A';
        let mut rules: HashMap<Nonterminal, Vec<Word>> = HashMap::new();
        rules.insert('A', vec![
            vec![Symbol::Terminal('a'), Symbol::Nonterminal('A')],
            vec![Symbol::Terminal('a')],
        ]);
        Grammar::new(terminals, nonterminals, start, rules) 
    }

    fn indirect_right_recursive_grammar() -> Grammar {
        let terminals: HashSet<Terminal> = HashSet::from(['a']);
        let nonterminals: HashSet<Nonterminal> = HashSet::from(['A', 'B']);
        let start: Nonterminal = 'A';
        let mut rules: HashMap<Nonterminal, Vec<Word>> = HashMap::new();
        rules.insert('A', vec![
            vec![Symbol::Terminal('a'), Symbol::Nonterminal('B')],
            vec![Symbol::Terminal('a')],
        ]);
        rules.insert('B', vec![
            vec![Symbol::Terminal('b'), Symbol::Nonterminal('A')],
            vec![Symbol::Terminal('b')],
        ]);
        Grammar::new(terminals, nonterminals, start, rules) 
    }

    #[test]
    fn test_g_generates_string() {
        let grammar: Grammar = e_rule_relational_parsing_example_grammar();
        println!("{:?}", grammar.atomics);

        //assert!(g_generates_string(&grammar, "abcbcbcbaaaaaac".to_string()));
        //assert!(g_generates_string(&grammar, "aababacc".to_string()));
        assert!(g_generates_string(&grammar, "a".to_string()));
        //assert!(g_generates_string(&grammar, "aa".to_string()));
        //assert!(g_generates_string(&grammar, "aaa".to_string()));
        //assert!(g_generates_string(&grammar, "abac".to_string()));
        //assert!(g_generates_string(&grammar, "abaac".to_string()));
        //assert!(g_generates_string(&grammar, "ababacc".to_string()));
        //assert!(g_generates_string(&grammar, "abaabacca".to_string()));
        //assert!(g_generates_string(&grammar, "abaca".to_string()));
        //assert!(!g_generates_string(&grammar, "aababac".to_string()));
    }

    #[test]
    fn three_rule_grammar_test() {
        let mut grammar: Grammar = three_rule_grammar();
        grammar.atomics = HashMap::from([
            ((Symbol::Nonterminal(' '), ' '), (Operator::ONE, HashSet::from([vec![Symbol::Nonterminal('S')]]))),
            ((Symbol::Terminal('a'), 'a'), (Operator::ONE, HashSet::new())),
            ((Symbol::Terminal('b'), 'b'), (Operator::ONE, HashSet::new())),
            ((Symbol::Nonterminal('S'), 'a'), (Operator::PLUS, HashSet::from([
                vec![Symbol::Nonterminal('S'), Symbol::Terminal('b'), Symbol::Terminal('a'), Symbol::Terminal('b'), Symbol::AtomicLanguage('S', 'a', Operator::STAR)],
                vec![Symbol::Terminal('a'), Symbol::Terminal('a'), Symbol::Terminal('a'), Symbol::Terminal('b'), Symbol::AtomicLanguage('S', 'a', Operator::STAR)],
                vec![Symbol::Nonterminal('S'), Symbol::Terminal('b')],
                vec![Symbol::Terminal('a'), Symbol::Terminal('a')]
            ])))
        ]);
        println!("{:?}", grammar.atomics);

        assert!(g_generates_string(&grammar, "aaaab".to_string()));
        assert!(g_generates_string(&grammar, "aaa".to_string()));
        assert!(g_generates_string(&grammar, "aaaabab".to_string()));
        assert!(g_generates_string(&grammar, "aaaaabbab".to_string()));
        assert!(g_generates_string(&grammar, "aaaababab".to_string()));
        assert!(g_generates_string(&grammar, "aaaaabb".to_string()));
        assert!(!g_generates_string(&grammar, "aaaaab".to_string()));
        assert!(!g_generates_string(&grammar, "aaaabb".to_string()));
        assert!(!g_generates_string(&grammar, "aaaaa".to_string()));
    }

    #[test]
    fn difficult_bottom_up_test() {
        let grammar: Grammar = difficult_bottom_up_grammar();
        println!("{:?}", grammar.atomics);

        assert!(g_generates_string(&grammar, "a-a+a".to_string()));
        assert!(g_generates_string(&grammar, "a-a-a-a".to_string()));
        assert!(g_generates_string(&grammar, "a".to_string()));
        assert!(g_generates_string(&grammar, "a+a-a".to_string()));
        assert!(g_generates_string(&grammar, "a+a".to_string()));
        assert!(g_generates_string(&grammar, "a-a".to_string()));
        assert!(g_generates_string(&grammar, "a-a-a".to_string()));
        assert!(g_generates_string(&grammar, "a+a+a".to_string()));
        assert!(g_generates_string(&grammar, "a+a+a+a".to_string()));
        assert!(g_generates_string(&grammar, "a+a-a+a".to_string()));
        assert!(g_generates_string(&grammar, "a+a+a-a".to_string()));
        assert!(g_generates_string(&grammar, "a-a+a+a".to_string()));
        assert!(!g_generates_string(&grammar, "a-aa+a".to_string()));
        assert!(!g_generates_string(&grammar, "a-a+a+".to_string()));
    }

    #[test]
    fn odd_number_of_a_test() {
        let grammar: Grammar = odd_number_of_a_grammar();
        println!("{:?}", grammar.atomics);

        assert!(g_generates_string(&grammar, "a".to_string()));
        assert!(g_generates_string(&grammar, "aaa".to_string()));
        assert!(g_generates_string(&grammar, "aaaaa".to_string()));
        assert!(g_generates_string(&grammar, "aaaaaaa".to_string()));
        assert!(!g_generates_string(&grammar, "aa".to_string()));
        assert!(!g_generates_string(&grammar, "aaaa".to_string()));
        assert!(!g_generates_string(&grammar, "aaaaaa".to_string()));
    }

    #[test]
    fn direct_left_recursion_test() {
        let grammar: Grammar = direct_left_recursive_grammar();
        println!("{:?}", grammar.atomics);

        assert!(g_generates_string(&grammar, "a".to_string()));
        assert!(g_generates_string(&grammar, "aa".to_string()));
        assert!(g_generates_string(&grammar, "aaa".to_string()));
        assert!(g_generates_string(&grammar, "aaaa".to_string()));
        assert!(g_generates_string(&grammar, "aaaaa".to_string()));
        assert!(g_generates_string(&grammar, "aaaaaa".to_string()));
        assert!(g_generates_string(&grammar, "aaaaaaa".to_string()));
        assert!(g_generates_string(&grammar, "aaaaaaaa".to_string()));
    }

    fn setup_empty_grammar() -> Grammar {
        Grammar::new(HashSet::new(), HashSet::new(), 'S', HashMap::new())
    }

    #[test]
    fn print_grammar() {
        let grammar: Grammar = basic_relational_parsing_example_grammar();

        println!("{:?}", grammar);
    }

    #[test]
    fn test_word_slice() {
        let word: Word = vec![Symbol::Nonterminal('S'), Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')];
        let test: Word = word.clone()[1..].to_vec();

        assert_eq!(test, vec![Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')]);
    }

    #[test]
    fn test_atomics() {
        let grammar: Grammar = basic_relational_parsing_example_grammar();
        assert_eq!(grammar.atomics, HashMap::from([
            ((Symbol::Nonterminal(' '), ' '), (Operator::ONE, HashSet::from([vec![Symbol::Nonterminal('S')]]))),
            ((Symbol::Terminal('a'), 'a'), (Operator::ONE, HashSet::new())),
            ((Symbol::Terminal('b'), 'b'), (Operator::ONE, HashSet::new())),
            ((Symbol::Terminal('c'), 'c'), (Operator::ONE, HashSet::new())),
            ((Symbol::Nonterminal('S'), 'a'), (Operator::STAR, HashSet::from([
                vec![Symbol::Terminal('a'), Symbol::AtomicLanguage('S', 'a', Operator::STAR)],
                vec![Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c'), Symbol::AtomicLanguage('S', 'a', Operator::STAR)],
            ]))),
        ]));

        let grammar: Grammar = e_rule_relational_parsing_example_grammar();
        assert_eq!(grammar.atomics, HashMap::from([
            ((Symbol::Nonterminal(' '), ' '), (Operator::ONE, HashSet::from([vec![Symbol::Nonterminal('S')]]))),
            ((Symbol::Terminal('a'), 'a'), (Operator::ONE, HashSet::new())),
            ((Symbol::Terminal('b'), 'b'), (Operator::ONE, HashSet::new())),
            ((Symbol::Terminal('c'), 'c'), (Operator::ONE, HashSet::new())),
            ((Symbol::Nonterminal('S'), 'a'), (Operator::STAR, HashSet::from([
                vec![Symbol::Terminal('a'), Symbol::AtomicLanguage('S', 'a', Operator::STAR)],
                vec![Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c'), Symbol::AtomicLanguage('S', 'a', Operator::STAR)],
                vec![Symbol::Terminal('b'), Symbol::Terminal('c'), Symbol::AtomicLanguage('S', 'a', Operator::STAR)],
            ]))),
        ]));

        let grammar: Grammar = three_rule_grammar();
        assert_eq!(grammar.atomics, HashMap::from([
            ((Symbol::Nonterminal(' '), ' '), (Operator::ONE, HashSet::from([vec![Symbol::Nonterminal('S')]]))),
            ((Symbol::Terminal('a'), 'a'), (Operator::ONE, HashSet::new())),
            ((Symbol::Terminal('b'), 'b'), (Operator::ONE, HashSet::new())),
            ((Symbol::Nonterminal('S'), 'a'), (Operator::PLUS, HashSet::from([
                vec![Symbol::Terminal('a'), Symbol::Terminal('a'), Symbol::AtomicLanguage('S', 'a', Operator::STAR)],
                vec![Symbol::Nonterminal('S'), Symbol::Terminal('b'), Symbol::AtomicLanguage('S', 'a', Operator::STAR)],
                vec![Symbol::Terminal('a'), Symbol::Terminal('b'), Symbol::AtomicLanguage('S', 'a', Operator::STAR)]
            ])))
        ]));

        let grammar: Grammar = difficult_bottom_up_grammar();
        assert_eq!(grammar.atomics, HashMap::from([
            ((Symbol::Nonterminal(' '), ' '), (Operator::ONE, HashSet::from([vec![Symbol::Nonterminal('S')]]))),
            ((Symbol::Terminal('a'), 'a'), (Operator::ONE, HashSet::new())),
            ((Symbol::Terminal('+'), '+'), (Operator::ONE, HashSet::new())),
            ((Symbol::Terminal('-'), '-'), (Operator::ONE, HashSet::new())),
            ((Symbol::Nonterminal('Q'), '+'), (Operator::OPT, HashSet::new())),
            ((Symbol::Nonterminal('Q'), '-'), (Operator::OPT, HashSet::new())),
            ((Symbol::Nonterminal('F'), 'a'), (Operator::OPT, HashSet::new())),
            ((Symbol::Nonterminal('S'), 'a'), (Operator::ONE, HashSet::from([vec![Symbol::Nonterminal('Q'), Symbol::Nonterminal('F'), Symbol::AtomicLanguage('E', 'a', Operator::STAR)]]))),
            ((Symbol::Nonterminal('E'), 'a'), (Operator::PLUS, HashSet::from([vec![Symbol::Nonterminal('Q'), Symbol::Nonterminal('F'), Symbol::AtomicLanguage('E', 'a', Operator::STAR)]])))
        ]));
    }

    #[test]
    fn test_grammar_derive() {
        let grammar: Grammar = basic_relational_parsing_example_grammar();
        assert_eq!(Grammar::derive(&grammar.rules, Symbol::Terminal('a'), 'a', &HashMap::new()), Some((Operator::ONE, HashSet::new())));

        assert_eq!(Grammar::derive(&grammar.rules, Symbol::Terminal('a'), 'b', &HashMap::new()), None);

        assert_eq!(Grammar::derive(&grammar.rules, Symbol::Nonterminal('S'), 'a', &HashMap::new()),
            Some((Operator::ONE, HashSet::from([
                vec![Symbol::Empty],
                vec![Symbol::Nonterminal('S')],
                vec![Symbol::Terminal('a')],
                vec![Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')]
            ])))
        );
    }

    #[test]
    fn test_grammar_derive_nulling() {
        let grammar: Grammar = e_rule_relational_parsing_example_grammar();
        assert_eq!(Grammar::derive(&grammar.rules, Symbol::Nonterminal('S'), 'a', &HashMap::new()),
            Some((Operator::ONE, HashSet::from([
                vec![Symbol::Empty],
                vec![Symbol::Nonterminal('S')],
                vec![Symbol::Terminal('a')],
                vec![Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')],
                vec![Symbol::Terminal('b'), Symbol::Terminal('c')]
            ])))
        );
    }

    #[test]
    fn test_grammar_find_rule() {
        let grammar: Grammar = basic_relational_parsing_example_grammar();
        
        assert_eq!(Grammar::find_rule_with_terminal(&grammar.rules, 'S', 'a', &mut Vec::new()), HashSet::from([vec![Symbol::Terminal('a')]]));
        assert_eq!(Grammar::find_rule_with_terminal(&grammar.rules, 'S', 'b', &mut Vec::new()), HashSet::new());
        assert_eq!(Grammar::find_rule_with_terminal(&grammar.rules, 'S', 'c', &mut Vec::new()), HashSet::new());
    }

    #[test]
    fn test_break_grammar_find_rule() {
        let mut grammar: Grammar = basic_relational_parsing_example_grammar();
        grammar.rules.get_mut(&'S').expect("").push(vec![Symbol::Nonterminal('T')]);
        grammar.rules.insert('T', vec![vec![Symbol::Nonterminal('S')]]);
        Grammar::find_rule_with_terminal(&grammar.rules, 'S', 'a', &mut Vec::new());
    }
}