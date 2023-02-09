//! # Regex
//! 
//! The `regex` module contains data structures and associated methods which calculate atomic languages
//! according to the derivation rules of a provided grammar and expresses these atomic languages
//! as regular expressions.

use std::fmt;
use std::collections::{HashSet, HashMap, BTreeSet, VecDeque};

use crate::word::*;

// Used as an intermediary data structure, keeping track of some additional information while we calculate atomic languages.
#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub enum RegexSymbol {
    Terminal(Terminal),
    Nonterminal(Nonterminal),

    // Placeholder symbol for atomic languages calculated in the future. If these atomic languages will exist,
    // they will be prepended to the rest of the rule following the atomic language symbol.
    AtomicLanguage(Nonterminal, Terminal),

    // Expresses nulled symbols and its associated nulling rule
    Nulled(Rules),
    Epsilon,
}

// Calculates and stores atomic languages.
#[derive(Debug)]
pub struct Regex {
    pub regex: HashMap<(Nonterminal, Terminal), RegexNode>,
}

impl fmt::Display for Regex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for ((nonterminal, terminal), node) in &self.regex {
            write!(f, "[{}]({}): {}\n", nonterminal, terminal, node)?;
        }
        Ok(())
    }
}

impl Regex {

    /// Takes a set of terminals and a set of derivation rules and calculates its associated atomic languages as a regular expression.
    pub fn new(terminals: &HashSet<Terminal>, rules: &HashMap<Nonterminal, HashSet<Word>>) -> Regex {
        let mut atomic_regex_rules: HashMap<(Nonterminal, Terminal), (HashSet<(Vec<RegexSymbol>, Rules)>, HashSet<(Vec<RegexSymbol>, Rules)>, HashSet<(Vec<RegexSymbol>, Rules)>)> = HashMap::new();
        let mut queue: VecDeque<(Nonterminal, Terminal)> = VecDeque::new();

        for (nonterminal, rule_list) in rules {
            for terminal in terminals {
                let regex_rules = Regex::calculate_regex_rules(nonterminal, terminal, rule_list, rules);
                atomic_regex_rules.insert((*nonterminal, *terminal), regex_rules);
                queue.push_back((*nonterminal, *terminal));
            }
        }

        Regex {regex: Regex::build_regex_map(queue, atomic_regex_rules)}
    }

    // For a given nonterminal, its associated derivation rules and a terminal, sorts derivation rules in three separate sets according what properties they hold.
    // direct: The set of rules on which we can directly perform a derivative by any terminal.
    // recursive: The set of rules which start with the calling nonterminal, i.e. the rules which are directly left-recursive.
    // different_atomic: The set of rules which start with a different nonterminal.
    // calculate_regex_rules also adds new rules for any nonterminal which can be nulled.
    // Ex: suppose we have a grammar with rules of the form
    // S -> aSa
    // S -> Sa
    // S -> epsilon
    // then S -> aSa and S -> epsilon are sorted into direct, S -> Sa into recursive.
    // Additionally, the rules S -> aa and S -> a are created by nulling S in the first two rules and both are sorted into direct.
    fn calculate_regex_rules(nonterminal: &Nonterminal, terminal: &Terminal, rule_list: &HashSet<Vec<Symbol>>, rules: &HashMap<Nonterminal, HashSet<Word>>) -> (HashSet<(Vec<RegexSymbol>, Rules)>, HashSet<(Vec<RegexSymbol>, Rules)>, HashSet<(Vec<RegexSymbol>, Rules)>) {
        let mut direct: HashSet<(Vec<RegexSymbol>, Rules)> = HashSet::new();
        let mut recursive: HashSet<(Vec<RegexSymbol>, Rules)> = HashSet::new();
        let mut different_atomic: HashSet<(Vec<RegexSymbol>, Rules)> = HashSet::new();
        let mut nullable_atomic: bool = false;

        for rule in rule_list {
            if rule[0] == Symbol::Epsilon {
                nullable_atomic = true;
            }
            let regex_rule = Regex::word_to_regex_word(rule, *terminal);
            if regex_rule.len() > 0 {
                match regex_rule[0] {
                    RegexSymbol::AtomicLanguage(nt, _) => {
                        if *nonterminal == nt {
                            recursive.insert((regex_rule, vec![(*nonterminal, rule.clone())]));
                        } else {
                            different_atomic.insert((regex_rule, vec![(*nonterminal, rule.clone())]));
                        }
                    },
                    _ => {
                        direct.insert((regex_rule, vec![(*nonterminal, rule.clone())]));
                    },
                }
            }
        }

        let mut new_rules: HashSet<(Vec<RegexSymbol>, Rules)> = HashSet::new();
        Regex::null_rules(rules, &direct, nullable_atomic, &mut new_rules);
        Regex::null_rules(rules, &recursive, nullable_atomic, &mut new_rules);
        Regex::null_rules(rules, &different_atomic, nullable_atomic, &mut new_rules);

        for rule in new_rules {
            let no_nulled_rule = Regex::reg_symb_vec_without_nulled(&rule);
            let mut new_rules: Rules = rule.1.clone();
            let collected_rules: Rules = Regex::collect_starting_null_rules(&rule);
            if collected_rules.len() > 0 {
                new_rules.extend(collected_rules);
            }
            //println!("rule: {:?}", rule);
            //println!("no_nulled {:?}", no_nulled_rule);
            match no_nulled_rule.0[0] {
                RegexSymbol::AtomicLanguage(nt, _) => {
                    if *nonterminal == nt {
                        recursive.insert((rule.0.clone(), new_rules));
                    } else {
                        different_atomic.insert((rule.0.clone(), new_rules));
                    }
                },
                RegexSymbol::Epsilon => {
                    direct.insert((rule.0.clone(), new_rules));
                },
                RegexSymbol::Terminal(t) => {
                    if t == *terminal && nullable_atomic {
                        if no_nulled_rule.0.len() > 1 {
                            direct.insert((no_nulled_rule.0[1..].to_vec(), new_rules));
                        } else {
                            direct.insert((vec![RegexSymbol::Epsilon], new_rules));
                        }
                    }
                },
                _ => {},
            }
            //println!("direct: {:?}", direct);
        }
        //println!("nt: {}, t: {}", nonterminal, terminal);
        //println!("Direct: {:?}", direct);
        //println!("Recursive: {:?}", recursive);
        //println!("Different_atomic: {:?}", different_atomic);
        //println!();
        (direct, recursive, different_atomic)
    }
    
    // From an input, produces a new output with any Nulled symbols removed.
    fn reg_symb_vec_without_nulled(input: &(Vec<RegexSymbol>, Rules)) -> (Vec<RegexSymbol>, Rules) {
        let mut res: (Vec<RegexSymbol>, Rules) = (Vec::with_capacity(input.0.len()), input.1.clone());
        for regsymb in &input.0 {
            if let RegexSymbol::Nulled(_) = regsymb {
                if res.0.len() == 0 {
                    res.1.remove(0);
                } else {
                    res.0.push(regsymb.clone());
                }
            } else {
                res.0.push(regsymb.clone());
            }
        }
        if res.0.is_empty() {
            res.0.push(RegexSymbol::Epsilon);
        }
        res
    }

    // From an input starting with Nulled symbols, adds the associated rules until a non-nulled symbol is encountered.
    fn collect_starting_null_rules(input: &(Vec<RegexSymbol>, Rules)) -> Rules {
        let mut res: Rules = Vec::new();
        let mut i: usize = 0;
        while let Some(RegexSymbol::Nulled(rules)) = &input.0.get(i) {
            i += 1;
            res.extend(rules.clone());
        }
        res
    }

    // From the three lists, direct, recursive and different_atomic, calculates the atomic languages.
    fn build_regex_map(mut queue: VecDeque<(Nonterminal, Terminal)>, atomic_regex_rules: HashMap<(Nonterminal, Terminal), (HashSet<(Vec<RegexSymbol>, Rules)>, HashSet<(Vec<RegexSymbol>, Rules)>, HashSet<(Vec<RegexSymbol>, Rules)>)>) -> HashMap<(Nonterminal, Terminal), RegexNode> {
        let mut atomic_regex_rules_working_copy: HashMap<(Nonterminal, Terminal), (HashSet<(Vec<RegexSymbol>, Rules)>, HashSet<(Vec<RegexSymbol>, Rules)>, HashSet<(Vec<RegexSymbol>, Rules)>)> = atomic_regex_rules.clone();
        let mut res: HashMap<(Nonterminal, Terminal), RegexNode> = HashMap::new();

        while queue.len() > 0 {
            if let Some((nonterminal, terminal)) = queue.pop_front() {
                if let Some((direct, recursive, different_atomic)) = atomic_regex_rules_working_copy.get_mut(&(nonterminal, terminal)) {
                    for different_rule in different_atomic.clone() {
                        if let RegexSymbol::AtomicLanguage(nt, t) = different_rule.0[0] {
                            if let Some((ntdirect, ntrecursive, ntdifferent)) = atomic_regex_rules.get(&(nt, t)) {
                                for ntdirect_rule in ntdirect.clone() {
                                    if different_rule.0.len() <= 1 {
                                        direct.insert((ntdirect_rule.0.clone(), [&different_rule.1[..], &ntdirect_rule.1[..]].concat()));
                                    } else {
                                        direct.insert(([&ntdirect_rule.0[1..], &different_rule.0[1..]].concat(), [&different_rule.1[..], &ntdirect_rule.1[..]].concat()));
                                    }
                                }
                                for ntrecursive_rule in ntrecursive.clone() {
                                    recursive.insert(([&[RegexSymbol::AtomicLanguage(nonterminal, t)], &different_rule.0[1..], &ntrecursive_rule.0[1..]].concat(), ntrecursive_rule.1.clone()));
                                }
                                for ntdifferent_rule in ntdifferent.clone() {
                                    if let RegexSymbol::AtomicLanguage(nt2, _) = ntdifferent_rule.0[0] {
                                        if nt2 == nonterminal {
                                            recursive.insert(([&ntdifferent_rule.0[..], &different_rule.0[1..]].concat(), [&different_rule.1[..], &ntdifferent_rule.1[..]].concat()));
                                        } else {
                                            different_atomic.insert(([&ntdifferent_rule.0[..], &different_rule.0[1..]].concat(), [&different_rule.1[..], &ntdifferent_rule.1[..]].concat()));
                                        }
                                    }
                                }
                                different_atomic.remove(&different_rule);
                            }
                        }
                    }
                    
                    if different_atomic.len() == 0 && direct.len() > 0 {
                        if nonterminal == 'V' && terminal == '(' {
                            println!("[V](()");
                            println!("direct: {:?}", direct);
                            println!("different_atomic: {:?}", different_atomic);
                            println!("recursive: {:?}", recursive);
                        }
                        res.insert((nonterminal, terminal), Regex::build_regex_node(&direct, &Vec::new(), &recursive));
                    } else {
                        if different_atomic.len() > 0 {
                            let mut all_in_res: bool = true;
                            let mut regex_nodes: Vec<(RegexNode, Rules)> = Vec::with_capacity(different_atomic.len());
                            for different_rule in different_atomic.iter() {
                                if let RegexSymbol::AtomicLanguage(nt, t) = different_rule.0[0] {
                                    if let Some(regex) = res.get(&(nt, t)) {
                                        regex_nodes.push((regex.clone(), different_rule.1.clone()));
                                    } else if queue.contains(&(nt, t)) {
                                        all_in_res = false;
                                        queue.push_back((nonterminal, terminal));
                                        break;
                                    } else {
                                        all_in_res = false;
                                    }
                                }
                            }
                            if all_in_res {
                                if nonterminal == 'V' && terminal == '(' {
                                    println!("[V](()");
                                    println!("direct: {:?}", direct);
                                    println!("different_atomic: {:?}", different_atomic);
                                    println!("recursive: {:?}", recursive);
                                    println!("different_recursive: {:?}", regex_nodes);
                                }
                                res.insert((nonterminal, terminal), Regex::build_regex_node(&direct, &regex_nodes, &recursive));
                                continue;
                            }
                        }
                    }
                }
            }
        }
        res
    }

    // Produces all possible combinations of nulled rules.
    // Ex. consider the following grammar:
    // S -> SaSbS
    // S -> epsilon
    // the set of new rules will be:
    // S -> SaSb, S -> SabS, S -> aSbS, S -> Sab, S -> aSb, S -> abS, S -> ab
    fn null_rules(rules: &HashMap<Nonterminal, HashSet<Word>>, rule_list: &HashSet<(Vec<RegexSymbol>, Rules)>, nullable_atomic: bool, new_rules: &mut HashSet<(Vec<RegexSymbol>, Rules)>) {
        for rule in rule_list {
            let mut nullable_positions: Vec<(usize, Rules)> = Vec::new();
            for (pos, symbol) in rule.0.iter().enumerate() {
                match symbol {
                    RegexSymbol::Nonterminal(nt) => {
                        if Regex::nullable_nonterminal(rules, &nt) {
                            nullable_positions.push((pos, vec![(*nt, vec![Symbol::Epsilon])]));
                        }
                    },
                    RegexSymbol::AtomicLanguage(nt, _) => {
                        if nullable_atomic && Regex::nullable_nonterminal(rules, &nt) {
                            nullable_positions.push((pos, vec![(*nt, vec![Symbol::Epsilon])]));
                        }
                    }
                    _ => {},
                }
            }

            let mut nullable_combinations: Vec<Vec<(usize, Rules)>> = Regex::powerset(&nullable_positions);
            //nullable_combinations.remove(0); //remove [] from nullable combinations
                for comb in nullable_combinations {
                    let mut rev = comb.clone();
                    rev.sort();
                    rev.reverse();
                    let mut new_word = rule.0.clone();
                    for (index, erasing_rules) in rev {
                        new_word[index] = RegexSymbol::Nulled(erasing_rules);
                    }
                    if rule.1.len() > 0 {
                        new_rules.insert((new_word, rule.1.clone()));
                    }
                }
        }
    }

    fn powerset<T>(s: &[T]) -> Vec<Vec<T>> where T: Clone {
        (0..2usize.pow(s.len() as u32)).map(|i| {
            s.iter().enumerate().filter(|&(t, _)| (i >> t) % 2 == 1)
                .map(|(_, element)| element.clone()).collect()
        }).collect()
    }

    // Determines if a nonterminal has an epsilon rule.
    fn nullable_nonterminal(rules: &HashMap<Nonterminal, HashSet<Word>>, nonterminal: &Nonterminal) -> bool {
        if let Some(rule_list) = rules.get(nonterminal) {
            for rule in rule_list {
                if *rule == vec![Symbol::Epsilon] {
                    return true;
                }
            }
        }
        false
    }

    // From the two sets direct, recursive and a new different_recursive set, calculates the associated regular expression.
    fn build_regex_node(direct: &HashSet<(Vec<RegexSymbol>, Rules)>, different_recursive: &Vec<(RegexNode, Rules)>, recursive: &HashSet<(Vec<RegexSymbol>, Rules)>) -> RegexNode {
        let mut res_nodes: Vec<WordNode> = Vec::new();
        if direct.len() > 0 {
            let mut direct_word_set: BTreeSet<(WordNodeWord, Rules)> = BTreeSet::new();
            for direct_rule in direct {
                direct_word_set.insert((Regex::regex_word_to_word(&direct_rule.0), direct_rule.1.clone()));
            }
            res_nodes.push(WordNode{words: direct_word_set, kleene_star: false});
        }
        if different_recursive.len() > 0 {
            println!("using diff rec");
            if different_recursive.len() == 1 {
                res_nodes.extend(different_recursive[0].0.append_rule(&different_recursive[0].1));
            } else {
                let mut different_recursive_node_vec: Vec<WordNode> = Vec::with_capacity(different_recursive.len());
                for different_recursive_node in different_recursive {
                    different_recursive_node_vec.extend(different_recursive_node.0.append_rule(&different_recursive_node.1));
                }
                res_nodes.extend(different_recursive_node_vec);
            }
        }
        if recursive.len() > 0 {
            let mut recursive_word_set: BTreeSet<(WordNodeWord, Rules)> = BTreeSet::new();
            for recursive_rule in recursive {
                let word = Regex::regex_word_to_word(&recursive_rule.0[1..].to_vec());
                if word.len() > 0 {
                    recursive_word_set.insert((word, recursive_rule.1.clone()));
                }
            }
            res_nodes.push(WordNode{words: recursive_word_set, kleene_star: true});
        }
        RegexNode{nodes: res_nodes}
    }

    pub fn word_to_regex_word(rule: &Word, terminal: Terminal) -> Vec<RegexSymbol> {
        match rule[0] {
            Symbol::Nonterminal(nt) => {
                let mut res: Vec<RegexSymbol> = Vec::with_capacity(rule.len());
                res.push(RegexSymbol::AtomicLanguage(nt, terminal));
                for symbol in &rule[1..] {
                    res.push(Regex::symbol_to_regex_symbol(symbol));
                }
                res
            },
            Symbol::Terminal(t) => {
                if t == terminal {
                    if rule.len() > 1 {
                        let mut res: Vec<RegexSymbol> = Vec::with_capacity(rule.len());
                        for symbol in &rule[1..] {
                            res.push(Regex::symbol_to_regex_symbol(symbol));
                        }
                        res
                    } else {
                        vec![RegexSymbol::Epsilon]
                    }
                } else {
                    Vec::new()
                }
            },
            _ => {Vec::new()},
        }
    }

    fn symbol_to_regex_symbol(symbol: &Symbol) -> RegexSymbol {
        match symbol {
            Symbol::Nonterminal(nt) => RegexSymbol::Nonterminal(*nt),
            Symbol::Terminal(t) => RegexSymbol::Terminal(*t),
            Symbol::Epsilon => RegexSymbol::Epsilon
        }
    }

    pub fn regex_word_to_word(regex_word: &Vec<RegexSymbol>) -> WordNodeWord {
        let mut res: WordNodeWord = Vec::with_capacity(regex_word.len());
        for regex_symbol in regex_word {
            if let Some(symbol) = Regex::regex_symbol_to_symbol(regex_symbol) {
                res.push(symbol);
            } else {
                return Vec::new();
            }
        }
        res
    }

    fn regex_symbol_to_symbol(regex_symbol: &RegexSymbol) -> Option<WordNodeSymbol> {
        match regex_symbol {
            RegexSymbol::Nonterminal(nt) => Some(WordNodeSymbol::Nonterminal(*nt)),
            RegexSymbol::Terminal(t) => Some(WordNodeSymbol::Terminal(*t)),
            RegexSymbol::Epsilon => Some(WordNodeSymbol::Epsilon),
            RegexSymbol::Nulled(rules) => Some(WordNodeSymbol::Rules(rules.clone())),
            RegexSymbol::AtomicLanguage(_, _) => None,
        }
    }
    
    pub fn print_with_rules(&self) {
        for ((nonterminal, terminal), node) in &self.regex {
            print!("[{}]({}): ", nonterminal, terminal);
            node.print_with_rules();
            println!();
        }
        println!();
    }

}

// Represents a regular expression.
#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub struct RegexNode {
    pub nodes: Vec<WordNode>,
}

impl fmt::Display for RegexNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(")?;
        for node in &self.nodes {
            write!(f, "{}", node)?;
        }
        write!(f, ")")
    }
}

impl RegexNode {
    fn append_rule(&self, rules: &Rules) -> Vec<WordNode> {
        let mut new_nodes: Vec<WordNode> = Vec::new();
        for n in &self.nodes {
            new_nodes.push(n.append_rule(rules));
        }
        new_nodes
    }

    pub fn print_with_rules(&self) {
        print!("(");
        for node in &self.nodes {
            node.print_with_rules();
        }
        print!(")");
    }

    pub fn is_e_node_get_rules(&self) -> (bool, HashSet<Rules>) {
        let mut is_e: bool = true;
        let mut rules_set: HashSet<Rules> = HashSet::new();
        for node in &self.nodes {
            if let (true, rules) = node.is_e_node_get_rules() {
                rules_set.extend(rules);
            } else {
                is_e = false;
            }
        }
        (is_e, rules_set)
    }
}

pub type Rule = (Nonterminal, Word);
pub type Rules = Vec<Rule>;
pub type RulesSet = HashSet<Rules>;

pub fn print_rules_set(rules_set: &RulesSet, f: &mut fmt::Formatter) -> fmt::Result {
    for rules in rules_set {
        print_rules(rules, f)?;
    }
    Ok(())
}

pub fn print_rules(rules: &Rules, f: &mut fmt::Formatter) -> fmt::Result {
    for rule in rules {
        print_rule(rule, f)?;
    }
    Ok(())
}

pub fn print_rule(rule: &Rule, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "[{} -> ", rule.0)?;
    for symbol in &rule.1 {
        write!(f, "{}", symbol)?;
    }
    write!(f, "]")
}

// Placeholder structure similar to Symbol, allowing us to keep track of the rules used to null symbols.
// Only used to generate the finite_state_automaton.
#[derive(Eq, PartialEq, Debug, Clone, Hash, PartialOrd, Ord)]
pub enum WordNodeSymbol {
    Rules(Rules),
    Terminal(Terminal),
    Nonterminal(Nonterminal),
    Epsilon,
}

impl fmt::Display for WordNodeSymbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            WordNodeSymbol::Terminal(t) => write!(f, "{}", t),
            WordNodeSymbol::Nonterminal(nt) => write!(f, "{}", nt),
            WordNodeSymbol::Epsilon => write!(f, "e"),
            WordNodeSymbol::Rules(rules) => {
                for (nonterminal, rule) in rules {
                    write!(f, "[{} -> ", nonterminal)?;
                    for symbol in rule {
                        write!(f, "{}", symbol)?;
                    }
                    write!(f, "]")?;
                }
                Ok(())
            },
        }
    }
}

pub type WordNodeWord = Vec<WordNodeSymbol>;

// Partial regular expression. Keeps track of words contained in this part of the expression along with the rules applied to produce that word.
// When WordNode contains multiple words, each word is considered an alternative. I.E. they form a list separated by the regex '+' operator.
#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub struct WordNode {
    pub words: BTreeSet<(WordNodeWord, Rules)>,
    pub kleene_star: bool,
}

impl fmt::Display for WordNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(")?;
        let mut words_iter = self.words.iter().peekable();
        while words_iter.peek().is_some() {
            write!(f, "\"")?;
            for symbol in &words_iter.next().unwrap().0 {
                write!(f, "{}", symbol)?;
            }
            write!(f, "\"")?;
            if words_iter.peek().is_some() {
                write!(f, " + ")?;
            }
        }
        if self.kleene_star {
            write!(f, ")*")
        } else {
            write!(f, ")")
        }
    }
}

impl WordNode {
    fn append_rule(&self, rules: &Rules) -> WordNode {
        let mut new_words: BTreeSet<(WordNodeWord, Rules)> = BTreeSet::new();
        for (word, rule) in &self.words {
            new_words.insert((word.clone(), [rules.clone(), rule.clone()].concat()));
        }
        WordNode {words: new_words, kleene_star: self.kleene_star}
    }

    pub fn get_by_base_rules(&self) -> HashMap<Rules, HashSet<WordNodeWord>> {
        let mut res: HashMap<Rules, HashSet<WordNodeWord>> = HashMap::new();

        for (word, rules) in &self.words {
            res.entry(rules.clone()).or_default().insert(word.clone());
        }

        res
    }

    pub fn print_with_rules(&self) {
        print!("(");
        let mut words_iter = self.words.iter().peekable();
        while words_iter.peek().is_some() {
            if let Some((symbols, rules)) = words_iter.next() {
                for symbol in symbols {
                    print!("{}", symbol);
                }
                print!(" [");
                for (nonterminal, word) in rules {
                    print!(" {} -> ", nonterminal);
                    for symbol in word {
                        print!("{}", symbol);
                    }
                }
                print!(" ] ");
            }
            if words_iter.peek().is_some() {
                print!(" + ");
            }
        }
        if self.kleene_star {
            print!(")*");
        } else {
            print!(")");
        }
    }

    pub fn is_e_node(&self) -> bool {
        let mut res: bool = true;
        for (word, _) in &self.words {
            if word != &vec![WordNodeSymbol::Epsilon] {
                res = false;
            }
        }
        res
    }

    pub fn get_rules(&self) -> HashSet<Rules> {
        let mut res: HashSet<Rules> = HashSet::new();
        for (_, rules) in &self.words {
            res.insert(rules.clone());
        }
        res
    }

    pub fn is_e_node_get_rules(&self) -> (bool, HashSet<Rules>) {
        let mut is_e: bool = true;
        let mut rule_set: HashSet<Rules> = HashSet::new();
        for (word, rules) in &self.words {
            if word != &vec![WordNodeSymbol::Epsilon] {
                is_e = false;
            } else {
                rule_set.insert(rules.clone());
            }
        }
        (is_e, rule_set)
    }
}

#[cfg(test)]
mod tests{
    use super::*;

    #[test]
    fn rule_to_regex_rule_test() {
        assert_eq!(Regex::word_to_regex_word(&vec![Symbol::Terminal('a'), Symbol::Nonterminal('S'), Symbol::Terminal('b')], 'a'), vec![RegexSymbol::Nonterminal('S'), RegexSymbol::Terminal('b')]);
        assert_eq!(Regex::word_to_regex_word(&vec![Symbol::Nonterminal('S'), Symbol::Terminal('a'), Symbol::Terminal('b')], 'a'), vec![RegexSymbol::AtomicLanguage('S', 'a'), RegexSymbol::Terminal('a'), RegexSymbol::Terminal('b')]);
        assert_eq!(Regex::word_to_regex_word(&vec![Symbol::Terminal('a'), Symbol::Terminal('a'), Symbol::Terminal('a')], 'a'), vec![RegexSymbol::Terminal('a'), RegexSymbol::Terminal('a')]);
        assert_eq!(Regex::word_to_regex_word(&vec![Symbol::Terminal('a')], 'a'), vec![RegexSymbol::Epsilon]);
        assert_eq!(Regex::word_to_regex_word(&vec![Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('b')], 'a'), vec![]);
    }

    #[test]
    fn regex_word_to_word_test() {
        assert_eq!(Regex::regex_word_to_word(&vec![RegexSymbol::Terminal('a'), RegexSymbol::Nonterminal('b')]), vec![WordNodeSymbol::Terminal('a'), WordNodeSymbol::Nonterminal('b')]);
        assert_eq!(Regex::regex_word_to_word(&vec![RegexSymbol::Terminal('a'), RegexSymbol::Nonterminal('b'), RegexSymbol::Epsilon]), vec![WordNodeSymbol::Terminal('a'), WordNodeSymbol::Nonterminal('b'), WordNodeSymbol::Epsilon]);
        assert_eq!(Regex::regex_word_to_word(&vec![RegexSymbol::AtomicLanguage('a', 'b')]), vec![]);
        assert_eq!(Regex::regex_word_to_word(&vec![RegexSymbol::Terminal('a'), RegexSymbol::AtomicLanguage('a', 'b')]), vec![]);
        assert_eq!(Regex::regex_word_to_word(&vec![RegexSymbol::Nonterminal('a'), RegexSymbol::AtomicLanguage('a', 'b')]), vec![]);
    }
}