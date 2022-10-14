use std::fmt;
use std::collections::{HashSet, HashMap, BTreeSet, VecDeque};

use crate::word::*;

#[derive(Eq, PartialEq, Hash, Debug, Clone, Copy)]
pub enum RegexSymbol {
    Terminal(Terminal),
    Nonterminal(Nonterminal),
    AtomicLanguage(Nonterminal, Terminal),
    Epsilon,
}

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
    pub fn new(terminals: &HashSet<Terminal>, rules: &HashMap<Nonterminal, HashSet<Word>>) -> Regex {
        let mut atomic_regex_rules: HashMap<(Nonterminal, Terminal), (HashSet<(Vec<RegexSymbol>, Rules)>, HashSet<(Vec<RegexSymbol>, Rules)>, HashSet<(Vec<RegexSymbol>, Rules)>)> = HashMap::new();
        let mut queue: VecDeque<(Nonterminal, Terminal)> = VecDeque::new();

        for (nonterminal, rule_list) in rules {
            for terminal in terminals {
                //println!("Nonterminal: {}, terminal: {}", nonterminal, terminal);
                let regex_rules = Regex::calculate_regex_rules(nonterminal, terminal, rules, rule_list);
                atomic_regex_rules.insert((*nonterminal, *terminal), regex_rules);
                queue.push_back((*nonterminal, *terminal));
            }
        }

        Regex {regex: Regex::build_regex_map(queue, atomic_regex_rules)}
    }

    fn calculate_regex_rules(nonterminal: &Nonterminal, terminal: &Terminal, rules: &HashMap<Nonterminal, HashSet<Word>>, rule_list: &HashSet<Vec<Symbol>>) -> (HashSet<(Vec<RegexSymbol>, Rules)>, HashSet<(Vec<RegexSymbol>, Rules)>, HashSet<(Vec<RegexSymbol>, Rules)>) {
        let mut direct: HashSet<(Vec<RegexSymbol>, Rules)> = HashSet::new();
        let mut recursive: HashSet<(Vec<RegexSymbol>, Rules)> = HashSet::new();
        let mut different_atomic: HashSet<(Vec<RegexSymbol>, Rules)> = HashSet::new();
        let mut nullable_atomic: bool = false;

        for rule in rule_list {
            //println!("Rules: {:?}", rule);
            if rule[0] == Symbol::Epsilon {
                nullable_atomic = true;
            }
            if let Some(regex_rule) = Regex::rule_to_regex_rule(rule, *terminal) {
                //println!("Regex rule: {:?}", regex_rule);
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
        let mut new_rules = Regex::null_rules(rules, &mut direct, nullable_atomic);
        new_rules.extend(Regex::null_rules(rules, &mut recursive, nullable_atomic));
        new_rules.extend(Regex::null_rules(rules, &mut different_atomic, nullable_atomic));

        for rule in new_rules {
            match rule.0[0] {
                RegexSymbol::AtomicLanguage(nt, _) => {
                    if *nonterminal == nt {
                        recursive.insert(rule);
                    } else {
                        different_atomic.insert(rule);
                    }
                },
                RegexSymbol::Epsilon => {direct.insert(rule);},
                RegexSymbol::Terminal(t) => {
                    if t == *terminal && nullable_atomic {
                        if rule.0.len() > 1 {
                            direct.insert((rule.0[1..].to_vec(), rule.1));
                        } else {
                            direct.insert((vec![RegexSymbol::Epsilon], rule.1));
                        }
                    }
                },
                _ => {},
            }
        }
        (direct, recursive, different_atomic)
    }

    fn build_regex_map(mut queue: VecDeque<(Nonterminal, Terminal)>, atomic_regex_rules: HashMap<(Nonterminal, Terminal), (HashSet<(Vec<RegexSymbol>, Rules)>, HashSet<(Vec<RegexSymbol>, Rules)>, HashSet<(Vec<RegexSymbol>, Rules)>)>) -> HashMap<(Nonterminal, Terminal), RegexNode> {
        let mut atomic_regex_rules_working_copy: HashMap<(Nonterminal, Terminal), (HashSet<(Vec<RegexSymbol>, Rules)>, HashSet<(Vec<RegexSymbol>, Rules)>, HashSet<(Vec<RegexSymbol>, Rules)>)> = atomic_regex_rules.clone();
        let mut res: HashMap<(Nonterminal, Terminal), RegexNode> = HashMap::new();

        while queue.len() > 0 {
            if let Some((nonterminal, terminal)) = queue.pop_front() {
                //println!("Current (nonterminal, terminal): {:?}", (nonterminal, terminal));
                if let Some((direct, recursive, different_atomic)) = atomic_regex_rules_working_copy.get_mut(&(nonterminal, terminal)) {
                    //println!("Current different: {:?}", different_atomic);
                    //let mut different_recursives: HashSet<Vec<RegexSymbol>> = HashSet::new();
                    for different_rule in different_atomic.clone() {
                        if let RegexSymbol::AtomicLanguage(nt, t) = different_rule.0[0] {
                            if let Some((ntdirect, ntrecursive, ntdifferent)) = atomic_regex_rules.get(&(nt, t)) {
                                for ntdirect_rule in ntdirect.clone() {
                                    direct.insert(([&ntdirect_rule.0[..], &different_rule.0[1..]].concat(), [&ntdirect_rule.1[..], &different_rule.1[..]].concat()));
                                }
                                for ntrecursive_rule in ntrecursive.clone() {
                                    recursive.insert(([&[RegexSymbol::AtomicLanguage(nonterminal, t)], &ntrecursive_rule.0[1..], &different_rule.0[1..]].concat(), ntrecursive_rule.1.clone()));
                                }
                                for ntdifferent_rule in ntdifferent.clone() {
                                    if let RegexSymbol::AtomicLanguage(nt2, _) = ntdifferent_rule.0[0] {
                                        if nt2 == nonterminal {
                                            recursive.insert(([&ntdifferent_rule.0[..], &different_rule.0[1..]].concat(), [&ntdifferent_rule.1[..], &different_rule.1[..]].concat()));
                                        } else {
                                            different_atomic.insert(([&ntdifferent_rule.0[..], &different_rule.0[1..]].concat(), [&ntdifferent_rule.1[..], &different_rule.1[..]].concat()));
                                        }
                                    }
                                }
                                different_atomic.remove(&different_rule);
                            }
                        }
                    }
                    
                    //println!("Current direct: {:?}", direct);
                    //println!("Current recursive: {:?}", recursive);
                    //println!("Current different: {:?}", different_atomic);
                    if different_atomic.len() == 0 && direct.len() > 0 {
                        res.insert((nonterminal, terminal), Regex::build_regex_node(&direct, &Vec::new(), &recursive));
                    } else {
                        if different_atomic.len() > 0 { //&& different_atomic.clone() == different_recursives {
                            let mut all_in_res: bool = true;
                            let mut regex_nodes: Vec<(RegexNode, Rules)> = Vec::new();
                            for different_rule in different_atomic.iter() {
                                if let RegexSymbol::AtomicLanguage(nt, t) = different_rule.0[0] {
                                    if let Some(regex) = res.get(&(nt, t)) {
                                        regex_nodes.push((regex.clone(), different_rule.1.clone()));
                                    } else if queue.contains(&(nt, t)) {
                                        all_in_res = false;
                                        queue.push_back((nonterminal, terminal));
                                    } else {
                                        all_in_res = false;
                                    }
                                }
                                //else {
                                //    all_in_res = false;
                                //}
                            }
                            if all_in_res {
                                res.insert((nonterminal, terminal), Regex::build_regex_node(&direct, &regex_nodes, &recursive));
                                continue;
                            }
                        }
                        //second_pass_queue.push_back((nonterminal, terminal));
                    }
                    //println!("End different: {:?}", different);
                }
            }
        }
        res
    }

    fn null_rules(rules: &HashMap<Nonterminal, HashSet<Word>>, rule_list: &HashSet<(Vec<RegexSymbol>, Rules)>, nullable_atomic: bool) -> HashSet<(Vec<RegexSymbol>, Rules)> {
        //println!("Rules_list: {:?}", rule_list);
        let mut new_rules: HashSet<(Vec<RegexSymbol>, Rules)> = HashSet::new();
        for rule in rule_list {
            //println!("Rules: {:?}", rule);
            let mut nullable_positions: Vec<(usize, Rules)> = Vec::new();
            for (pos, symbol) in rule.0.iter().enumerate() {
                //println!("Symbol: {:?}", symbol);
                match symbol {
                    RegexSymbol::Nonterminal(nt) => {
                        //println!("Nullable_nonterminal: {}", Regex::nullable_nonterminal(rules, &nt));
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
            //println!("Nullable_positions: {:?}", nullable_positions);

            let nullable_combinations: Vec<Vec<(usize, Rules)>> = Regex::powerset(&nullable_positions);
            for comb in nullable_combinations {
                let mut rev = comb.clone();
                rev.sort();
                rev.reverse();
                let mut new_word = rule.0.clone();
                let mut new_rule = rule.1.clone();
                for (index, erasing_rules) in rev {
                    new_word.remove(index);
                    for erasing_rule in erasing_rules {
                        new_rule.insert(0, erasing_rule);
                    }
                }
                //println!("New rule: {:?}", new_rule);
                if new_rule.len() > 0 {
                    new_rules.insert((new_word, new_rule));
                }
            }
        }
        new_rules
    }

    fn powerset<T>(s: &[T]) -> Vec<Vec<T>> where T: Clone {
        (0..2usize.pow(s.len() as u32)).map(|i| {
            s.iter().enumerate().filter(|&(t, _)| (i >> t) % 2 == 1)
                .map(|(_, element)| element.clone()).collect()
        }).collect()
    }

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

    fn prepend_rule(node: &RegexNode, rules: &Rules) -> RegexNode {
        let new_node: RegexNode;
        match node {
            RegexNode::Node(node) => {
                let mut new_nodes: Vec<RegexNode> = Vec::new();
                for n in &node.nodes {
                    new_nodes.push(Regex::prepend_rule(&n, rules))
                }
                new_node = RegexNode::Node(NodeNode {nodes: new_nodes});
            },
            RegexNode::Word(node) => {
                let mut new_words: BTreeSet<(Vec<Symbol>, Rules)> = BTreeSet::new();
                for (word, rule) in &node.words {
                    new_words.insert((word.clone(), [rule.clone(), rules.clone()].concat()));
                }
                new_node = RegexNode::Word(WordNode { words: new_words, kleene_star: node.kleene_star });
            },
        }
        new_node
    }

    fn build_regex_node(direct: &HashSet<(Vec<RegexSymbol>, Rules)>, different_recursive: &Vec<(RegexNode, Rules)>, recursive: &HashSet<(Vec<RegexSymbol>, Rules)>) -> RegexNode {
        let mut res_nodes: Vec<RegexNode> = Vec::new();
        if direct.len() > 0 {
            let mut direct_word_set: BTreeSet<(Word, Rules)> = BTreeSet::new();
            for direct_rule in direct {
                if let Some(word) = Regex::regex_word_to_word(&direct_rule.0) {
                    direct_word_set.insert((word, direct_rule.1.clone()));
                }
            }
            res_nodes.push(RegexNode::Word(WordNode{words: direct_word_set, kleene_star: false}));
        }
        if different_recursive.len() > 0 {
            if different_recursive.len() == 1 {
                res_nodes.push(Regex::prepend_rule(&different_recursive[0].0, &different_recursive[0].1));
            } else {
                let mut different_recursive_node_vec: Vec<RegexNode> = Vec::new();
                for different_recursive_node in different_recursive {
                    different_recursive_node_vec.push(Regex::prepend_rule(&different_recursive_node.0, &different_recursive_node.1));
                }
                res_nodes.push(RegexNode::Node(NodeNode{nodes: different_recursive_node_vec}));
            }
        }
        if recursive.len() > 0 {
            let mut recursive_word_set: BTreeSet<(Word, Rules)> = BTreeSet::new();
            for recursive_rule in recursive {
                if let Some(word) = Regex::regex_word_to_word(&recursive_rule.0[1..].to_vec()) {
                    if word.len() > 0 {
                        recursive_word_set.insert((word, recursive_rule.1.clone()));
                    }
                }
            }
            res_nodes.push(RegexNode::Word(WordNode{words: recursive_word_set, kleene_star: true}));
        }
        if res_nodes.len() == 1 {
            res_nodes.pop().unwrap()
        } else {
            RegexNode::Node(NodeNode{nodes: res_nodes})
        }
    }

    pub fn rule_to_regex_rule(rule: &Word, terminal: Terminal) -> Option<Vec<RegexSymbol>> {
        match rule[0] {
            Symbol::Nonterminal(nt) => {
                let mut res: Vec<RegexSymbol> = Vec::new();
                res.push(RegexSymbol::AtomicLanguage(nt, terminal));
                for symbol in &rule[1..] {
                    res.push(Regex::symbol_to_regex_symbol(symbol));
                }
                Some(res)
            },
            Symbol::Terminal(t) => {
                if t == terminal {
                    if rule.len() > 1 {
                        let mut res: Vec<RegexSymbol> = Vec::new();
                        for symbol in &rule[1..] {
                            res.push(Regex::symbol_to_regex_symbol(symbol));
                        }
                        Some(res)
                    } else {
                        Some(vec![RegexSymbol::Epsilon])
                    }
                } else {
                    None
                }
            },
            _ => {None},
        }
    }

    fn symbol_to_regex_symbol(symbol: &Symbol) -> RegexSymbol {
        match symbol {
            Symbol::Nonterminal(nt) => RegexSymbol::Nonterminal(*nt),
            Symbol::Terminal(t) => RegexSymbol::Terminal(*t),
            Symbol::Epsilon => RegexSymbol::Epsilon
        }
    }

    pub fn regex_word_to_word(regex_word: &Vec<RegexSymbol>) -> Option<Word> {
        let mut res: Word = Vec::new();
        for regex_symbol in regex_word {
            if let Some(symbol) = Regex::regex_symbol_to_symbol(regex_symbol) {
                res.push(symbol);
            } else {
                return None
            }
        }
        Some(res)
    }

    fn regex_symbol_to_symbol(regex_symbol: &RegexSymbol) -> Option<Symbol> {
        match regex_symbol {
            RegexSymbol::Nonterminal(nt) => Some(Symbol::Nonterminal(*nt)),
            RegexSymbol::Terminal(t) => Some(Symbol::Terminal(*t)),
            RegexSymbol::Epsilon => Some(Symbol::Epsilon),
            RegexSymbol::AtomicLanguage(_, _) => None
        }
    }
    
    pub fn print_with_rules(&self) {
        for ((nonterminal, terminal), node) in &self.regex {
            print!("[{}]({}): ", nonterminal, terminal);
            node.print_with_rules();
            println!();
        }
    }

}

#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub enum RegexNode {
    Word(WordNode),
    Node(NodeNode),
}

impl fmt::Display for RegexNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RegexNode::Word(node) => write!(f, "{}", node),
            RegexNode::Node(node) => write!(f, "{}", node),
        }
    }
}

impl RegexNode {
    pub fn print_with_rules(&self) {
        match self {
            RegexNode::Word(node) => node.print_with_rules(),
            RegexNode::Node(node) => node.print_with_rules(),
        }
    }

    pub fn is_e_node(&self) -> bool {
        match self {
            RegexNode::Word(node) => {node.is_e_node()},
            RegexNode::Node(node) => {node.is_e_node()},
        }
    }
}

#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub struct NodeNode {
    pub nodes: Vec<RegexNode>,
}

impl fmt::Display for NodeNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(")?;
        for node in &self.nodes {
            write!(f, "{}", node)?;
        }
        write!(f, ")")
    }
}

impl NodeNode {
    pub fn print_with_rules(&self) {
        print!("(");
        for node in &self.nodes {
            node.print_with_rules();
        }
        print!(")");
    }

    pub fn is_e_node(&self) -> bool {
        let mut res: bool = true;
        for node in &self.nodes {
            if !node.is_e_node() {
                res = false;
            }
        }
        res
    }
}

pub type Rules = Vec<(Nonterminal, Word)>;

#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub struct WordNode {
    pub words: BTreeSet<(Word, Rules)>,
    pub kleene_star: bool,
}

impl fmt::Display for WordNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(")?;
        let mut words_iter = self.words.iter().peekable();
        while words_iter.peek().is_some() {
            for symbol in &words_iter.next().unwrap().0 {
                write!(f, "{}", symbol)?;
            }
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
            if word != &vec![Symbol::Epsilon] {
                res = false;
            }
        }
        res
    }

    pub fn is_e_node_get_rules(&self) -> (bool, Option<HashSet<Rules>>) {
        let mut res: (bool, Option<HashSet<Rules>>) = (true, None);
        let mut rule_set: HashSet<Rules> = HashSet::new();
        for (word, rules) in &self.words {
            if word != &vec![Symbol::Epsilon] {
                res.0 = false;
            } else {
                rule_set.insert(rules.clone());
            }
        }
        if !rule_set.is_empty() {
            res.1 = Some(rule_set);
        }
        res
    }
}

#[cfg(test)]
mod tests{
    use super::*;

    #[test]
    fn rule_to_regex_rule_test() {
        assert_eq!(Regex::rule_to_regex_rule(&vec![Symbol::Terminal('a'), Symbol::Nonterminal('S'), Symbol::Terminal('b')], 'a'), Some(vec![RegexSymbol::Nonterminal('S'), RegexSymbol::Terminal('b')]));
        assert_eq!(Regex::rule_to_regex_rule(&vec![Symbol::Nonterminal('S'), Symbol::Terminal('a'), Symbol::Terminal('b')], 'a'), Some(vec![RegexSymbol::AtomicLanguage('S', 'a'), RegexSymbol::Terminal('a'), RegexSymbol::Terminal('b')]));
        assert_eq!(Regex::rule_to_regex_rule(&vec![Symbol::Terminal('a'), Symbol::Terminal('a'), Symbol::Terminal('a')], 'a'), Some(vec![RegexSymbol::Terminal('a'), RegexSymbol::Terminal('a')]));
        assert_eq!(Regex::rule_to_regex_rule(&vec![Symbol::Terminal('a')], 'a'), Some(vec![RegexSymbol::Epsilon]));
        assert_eq!(Regex::rule_to_regex_rule(&vec![Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('b')], 'a'), None);
    }

    #[test]
    fn regex_word_to_word_test() {
        assert_eq!(Regex::regex_word_to_word(&vec![RegexSymbol::Terminal('a'), RegexSymbol::Nonterminal('b')]), Some(vec![Symbol::Terminal('a'), Symbol::Nonterminal('b')]));
        assert_eq!(Regex::regex_word_to_word(&vec![RegexSymbol::Terminal('a'), RegexSymbol::Nonterminal('b'), RegexSymbol::Epsilon]), Some(vec![Symbol::Terminal('a'), Symbol::Nonterminal('b'), Symbol::Epsilon]));
        assert_eq!(Regex::regex_word_to_word(&vec![RegexSymbol::AtomicLanguage('a', 'b')]), None);
        assert_eq!(Regex::regex_word_to_word(&vec![RegexSymbol::Terminal('a'), RegexSymbol::AtomicLanguage('a', 'b')]), None);
        assert_eq!(Regex::regex_word_to_word(&vec![RegexSymbol::Nonterminal('a'), RegexSymbol::AtomicLanguage('a', 'b')]), None);
    }
}