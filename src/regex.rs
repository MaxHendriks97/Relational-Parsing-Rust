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
        let mut atomic_regex_rules: HashMap<(Nonterminal, Terminal), (HashSet<Vec<RegexSymbol>>, HashSet<Vec<RegexSymbol>>, HashSet<Vec<RegexSymbol>>)> = HashMap::new();
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

    fn calculate_regex_rules(nonterminal: &Nonterminal, terminal: &Terminal, rules: &HashMap<Nonterminal, HashSet<Word>>, rule_list: &HashSet<Vec<Symbol>>) -> (HashSet<Vec<RegexSymbol>>, HashSet<Vec<RegexSymbol>>, HashSet<Vec<RegexSymbol>>) {
        let mut direct: HashSet<Vec<RegexSymbol>> = HashSet::new();
        let mut recursive: HashSet<Vec<RegexSymbol>> = HashSet::new();
        let mut different_atomic: HashSet<Vec<RegexSymbol>> = HashSet::new();
        let mut nullable_atomic: bool = false;

        for rule in rule_list {
            //println!("Rule: {:?}", rule);
            if rule[0] == Symbol::Epsilon {
                nullable_atomic = true;
            }
            if let Some(regex_rule) = Regex::rule_to_regex_rule(rule, *terminal) {
                //println!("Regex rule: {:?}", regex_rule);
                match regex_rule[0] {
                    RegexSymbol::AtomicLanguage(nt, _) => {
                        if *nonterminal == nt {
                            recursive.insert(regex_rule);
                        } else {
                            different_atomic.insert(regex_rule);
                        }
                    },
                    _ => {
                        direct.insert(regex_rule);
                    },
                }
            }
        }
        let mut new_rules = Regex::null_rules(rules, &mut direct, nullable_atomic);
        new_rules.extend(Regex::null_rules(rules, &mut recursive, nullable_atomic));
        new_rules.extend(Regex::null_rules(rules, &mut different_atomic, nullable_atomic));

        for rule in new_rules {
            match rule[0] {
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
                        if rule.len() > 1 {
                            direct.insert(rule[1..].to_vec());
                        } else {
                            direct.insert(vec![RegexSymbol::Epsilon]);
                        }
                    }
                },
                _ => {},
            }
        }
        (direct, recursive, different_atomic)
    }

    fn build_regex_map(mut queue: VecDeque<(Nonterminal, Terminal)>, atomic_regex_rules: HashMap<(Nonterminal, Terminal), (HashSet<Vec<RegexSymbol>>, HashSet<Vec<RegexSymbol>>, HashSet<Vec<RegexSymbol>>)>) -> HashMap<(Nonterminal, Terminal), RegexNode> {
        let mut atomic_regex_rules_working_copy: HashMap<(Nonterminal, Terminal), (HashSet<Vec<RegexSymbol>>, HashSet<Vec<RegexSymbol>>, HashSet<Vec<RegexSymbol>>)> = atomic_regex_rules.clone();
        let mut res: HashMap<(Nonterminal, Terminal), RegexNode> = HashMap::new();

        while queue.len() > 0 {
            if let Some((nonterminal, terminal)) = queue.pop_front() {
                //println!("Current (nonterminal, terminal): {:?}", (nonterminal, terminal));
                if let Some((direct, recursive, different_atomic)) = atomic_regex_rules_working_copy.get_mut(&(nonterminal, terminal)) {
                    //println!("Current different: {:?}", different_atomic);
                    //let mut different_recursives: HashSet<Vec<RegexSymbol>> = HashSet::new();
                    for different_rule in different_atomic.clone() {
                        if let RegexSymbol::AtomicLanguage(nt, t) = different_rule[0] {
                            if let Some((ntdirect, ntrecursive, ntdifferent)) = atomic_regex_rules.get(&(nt, t)) {
                                for ntdirect_rule in ntdirect.clone() {
                                    direct.insert([&ntdirect_rule[..], &different_rule[1..]].concat());
                                }
                                for ntrecursive_rule in ntrecursive.clone() {
                                    recursive.insert([&[RegexSymbol::AtomicLanguage(nonterminal, t)], &ntrecursive_rule[1..], &different_rule[1..]].concat());
                                }
                                for ntdifferent_rule in ntdifferent.clone() {
                                    if let RegexSymbol::AtomicLanguage(nt2, _) = ntdifferent_rule[0] {
                                        if nt2 == nonterminal {
                                            recursive.insert([&ntdifferent_rule[..], &different_rule[1..]].concat());
                                        } else {
                                            different_atomic.insert([&ntdifferent_rule[..], &different_rule[1..]].concat());
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
                            let mut regex_nodes: Vec<RegexNode> = Vec::new();
                            for different_rule in different_atomic.iter() {
                                if let RegexSymbol::AtomicLanguage(nt, t) = different_rule[0] {
                                    if let Some(regex) = res.get(&(nt, t)) {
                                        regex_nodes.push(regex.clone());
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

    fn null_rules(rules: &HashMap<Nonterminal, HashSet<Word>>, rule_list: &HashSet<Vec<RegexSymbol>>, nullable_atomic: bool) -> HashSet<Vec<RegexSymbol>> {
        //println!("Rule_list: {:?}", rule_list);
        let mut new_rules: HashSet<Vec<RegexSymbol>> = HashSet::new();
        for rule in rule_list {
            //println!("Rule: {:?}", rule);
            let mut nullable_positions: Vec<usize> = Vec::new();
            for (pos, symbol) in rule.iter().enumerate() {
                //println!("Symbol: {:?}", symbol);
                match symbol {
                    RegexSymbol::Nonterminal(nt) => {
                        //println!("Nullable_nonterminal: {}", Regex::nullable_nonterminal(rules, &nt));
                        if Regex::nullable_nonterminal(rules, &nt) {
                            nullable_positions.push(pos);
                        }
                    },
                    RegexSymbol::AtomicLanguage(nt, _) => {
                        if nullable_atomic && Regex::nullable_nonterminal(rules, &nt) {
                            nullable_positions.push(pos);
                        }
                    }
                    _ => {},
                }
            }
            //println!("Nullable_positions: {:?}", nullable_positions);

            let nullable_combinations: Vec<Vec<usize>> = Regex::powerset(&nullable_positions);
            for comb in nullable_combinations {
                let mut rev = comb.clone();
                rev.sort();
                rev.reverse();
                let mut new_rule = rule.clone();
                for index in rev {
                    new_rule.remove(index);
                }
                //println!("New rule: {:?}", new_rule);
                if new_rule.len() > 0 {
                    new_rules.insert(new_rule);
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

    fn build_regex_node(direct: &HashSet<Vec<RegexSymbol>>, different_recursive: &Vec<RegexNode>, recursive: &HashSet<Vec<RegexSymbol>>) -> RegexNode {
        let mut res_nodes: Vec<RegexNode> = Vec::new();
        if direct.len() > 0 {
            let mut direct_word_set: BTreeSet<Word> = BTreeSet::new();
            for direct_rule in direct {
                if let Some(word) = Regex::regex_word_to_word(&direct_rule) {
                    direct_word_set.insert(word);
                }
            }
            res_nodes.push(RegexNode::Word(WordNode{words: direct_word_set, kleene_star: false}));
        }
        if different_recursive.len() > 0 {
            if different_recursive.len() == 1 {
                res_nodes.push(different_recursive.clone().pop().unwrap());
            } else {
                let mut different_recursive_node_vec: Vec<RegexNode> = Vec::new();
                for different_recursive_node in different_recursive {
                    different_recursive_node_vec.push(different_recursive_node.clone());
                }
                res_nodes.push(RegexNode::Node(NodeNode{nodes: different_recursive_node_vec}));
            }
        }
        if recursive.len() > 0 {
            let mut recursive_word_set: BTreeSet<Word> = BTreeSet::new();
            for recursive_rule in recursive {
                if let Some(word) = Regex::regex_word_to_word(&recursive_rule[1..].to_vec()) {
                    if word.len() > 0 {
                        recursive_word_set.insert(word);
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
    

}

#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub enum RegexNode {
    Word(WordNode),
    Node(NodeNode),
}

impl RegexNode {
    pub fn is_e_node(&self) -> bool {
        if let RegexNode::Word(wordnode) = self {
            wordnode.words == BTreeSet::from([vec![Symbol::Epsilon]])
        } else {
            false
        }
    }
}

impl fmt::Display for RegexNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RegexNode::Word(node) => write!(f, "{}", node),
            RegexNode::Node(node) => write!(f, "{}", node),
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

#[derive(Eq, PartialEq, Debug, Clone, Hash)]
pub struct WordNode {
    pub words: BTreeSet<Word>,
    pub kleene_star: bool,
}

impl fmt::Display for WordNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(")?;
        let mut words_iter = self.words.iter().peekable();
        while words_iter.peek().is_some() {
            for symbol in words_iter.next().unwrap() {
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