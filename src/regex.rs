//! # Regex
//! 
//! The `regex` module contains data structures and associated methods which calculate atomic languages
//! according to the derivation rules of a provided grammar and expresses these atomic languages
//! as regular expressions.

use std::fmt;
use std::collections::{HashSet, HashMap, BTreeSet};

use crate::{word::*, GrammarRules, RegexWordRuleSet, RegexWord, RegexWordRule, RegexSymbol};

#[derive(PartialEq, Eq, Debug, Clone)]
struct IntermediateAtomic {
    direct: RegexWordRuleSet,
    recursive: RegexWordRuleSet,
    different_atomic: RegexWordRuleSet,
}

#[derive(PartialEq, Eq, Debug, Clone)]
struct RecursiveAtomic{
    direct: RegexWordRuleSet,
    recursive: RegexWordRuleSet,
    different_atomic: BTreeSet<(Box<RecursiveAtomic>, RegexWord)>,
}

pub struct Regex(pub HashMap<(Nonterminal, Terminal), Node>);

#[derive(PartialEq, Eq, Debug, Clone, PartialOrd, Ord, Hash)]
pub enum Node {
    Seq {nodes: Vec<Node>, kleene: bool},
    Opt {nodes: BTreeSet<Node>, kleene: bool},
    Word {word: RegexWord, rules: Rules, kleene: bool},
}

impl Regex {
    pub fn new(terminals: &HashSet<Terminal>, rules: &GrammarRules) -> Regex {
        let mut intermediate_atomics: HashMap<(Nonterminal, Terminal), IntermediateAtomic> = HashMap::new();
        let nullable_nonterminals: HashMap<Nonterminal, RulesSet> = Regex::find_all_nullables(rules);

        for (nonterminal, word_list) in rules.iter() {
            //build intermediate atomic
            for terminal in terminals {
                if let Some(intermediate_atomic) = IntermediateAtomic::new(*nonterminal, *terminal, word_list, &nullable_nonterminals) {
                    intermediate_atomics.insert((*nonterminal, *terminal), intermediate_atomic);
                }
            }
        }

        let mut waiting_atomics: HashMap<(Nonterminal, Terminal), IntermediateAtomic> = HashMap::new();
        let mut finished_atomics: HashMap<(Nonterminal, Terminal), IntermediateAtomic> = HashMap::new();

        //first pass, then slowly resolve waiting atomics.
        for (key, intermediate_atomic) in &intermediate_atomics {
            if intermediate_atomic.direct.is_empty() && intermediate_atomic.different_atomic.is_empty() {
                continue;
            }
            if !intermediate_atomic.different_atomic.is_empty() {
                //TODO Implement waiting atomic into recursive atomic
                waiting_atomics.insert(key.clone(), intermediate_atomic.clone());
            } else {
                finished_atomics.insert(key.clone(), intermediate_atomic.clone());
            }
        }

        //TODO Resolve waiting atomic

        let mut res: Regex = Regex(HashMap::new());

        for (key, atomic) in finished_atomics {
            res.0.insert(key.clone(), IntermediateAtomic::atomic_to_node(atomic));
        }

        res
    }

    fn find_all_nullables(rules: &GrammarRules) -> HashMap<Nonterminal, RulesSet> {
        // Will contain all nullable nonterminals and the ways to null them
        let mut nullable_nonterminals: HashMap<Nonterminal, RulesSet> = HashMap::new();

        // Will contain all potentially nullable nonterminals with all the rules that may null them
        // I.E. every rule that only consists of nonterminal symbols is potentially nullable
        let mut potentially_nullable_rules: HashMap<Nonterminal, HashSet<(Word, Vec<Nonterminal>)>> = HashMap::new();

        for (nonterminal, word_set) in rules.iter() {
            for word in word_set {
                // If the rule is an epsilon rule, add the nonterminal to the nullables with that rule
                if *word == Word::from(vec![Symbol::Epsilon]) {
                    nullable_nonterminals.entry(*nonterminal).or_default().insert_rule(Rule::from(*nonterminal, word.clone()));
                    continue;
                }

                // Else, if the rule is only nonterminals, add it to the potentially nullable rules
                let mut nonterminals: Vec<Nonterminal> = Vec::with_capacity(word.len());
                let mut potentially_nullable: bool = true;
                for symbol in word.iter() {
                    match symbol {
                        Symbol::Nonterminal(nt) => nonterminals.push(nt),
                        _ => potentially_nullable = false,
                    }
                }
                if potentially_nullable {
                    potentially_nullable_rules.entry(*nonterminal).or_default().insert((word.clone(), nonterminals));
                }
            }
        }

        // While the nullable rules set is changing, we need to keep checking for new nullable nonterminals
        let mut changed: bool = true;
        while changed {
            changed = false;

            for (nonterminal, word_set) in &potentially_nullable_rules {
                for (word, chars) in word_set {
                    let mut nullable: bool = true;
                    // Initialise set of nulling rules with the rule that we are currently testing for nullability
                    let mut nulling_rules_set: RulesSet = RulesSet::from(
                        BTreeSet::from([Rules::from_single(*nonterminal, word.clone())])
                    );

                    // Check every nonterminal in the rule
                    for nonterminal in chars {
                        if let Some(rules_set) = nullable_nonterminals.get(nonterminal) {
                            // If the current nonterminal is nullable, add all the ways to null it to the nulling rules set
                            let mut new_nulling_rules_set: RulesSet = RulesSet::new();
                            for rules in rules_set.iter() {
                                for nulling_rules in nulling_rules_set.iter() {
                                    new_nulling_rules_set.insert_rules(nulling_rules.concat(&rules));
                                }
                            }

                            nulling_rules_set = new_nulling_rules_set;
                        } else {
                            // If the current nonterminal is not nullable, the rule is not nullable (yet) and we can stop checking the rest of the rule
                            nullable = false;
                            break;
                        }
                    }

                    if nullable {
                        for nulling_rules in nulling_rules_set.into_iter() {
                            // If the rule is nullable, we need to check whether we actually have a new way to null it
                            if let Some(entry) = nullable_nonterminals.get(nonterminal) {
                                if entry.contains(&nulling_rules) {
                                    continue;
                                }
                            }

                            // If the way to null the rule is new, add it to the set and make sure we cycle through all the potentially nullable rules again
                            nullable_nonterminals.entry(*nonterminal).or_default().insert_rules(nulling_rules);
                            changed = true;
                        }
                    }
                }
            }
        }

        nullable_nonterminals
    }
}

impl fmt::Display for Regex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for ((nonterminal, terminal), node) in &self.0 {
            write!(f, "[{}]({}): {}\n", nonterminal, terminal, node)?;
        }
        Ok(())
    }
}

impl fmt::Display for Node {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Node::Word { word, kleene, .. } => {
                write!(f, "({})", word)?;
                if *kleene {
                    write!(f, "*")?;
                }
                Ok(())
            },
            Node::Opt { nodes, kleene } => {
                write!(f, "(")?;
                let mut iter = nodes.iter().peekable();
                while let Some(node) = iter.next() {
                    write!(f, "{}", node)?;
                    if iter.peek().is_some() {
                        write!(f, " + ")?;
                    }
                }
                write!(f, ")")?;
                if *kleene {
                    write!(f, "*")?;
                }
                Ok(())
            },
            Node::Seq { nodes, kleene } => {
                write!(f, "(")?;
                let mut iter = nodes.iter().peekable();
                while let Some(node) = iter.next() {
                    write!(f, "{}", node)?;
                }
                write!(f, ")")?;
                if *kleene {
                    write!(f, "*")?;
                }
                Ok(())
            }
        }
    }
}

impl IntermediateAtomic {
    pub fn new(nonterminal: Nonterminal, terminal: Terminal, word_set: &HashSet<Word>, nullable_nonterminals: &HashMap<Nonterminal, RulesSet>) -> Option<IntermediateAtomic> {
        let mut direct: RegexWordRuleSet = RegexWordRuleSet::new();
        let mut recursive: RegexWordRuleSet = RegexWordRuleSet::new();
        let mut different_atomic: RegexWordRuleSet = RegexWordRuleSet::new();

        let nulled_word_set = IntermediateAtomic::word_set_with_nullings(word_set, nullable_nonterminals);

        for (word, regex_word) in nulled_word_set {
            if let Some((new_regex_word, nulled_rules)) = IntermediateAtomic::shift_regex_word(&regex_word, terminal) {
                match new_regex_word.first() {
                    RegexSymbol::AtomicLanguage(nt, _) => {
                        if nonterminal == *nt {
                            recursive.insert(RegexWordRule::new(new_regex_word, nulled_rules.concat_rule(&Rule::from(nonterminal, word.clone()))));
                        } else {
                            different_atomic.insert(RegexWordRule::new(new_regex_word, nulled_rules.concat_rule(&Rule::from(nonterminal, word.clone()))));
                        }
                    },
                    _ => {
                        direct.insert(RegexWordRule::new(new_regex_word, nulled_rules.concat_rule(&Rule::from(nonterminal, word.clone()))));
                    },
                }
            }
        }

        if !direct.is_empty() || !different_atomic.is_empty() {
            Some(IntermediateAtomic { direct, recursive, different_atomic })
        } else {
            None
        }
    }

    fn atomic_to_node(atomic: IntermediateAtomic) -> Node {
        if !atomic.different_atomic.is_empty() {
            panic!("Different_atomic should be empty: {:?}", atomic);
        }

        if let Some(direct_node) = IntermediateAtomic::regex_word_rule_set_to_node(&atomic.direct, false) {
            if let Some(recursive_node) = IntermediateAtomic::regex_word_rule_set_to_node(&atomic.recursive, true) {
                // We have a recursive node, return a sequence with the direct and recursive node, recursive having kleene set to true
                let new_recursive = match recursive_node {
                    Node::Opt {nodes, ..} => Node::Opt{ nodes, kleene: true },
                    Node::Word { word, rules, ..} => Node::Word { word, rules, kleene: true },
                    Node::Seq { .. } => panic!(), // recursive_node cannot be a sequence
                };
                Node::Seq { nodes: vec![direct_node, new_recursive], kleene: false }
            } else {
                // Return only direct node
                direct_node
            }
        } else {
            panic!("Could not make direct node with atomic: {:?}", atomic)
        }
    }

    fn regex_word_rule_set_to_node(set: &RegexWordRuleSet, rec: bool) -> Option<Node> {
        let mut nodes: BTreeSet<Node> = BTreeSet::new();

        for regex_word_rule in set.iter() {
            if rec {
                nodes.insert(Node::Word{word: regex_word_rule.regex_word().not_first(), rules: regex_word_rule.rules().clone(), kleene: false});
            } else {
                nodes.insert(Node::Word{word: regex_word_rule.regex_word().clone(), rules: regex_word_rule.rules().clone(), kleene: false});
            }
        }

        if nodes.len() == 0 {
            None
        } else if nodes.len() == 1 {
            // If set length is 1, we can return that node
            Some(nodes.into_iter().nth(0).unwrap())
        } else {
            // Otherwise we will need an opt node
            Some(Node::Opt{nodes, kleene: false})
        }
    }

    // Build new word set with nullings applied to regexword, with original word for record-keeping
    fn word_set_with_nullings(word_set: &HashSet<Word>, nullable_nonterminals: &HashMap<Nonterminal, RulesSet>) -> HashSet<(Word, RegexWord)> {
        let mut nulled_word_set: HashSet<(Word, RegexWord)> = HashSet::new();

        for word in word_set {
            let mut nullable_positions: Vec<(usize, RulesSet)> = Vec::new();
            for (pos, symbol) in word.iter().enumerate() {
                match symbol {
                    Symbol::Nonterminal(nt) => {
                        if let Some(nulling_rules_set) = nullable_nonterminals.get(&nt) {
                            nullable_positions.push((pos, nulling_rules_set.clone()));
                        }
                    },
                    _ => continue,
                }
            }

            let mut new_word_set = HashSet::from([(word.clone(), IntermediateAtomic::word_to_regex_word(word))]);
            for (pos, rules_set) in nullable_positions {
                let mut newer_word_set = new_word_set.clone();

                for (word, regex_word) in new_word_set {
                    for rules in rules_set.iter() {
                        let mut new_regex_word = regex_word.clone();
                        new_regex_word.set_position(pos, RegexSymbol::Nulled(rules.clone()));
                        newer_word_set.insert((word.clone(), new_regex_word));
                    }
                }

                new_word_set = newer_word_set;
            }

            nulled_word_set.extend(new_word_set);
        }

        nulled_word_set
    }

    fn word_to_regex_word(word: &Word) -> RegexWord {
        let mut res: RegexWord = RegexWord::new(Vec::with_capacity(word.len()));
        for symbol in word.iter() {
            res.push(IntermediateAtomic::symbol_to_regex_symbol(symbol));
        }
        res
    }
    
    //performs shift by terminal and converts to regexword
    fn shift_regex_word(word: &RegexWord, terminal: Terminal) -> Option<(RegexWord, Rules)> {
        let mut res_word: RegexWord = RegexWord::new(Vec::with_capacity(word.len()));
        let mut res_rules: Rules = Rules::new();
        let mut found_terminal: bool = false;

        for symbol in word.iter() {
            match symbol {
                RegexSymbol::Nulled(rules) => {
                    if !found_terminal {
                        res_rules.extend(rules.clone());
                    } else {
                        res_word.push(RegexSymbol::Nulled(rules.clone()));
                    }
                },
                RegexSymbol::Terminal(t) => {
                    if *t != terminal && !found_terminal {
                        break;
                    } else if *t == terminal && !found_terminal {
                        found_terminal = true;
                    } else {
                        res_word.push(RegexSymbol::Terminal(*t));
                    }
                },
                RegexSymbol::Nonterminal(nt) => {
                    if !found_terminal {
                        found_terminal = true;
                        res_word.push(RegexSymbol::AtomicLanguage(*nt, terminal));
                    } else {
                        res_word.push(RegexSymbol::Nonterminal(*nt));
                    }
                }
                symbol => {
                    if found_terminal {
                        res_word.push(symbol.clone());
                    } else {
                        break;
                    }
                },
            }
        }

        if found_terminal {
            if res_word.is_empty() {
                Some((RegexWord::new(vec![RegexSymbol::Epsilon]), res_rules))
            } else {
                Some((res_word, res_rules))
            }
        } else {
            None
        }
    }

    //converts symbol to regex symbol
    fn symbol_to_regex_symbol(symbol: Symbol) -> RegexSymbol {
        match symbol {
            Symbol::Nonterminal(nt) => RegexSymbol::Nonterminal(nt),
            Symbol::Terminal(t) => RegexSymbol::Terminal(t),
            Symbol::Epsilon => RegexSymbol::Epsilon,
        }
    }

}

impl Node {
    pub fn new_word(word: RegexWord, rules: Rules, kleene: bool) -> Node {
        Node::Word{word, rules, kleene}
    }

    pub fn new_seq(nodes: Vec<Node>, kleene: bool) -> Node {
        Node::Seq{nodes, kleene}
    }

    pub fn new_opt(nodes: BTreeSet<Node>, kleene: bool) -> Node {
        Node::Opt{nodes, kleene}
    }

    // Returns the rules by which a node is nulled.
    // Returns no nullings if the node contains a symbol
    pub fn get_nulling_rules(&self) -> Option<RulesSet> {
        let mut rules_set: RulesSet = RulesSet::new();
        match self {
            Node::Opt { nodes, .. } => {
                for node in nodes {
                    if let Some(rules) = node.get_nulling_rules() {
                        rules_set.extend(rules);
                    } else {
                        return None;
                    }
                }
            },
            Node::Seq { nodes, .. } => {
                for node in nodes {
                    if let Some(new_rules) = node.get_nulling_rules() {
                        rules_set = Node::stitch_nulling_sets(&rules_set, &new_rules);
                    } else {
                        return None;
                    }
                }
            },
            Node::Word { word, rules, .. } => {
                if let Some(r) = Node::word_get_nulling_rules(word) {
                    rules_set.insert_rules(r.concat(rules));
                } else {
                    return None
                }
            }
        }
        Some(rules_set)
    }

    fn word_get_nulling_rules(word: &RegexWord) -> Option<Rules> {
        let mut rules: Rules = Rules::new();
        for symbol in word.iter() {
            if let Some(r) = Node::symbol_get_nulling_rules(symbol) {
                rules = rules.concat(&r);
            } else {
                return None
            }
        }
        return Some(rules)
    }

    fn symbol_get_nulling_rules(symbol: &RegexSymbol) -> Option<Rules> {
        match symbol {
            RegexSymbol::AtomicLanguage(_, _) => None,
            RegexSymbol::Nonterminal(_) => None,
            RegexSymbol::Terminal(_) => None,
            RegexSymbol::Epsilon => Some(Rules::new()),
            RegexSymbol::Nulled(nulled_rules) => Some(nulled_rules.clone()),
        }
    }

    fn stitch_nulling_sets(set1: &RulesSet, set2: &RulesSet) -> RulesSet {
        if set1.is_empty() {
            set2.clone()
        } else if set2.is_empty() {
            set1.clone()
        } else {
            let mut res: RulesSet = RulesSet::new();
            for rules1 in set1.iter() {
                for rules2 in set2.iter() {
                    res.insert_rules(rules1.concat(&rules2));
                }
            }
            res
        }
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    fn standard_rules() -> GrammarRules {
        let mut grammar_rules: GrammarRules = GrammarRules::new();
        grammar_rules.extend_with_words('S', HashSet::from([
            Word::from(vec![Symbol::Terminal('a')]),
            Word::from(vec![Symbol::Nonterminal('S'), Symbol::Terminal('a')]),
            Word::from(vec![Symbol::Nonterminal('S'), Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')])
        ]));


        grammar_rules
    }

    fn epsilon_rules() -> GrammarRules {
        let mut grammar_rules: GrammarRules = GrammarRules::new();
        grammar_rules.extend_with_words('S', HashSet::from([
            Word::from(vec![Symbol::Terminal('a')]),
            Word::from(vec![Symbol::Nonterminal('S'), Symbol::Terminal('a')]),
            Word::from(vec![Symbol::Nonterminal('S'), Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')]),
            Word::from(vec![Symbol::Epsilon])
        ]));

        grammar_rules
    }

    #[test]
    fn regex() {
        let terminals: HashSet<Terminal> = HashSet::from(['a', 'b', 'c']);

        println!("{}", &Regex::new(&terminals, &standard_rules()));
        println!("{}", &Regex::new(&terminals, &epsilon_rules()));
    }


    #[test]
    fn intermediate_atomic() {
        let basic_words: HashSet<Word> = HashSet::from([
            Word::from(vec![Symbol::Terminal('a')]),
            Word::from(vec![Symbol::Nonterminal('S'), Symbol::Terminal('a')]),
            Word::from(vec![Symbol::Nonterminal('S'), Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')])
        ]);
        let mut grammar_rules: GrammarRules = GrammarRules::new();
        grammar_rules.extend_with_words('S', basic_words.clone());
        let nullable_nonterminals = Regex::find_all_nullables(&grammar_rules);
        if let Some(intermediate_atomic) = IntermediateAtomic::new('S', 'a', &basic_words, &nullable_nonterminals) {
            println!("{:?}", intermediate_atomic);
            // assert_eq!(intermediate_atomic, IntermediateAtomic{
            //     direct: HashSet::from([
            //         (vec![RegexSymbol::Epsilon], vec![('S', vec![Symbol::Terminal('a')])])
            //     ]),
            //     recursive: HashSet::from([
            //         (vec![RegexSymbol::AtomicLanguage('S', 'a'), RegexSymbol::Terminal('a')],
            //             vec![('S', vec![Symbol::Nonterminal('S'), Symbol::Terminal('a')])]),
            //         (vec![RegexSymbol::AtomicLanguage('S', 'a'), RegexSymbol::Terminal('b'), RegexSymbol::Nonterminal('S'), RegexSymbol::Terminal('c')],
            //             vec![('S', vec![Symbol::Nonterminal('S'), Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')])]),
            //     ]),
            //     different_atomic: HashSet::new(),
            // });
        }

        let e_words: HashSet<Word> = HashSet::from([
            Word::from(vec![Symbol::Epsilon]),
            Word::from(vec![Symbol::Terminal('a')]),
            Word::from(vec![Symbol::Nonterminal('S'), Symbol::Terminal('a')]),
            Word::from(vec![Symbol::Nonterminal('S'), Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')])
        ]);
        let mut grammar_rules: GrammarRules = GrammarRules::new();
        grammar_rules.extend_with_words('S', e_words.clone());
        let nullable_nonterminals = Regex::find_all_nullables(&grammar_rules);
        if let Some(intermediate_atomic) = IntermediateAtomic::new('S', 'a', &e_words, &nullable_nonterminals) {
            println!("{:?}", intermediate_atomic);
            // assert_eq!(intermediate_atomic, IntermediateAtomic{
            //     direct: HashSet::from([
            //         (vec![RegexSymbol::Epsilon], vec![('S', vec![Symbol::Terminal('a')])])
            //     ]),
            //     recursive: HashSet::from([
            //         (vec![RegexSymbol::AtomicLanguage('S', 'a'), RegexSymbol::Terminal('a')],
            //             vec![('S', vec![Symbol::Nonterminal('S'), Symbol::Terminal('a')])]),
            //         (vec![RegexSymbol::AtomicLanguage('S', 'a'), RegexSymbol::Terminal('b'), RegexSymbol::Nonterminal('S'), RegexSymbol::Terminal('c')],
            //             vec![('S', vec![Symbol::Nonterminal('S'), Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')])]),
            //     ]),
            //     different_atomic: HashSet::new(),
            // });
        }

        let odd_a_words: HashSet<Word> = HashSet::from([
            Word::from(vec![Symbol::Terminal('a')]),
            Word::from(vec![Symbol::Terminal('a'), Symbol::Nonterminal('S'), Symbol::Terminal('a')]),
        ]);
        let mut grammar_rules: GrammarRules = GrammarRules::new();
        grammar_rules.extend_with_words('S', odd_a_words.clone());
        let nullable_nonterminals = Regex::find_all_nullables(&grammar_rules);
        if let Some(intermediate_atomic) = IntermediateAtomic::new('S', 'a', &odd_a_words, &nullable_nonterminals) {
            println!("{:?}", intermediate_atomic);
            // assert_eq!(intermediate_atomic, IntermediateAtomic{
            //     direct: HashSet::from([
            //         (vec![RegexSymbol::Epsilon], vec![('S', vec![Symbol::Terminal('a')])]),
            //         (vec![RegexSymbol::Nonterminal('S'), RegexSymbol::Terminal('a')], vec![('S', vec![Symbol::Terminal('a'), Symbol::Nonterminal('S'), Symbol::Terminal('a')])]),
            //     ]),
            //     recursive: HashSet::new(),
            //     different_atomic: HashSet::new(),
            // });
        }
        
        let mut grammar_rules: GrammarRules = GrammarRules::new();
        grammar_rules.extend_with_words('S', HashSet::from([
            Word::from(vec![Symbol::Nonterminal('A'), Symbol::Nonterminal('B')]),
            Word::from(vec![Symbol::Nonterminal('B')]),
            Word::from(vec![Symbol::Terminal('z')]),
        ]));
        grammar_rules.extend_with_words('A', HashSet::from([
            Word::from(vec![Symbol::Epsilon]),
            Word::from(vec![Symbol::Terminal('a')]),
        ]));
        grammar_rules.extend_with_words('B', HashSet::from([
            Word::from(vec![Symbol::Epsilon]),
            Word::from(vec![Symbol::Nonterminal('A')]),
        ]));
        grammar_rules.extend_with_words('C', HashSet::from([
            Word::from(vec![Symbol::Terminal('c')]),
            Word::from(vec![Symbol::Nonterminal('D')]),
            Word::from(vec![Symbol::Nonterminal('B'), Symbol::Terminal('c')]),
        ]));
        grammar_rules.extend_with_words('D', HashSet::from([
            Word::from(vec![Symbol::Terminal('D')])
        ]));
        let nullable_nonterminals = Regex::find_all_nullables(&grammar_rules);
        if let Some(intermediate_atomic) = IntermediateAtomic::new('S', 'a', grammar_rules.get_words(&'S').unwrap(), &nullable_nonterminals) {
            println!("{:?}", intermediate_atomic);
        }
    }

    #[test]
    fn find_all_nullables() {
        let mut grammar_rules: GrammarRules = GrammarRules::new();
        grammar_rules.extend_with_words('S', HashSet::from([
            Word::from(vec![Symbol::Nonterminal('A'), Symbol::Nonterminal('B')]),
            Word::from(vec![Symbol::Nonterminal('B')]),
            Word::from(vec![Symbol::Terminal('z')]),
        ]));
        grammar_rules.extend_with_words('A', HashSet::from([
            Word::from(vec![Symbol::Epsilon]),
            Word::from(vec![Symbol::Terminal('a')]),
        ]));
        grammar_rules.extend_with_words('B', HashSet::from([
            Word::from(vec![Symbol::Epsilon]),
            Word::from(vec![Symbol::Nonterminal('A')]),
        ]));
        grammar_rules.extend_with_words('C', HashSet::from([
            Word::from(vec![Symbol::Terminal('c')]),
            Word::from(vec![Symbol::Nonterminal('D')]),
            Word::from(vec![Symbol::Nonterminal('B'), Symbol::Terminal('c')]),
        ]));
        grammar_rules.extend_with_words('D', HashSet::from([
            Word::from(vec![Symbol::Terminal('d')]),
        ]));

        println!("{:?}", Regex::find_all_nullables(&grammar_rules));
    }

    #[test]
    fn word_set_with_nullings() {
        let mut word_set: HashSet<Word> = HashSet::new();
        word_set.insert(Word::from(vec![Symbol::Terminal('a')]));
        word_set.insert(Word::from(vec![Symbol::Nonterminal('S'), Symbol::Terminal('a')]));
        word_set.insert(Word::from(vec![Symbol::Nonterminal('S'), Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')]));

        let nullable_nonterminals: HashMap<Nonterminal, RulesSet> = HashMap::from([('S', RulesSet::from(BTreeSet::from([Rules::from_single('S', Word::from(vec![Symbol::Epsilon]))])))]);

        println!("{:?}", IntermediateAtomic::word_set_with_nullings(&word_set, &nullable_nonterminals));
    }

    #[test]
    fn shift_regex_word() {
        // fn shift_regex_word(word: &RegexWord, terminal: Terminal) -> Option<(RegexWord, RulesSet)> {
        let regex_word: RegexWord = RegexWord::new(vec![
            RegexSymbol::Nulled(
                Rules::from(vec![
                    Rule::from('S', Word::from(vec![Symbol::Nonterminal('B')])),
                    Rule::from('B', Word::from(vec![Symbol::Epsilon]))
                ])
            ),
            RegexSymbol::Nulled(
                Rules::from(vec![
                    Rule::from('B', Word::from(vec![Symbol::Epsilon]))
                ])
            ),
            RegexSymbol::Terminal('a'),
            RegexSymbol::Terminal('a')]);
        println!("{:?}", IntermediateAtomic::shift_regex_word(&regex_word, 'a'));

        let regex_word: RegexWord = RegexWord::new(vec![
            RegexSymbol::Nonterminal('S'),
            RegexSymbol::Nulled(
                Rules::from(vec![
                    Rule::from('B', Word::from(vec![Symbol::Epsilon]))
                ])
            ),
            RegexSymbol::Terminal('a'),
            RegexSymbol::Terminal('a')]);
        println!("{:?}", IntermediateAtomic::shift_regex_word(&regex_word, 'a'));
    }
}
