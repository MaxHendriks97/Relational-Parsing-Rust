//! # Regex
//! 
//! The `regex` module contains data structures and associated methods which calculate atomic languages
//! according to the derivation rules of a provided grammar and expresses these atomic languages
//! as regular expressions.

use std::fmt;
use std::collections::{HashSet, HashMap, BTreeSet, VecDeque};
use std::ptr::null;

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

type RegexWord = Vec<RegexSymbol>;
type RegexWordRule = (RegexWord, Rules);
type RegexWordRuleSet = HashSet<RegexWordRule>;

#[derive(PartialEq, Eq, Debug)]
struct IntermediateAtomic {
    direct: RegexWordRuleSet,
    recursive: RegexWordRuleSet,
    different_atomic: RegexWordRuleSet,
}

pub struct Regex(HashMap<(Nonterminal, Terminal), Node>);

enum Node {
    Seq {nodes: Vec<Node>, kleene: bool},
    Opt {nodes: HashSet<Node>, kleene: bool},
    Word {word: Word, kleene: bool},
}

impl Regex {
    pub fn new(terminals: &HashSet<Terminal>, rules: &HashMap<Nonterminal, HashSet<Word>>) -> Regex {
        let mut intermediate_atomics: HashMap<(Nonterminal, Terminal), IntermediateAtomic> = HashMap::new();
        let nullable_nonterminals: HashMap<Nonterminal, RulesSet> = Regex::find_all_nullables(rules);

        for (nonterminal, word_list) in rules {
            //build intermediate atomic
            for terminal in terminals {
                if let Some(intermediate_atomic) = IntermediateAtomic::new(*nonterminal, *terminal, word_list, &nullable_nonterminals) {
                    intermediate_atomics.insert((*nonterminal, *terminal), intermediate_atomic);
                }
            }
        }

        // also need to null at some point


        let mut waiting_atomics: VecDeque<((Nonterminal, Terminal), IntermediateAtomic)> = VecDeque::new();
        let mut finished_atomics: HashMap<(Nonterminal, Terminal), IntermediateAtomic> = HashMap::new();

        //first pass, then slowly resolve waiting atomics.

        unimplemented!()
    }

    pub fn print_with_rules(&self) {
        unimplemented!()
    }

    fn find_all_nullables(rules: &HashMap<Nonterminal, HashSet<Word>>) -> HashMap<Nonterminal, RulesSet> {
        // Will contain all nullable nonterminals and the ways to null them
        let mut nullable_nonterminals: HashMap<Nonterminal, RulesSet> = HashMap::new();

        // Will contain all potentially nullable nonterminals with all the rules that may null them
        // I.E. every rule that only consists of nonterminal symbols is potentially nullable
        let mut potentially_nullable_rules: HashMap<Nonterminal, HashSet<(Word, Vec<Nonterminal>)>> = HashMap::new();

        for (nonterminal, word_set) in rules {
            for word in word_set {
                // If the rule is an epsilon rule, add the nonterminal to the nullables with that rule
                if *word == vec![Symbol::Epsilon] {
                    nullable_nonterminals.entry(*nonterminal).or_default().insert(vec![(*nonterminal, word.clone())]);
                    continue;
                }

                // Else, if the rule is only nonterminals, add it to the potentially nullable rules
                let mut nonterminals: Vec<Nonterminal> = Vec::with_capacity(word.len());
                let mut potentially_nullable: bool = true;
                for symbol in word {
                    match symbol {
                        Symbol::Nonterminal(nt) => nonterminals.push(*nt),
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
                    let mut nulling_rules_set: RulesSet = HashSet::from([vec![(*nonterminal, word.clone())]]);

                    // Check every nonterminal in the rule
                    for nonterminal in chars {
                        if let Some(rules_set) = nullable_nonterminals.get(nonterminal) {
                            // If the current nonterminal is nullable, add all the ways to null it to the nulling rules set
                            let mut new_nulling_rules_set: RulesSet = HashSet::new();
                            for rules in rules_set {
                                for nulling_rules in &nulling_rules_set {
                                    new_nulling_rules_set.insert([nulling_rules.clone(), rules.clone()].concat());
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
                        for nulling_rules in nulling_rules_set {
                            // If the rule is nullable, we need to check whether we actually have a new way to null it
                            if let Some(entry) = nullable_nonterminals.get(nonterminal) {
                                if entry.contains(&nulling_rules) {
                                    continue;
                                }
                            }

                            // If the way to null the rule is new, add it to the set and make sure we cycle through all the potentially nullable rules again
                            nullable_nonterminals.entry(*nonterminal).or_default().insert(nulling_rules);
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
        unimplemented!()
    }
}

impl IntermediateAtomic {
    pub fn new(nonterminal: Nonterminal, terminal: Terminal, word_set: &HashSet<Word>, nullable_nonterminals: &HashMap<Nonterminal, RulesSet>) -> Option<IntermediateAtomic> {
        let mut direct: RegexWordRuleSet = HashSet::new();
        let mut recursive: RegexWordRuleSet = HashSet::new();
        let mut different_atomic: RegexWordRuleSet = HashSet::new();

        let nulled_word_set = IntermediateAtomic::word_set_with_nullings(word_set, nullable_nonterminals);

        for (word, regex_word) in nulled_word_set {
            if let Some((new_regex_word, nulled_rules)) = IntermediateAtomic::shift_regex_word(&regex_word, terminal) {
                match new_regex_word[0] {
                    RegexSymbol::AtomicLanguage(nt, _) => {
                        if nonterminal == nt {
                            recursive.insert((new_regex_word, [vec![(nonterminal, word.clone())], nulled_rules.clone()].concat()));
                        } else {
                            different_atomic.insert((new_regex_word, [vec![(nonterminal, word.clone())], nulled_rules.clone()].concat()));
                        }
                    },
                    _ => {
                        direct.insert((new_regex_word, [vec![(nonterminal, word.clone())], nulled_rules.clone()].concat()));
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

    // Build new word set with nullings applied to regexword, with original word for record-keeping
    fn word_set_with_nullings(word_set: &HashSet<Word>, nullable_nonterminals: &HashMap<Nonterminal, RulesSet>) -> HashSet<(Word, RegexWord)> {
        let mut nulled_word_set: HashSet<(Word, RegexWord)> = HashSet::new();

        for word in word_set {
            let mut nullable_positions: Vec<(usize, RulesSet)> = Vec::new();
            for (pos, symbol) in word.iter().enumerate() {
                match symbol {
                    &Symbol::Nonterminal(nt) => {
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
                    for rules in &rules_set {
                        let mut new_regex_word = regex_word.clone();
                        new_regex_word[pos] = RegexSymbol::Nulled(rules.clone());
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
        let mut res: RegexWord = Vec::with_capacity(word.len());
        for symbol in word {
            res.push(IntermediateAtomic::symbol_to_regex_symbol(symbol));
        }
        res
    }
    
    //performs shift by terminal and converts to regexword
    fn shift_regex_word(word: &RegexWord, terminal: Terminal) -> Option<(RegexWord, Rules)> {
        let mut res_word: RegexWord = Vec::with_capacity(word.len());
        let mut res_rules: Rules = Vec::new();
        let mut found_terminal: bool = false;

        for symbol in word {
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
                Some((vec![RegexSymbol::Epsilon], res_rules))
            } else {
                Some((res_word, res_rules))
            }
        } else {
            None
        }
    }

    //converts symbol to regex symbol
    fn symbol_to_regex_symbol(symbol: &Symbol) -> RegexSymbol {
        match symbol {
            Symbol::Nonterminal(nt) => RegexSymbol::Nonterminal(*nt),
            Symbol::Terminal(t) => RegexSymbol::Terminal(*t),
            Symbol::Epsilon => RegexSymbol::Epsilon,
        }
    }

}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn intermediate_atomic() {
        let basic_words: HashSet<Word> = HashSet::from([
            vec![Symbol::Terminal('a')],
            vec![Symbol::Nonterminal('S'), Symbol::Terminal('a')],
            vec![Symbol::Nonterminal('S'), Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')]
        ]);
        let nullable_nonterminals = Regex::find_all_nullables(&HashMap::from([('S', basic_words.clone())]));
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
            vec![Symbol::Epsilon],
            vec![Symbol::Terminal('a')],
            vec![Symbol::Nonterminal('S'), Symbol::Terminal('a')],
            vec![Symbol::Nonterminal('S'), Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')]
        ]);
        let nullable_nonterminals = Regex::find_all_nullables(&HashMap::from([('S', e_words.clone())]));
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
            vec![Symbol::Terminal('a')],
            vec![Symbol::Terminal('a'), Symbol::Nonterminal('S'), Symbol::Terminal('a')],
        ]);
        let nullable_nonterminals = Regex::find_all_nullables(&HashMap::from([('A', odd_a_words.clone())]));
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
    }

    #[test]
    fn find_all_nullables() {
        let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
        rules.insert('S', HashSet::from([
            vec![Symbol::Nonterminal('A'), Symbol::Nonterminal('B')],
            vec![Symbol::Nonterminal('B')],
            vec![Symbol::Terminal('z')],
        ]));
        rules.insert('A', HashSet::from([
            vec![Symbol::Epsilon],
            vec![Symbol::Terminal('a')],
        ]));
        rules.insert('B', HashSet::from([
            vec![Symbol::Epsilon],
            vec![Symbol::Nonterminal('A')],
        ]));
        rules.insert('C', HashSet::from([
            vec![Symbol::Terminal('c')],
            vec![Symbol::Nonterminal('D')],
            vec![Symbol::Nonterminal('B'), Symbol::Terminal('c')],
        ]));
        rules.insert('D', HashSet::from([
            vec![Symbol::Terminal('D')]
        ]));

        println!("{:?}", Regex::find_all_nullables(&rules));
    }

    #[test]
    fn word_set_with_nullings() {
        let mut word_set: HashSet<Word> = HashSet::new();
        word_set.insert(vec![Symbol::Terminal('a')]);
        word_set.insert(vec![Symbol::Nonterminal('S'), Symbol::Terminal('a')]);
        word_set.insert(vec![Symbol::Nonterminal('S'), Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')]);

        let nullable_nonterminals: HashMap<Nonterminal, RulesSet> = HashMap::from([('S', HashSet::from([vec![('S', vec![Symbol::Epsilon])]]))]);

        println!("{:?}", IntermediateAtomic::word_set_with_nullings(&word_set, &nullable_nonterminals));
    }

    #[test]
    fn shift_regex_word() {    
        // fn shift_regex_word(word: &RegexWord, terminal: Terminal) -> Option<(RegexWord, RulesSet)> {
        let regex_word: RegexWord = vec![RegexSymbol::Nulled(vec![('S', vec![Symbol::Nonterminal('B')]), ('B', vec![Symbol::Epsilon])]), RegexSymbol::Nulled(vec![('B', vec![Symbol::Epsilon])]), RegexSymbol::Terminal('a'), RegexSymbol::Terminal('a')];
        println!("{:?}", IntermediateAtomic::shift_regex_word(&regex_word, 'a'));

        let regex_word: RegexWord = vec![RegexSymbol::Nonterminal('S'), RegexSymbol::Nulled(vec![('B', vec![Symbol::Epsilon])]), RegexSymbol::Terminal('a'), RegexSymbol::Terminal('a')];
        println!("{:?}", IntermediateAtomic::shift_regex_word(&regex_word, 'a'));
    }
}