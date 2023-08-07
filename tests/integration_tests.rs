// use relational_parsing::{self, Grammar, parse_count_memo};
// use crate::relational_parsing::{Regex, 
//     Memoize,
//     Symbol, 
//     Terminal,
//     Nonterminal};

use relational_parsing::{
    self, Regex
};

mod common;

#[test]
fn broken() {
    let grammar = common::another_broken_grammar();
    let reg = Regex::new(&grammar.terminals, &grammar.rules);
    println!("{}", reg);
}

#[test]
fn even_a_middle_b_test() {
    let grammar = common::even_a_middle_b_grammar();
//     let mut memoize: Memoize = Memoize::new();

//     assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'a', 'b', 'a', 'a', 'a'], &grammar, &mut memoize));

    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a', 'b', 'a', 'a', 'a'], &grammar));
}

// Indirect left recursion is not supported yet
// #[test]
// fn indirect_left_recursive_grammar_recog_test() {
//     let grammar = common::indirect_left_recursive_grammar();
//     let mut memoize: Memoize = Memoize::new();

//     assert!(relational_parsing::g_accepts_string(&vec!['a'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['b', 'a'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['b', 'a', 'b', 'a'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'b', 'a'], &grammar, &mut memoize));

//     assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a'], &grammar));
//     assert!(relational_parsing::g_accepts_string_no_memo(&vec!['b', 'a'], &grammar));
//     assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'b', 'a'], &grammar));
//     assert!(relational_parsing::g_accepts_string_no_memo(&vec!['b', 'a', 'b', 'a'], &grammar));
//     assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'b', 'a', 'b', 'a'], &grammar));
// }

// Indirect left recursion is not supported yet
// #[test]
// fn even_more_indirect_left_recursive_grammar_recog_test() {
//     let grammar = common::even_more_indirect_left_recursive_grammar();
//     let mut memoize: Memoize = Memoize::new();

//     assert!(relational_parsing::g_accepts_string(&vec!['a'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'a', 'a'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['b', 'a'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['b', 'a', 'a', 'a', 'a'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['b', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['c', 'a', 'a'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['c', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['c', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize));

//     assert!(!relational_parsing::g_accepts_string(&vec!['a', 'a'], &grammar, &mut memoize));
//     assert!(!relational_parsing::g_accepts_string(&vec!['a', 'a', 'a'], &grammar, &mut memoize));
//     assert!(!relational_parsing::g_accepts_string(&vec!['b'], &grammar, &mut memoize));
//     assert!(!relational_parsing::g_accepts_string(&vec!['b', 'a', 'a'], &grammar, &mut memoize));
//     assert!(!relational_parsing::g_accepts_string(&vec!['b', 'a', 'a', 'a'], &grammar, &mut memoize));
//     assert!(!relational_parsing::g_accepts_string(&vec!['c'], &grammar, &mut memoize));
//     assert!(!relational_parsing::g_accepts_string(&vec!['c', 'a'], &grammar, &mut memoize));
//     assert!(!relational_parsing::g_accepts_string(&vec!['c', 'a', 'a', 'a'], &grammar, &mut memoize));
//     assert!(!relational_parsing::g_accepts_string(&vec!['c', 'a', 'a', 'a', 'a'], &grammar, &mut memoize));

//     assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a'], &grammar));
//     assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a', 'a'], &grammar));
//     assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar));
//     assert!(relational_parsing::g_accepts_string_no_memo(&vec!['b', 'a'], &grammar));
//     assert!(relational_parsing::g_accepts_string_no_memo(&vec!['b', 'a', 'a', 'a', 'a'], &grammar));
//     assert!(relational_parsing::g_accepts_string_no_memo(&vec!['b', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar));
//     assert!(relational_parsing::g_accepts_string_no_memo(&vec!['c', 'a', 'a'], &grammar));
//     assert!(relational_parsing::g_accepts_string_no_memo(&vec!['c', 'a', 'a', 'a', 'a', 'a'], &grammar));
//     assert!(relational_parsing::g_accepts_string_no_memo(&vec!['c', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar));

//     assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a'], &grammar));
//     assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a'], &grammar));
//     assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['b'], &grammar));
//     assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['b', 'a', 'a'], &grammar));
//     assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['b', 'a', 'a', 'a'], &grammar));
//     assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['c'], &grammar));
//     assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['c', 'a'], &grammar));
//     assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['c', 'a', 'a', 'a'], &grammar));
//     assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['c', 'a', 'a', 'a', 'a'], &grammar));
// }

// fn build_rule(rule: (Nonterminal, &str)) -> (Nonterminal, Vec<Symbol>) {
//     let mut res_rule: Vec<Symbol> = Vec::with_capacity(rule.1.len());

//     for char in rule.1.chars() {
//         if char == 'e' {
//             res_rule.push(Symbol::Epsilon);
//         } else if char.is_uppercase() {
//             res_rule.push(Symbol::Nonterminal(char));
//         } else {
//             res_rule.push(Symbol::Terminal(char));
//         }
//     }

//     (rule.0, res_rule)
// }

// fn build_rules(rules: &Vec<(Nonterminal, &str)>) -> Vec<(Nonterminal, Vec<Symbol>)> {
//     let mut res_rules: Vec<(Nonterminal, Vec<Symbol>)> = Vec::with_capacity(rules.len());

//     for rule in rules {
//         res_rules.push(build_rule(*rule))
//     }

//     res_rules
// }

// #[test]
// fn even_a_middle_b_parse_test() {
//     let grammar = common::even_a_middle_b_grammar();
//     let mut memoize: Memoize = Memoize::new();

//     assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a', 'b', 'a', 'a', 'a'], &grammar, &mut memoize),
//         Ok(HashSet::from([
//             build_rules(&vec![('S', "aSa"), ('S', "aSa"), ('S', "aSa"), ('S', "b")]),
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a', 'b', 'a', 'a', 'a'], &grammar),
//         Ok(HashSet::from([
//             build_rules(&vec![('S', "aSa"), ('S', "aSa"), ('S', "aSa"), ('S', "b")]),
//         ]))
//     );
// }

// #[test]
// fn indirect_left_recursive_grammar_parse_test() {
//     let grammar = common::indirect_left_recursive_grammar();
//     let mut memoize: Memoize = Memoize::new();

//     assert_eq!(relational_parsing::find_parses(&vec!['a', 'b', 'a', 'b', 'a'], &grammar, &mut memoize),
//         Ok(HashSet::from([
//             build_rules(&vec![('A', "Ba"), ('B', "Ab"), ('A', "Ba"), ('B', "Ab"), ('A', "a")]),
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses(&vec!['a'], &grammar, &mut memoize),
//         Ok(HashSet::from([
//             build_rules(&vec![('A', "a")]),
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses(&vec!['b', 'a'], &grammar, &mut memoize),
//         Ok(HashSet::from([
//             build_rules(&vec![('A', "Ba"), ('B', "b")]),
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses(&vec!['a', 'b', 'a'], &grammar, &mut memoize),
//         Ok(HashSet::from([
//             build_rules(&vec![('A', "Ba"), ('B', "Ab"), ('A', "a")]),
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses(&vec!['b', 'a', 'b', 'a'], &grammar, &mut memoize),
//         Ok(HashSet::from([
//             build_rules(&vec![('A', "Ba"), ('B', "Ab"), ('A', "Ba"), ('B', "b")]),
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'b', 'a', 'b', 'a'], &grammar),
//         Ok(HashSet::from([
//             build_rules(&vec![('A', "Ba"), ('B', "Ab"), ('A', "Ba"), ('B', "Ab"), ('A', "a")]),
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a'], &grammar),
//         Ok(HashSet::from([
//             build_rules(&vec![('A', "a")]),
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses_no_memo(&vec!['b', 'a'], &grammar),
//         Ok(HashSet::from([
//             build_rules(&vec![('A', "Ba"), ('B', "b")]),
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'b', 'a'], &grammar),
//         Ok(HashSet::from([
//             build_rules(&vec![('A', "Ba"), ('B', "Ab"), ('A', "a")]),
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses_no_memo(&vec!['b', 'a', 'b', 'a'], &grammar),
//         Ok(HashSet::from([
//             build_rules(&vec![('A', "Ba"), ('B', "Ab"), ('A', "Ba"), ('B', "b")]),
//         ]))
//     );
// }

// #[test]
// fn even_more_indirect_left_recursive_grammar_parse_test() {
//     let grammar = common::even_more_indirect_left_recursive_grammar();
//     let mut memoize: Memoize = Memoize::new();

//     assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize),
//         Ok(HashSet::from([
//             build_rules(&vec![('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "a")]),
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses(&vec!['a'], &grammar, &mut memoize),
//         Ok(HashSet::from([
//             build_rules(&vec![('A', "a")]),
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a'], &grammar, &mut memoize),
//         Ok(HashSet::from([
//             build_rules(&vec![('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "a")]),
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses(&vec!['b', 'a'], &grammar, &mut memoize),
//         Ok(HashSet::from([
//             build_rules(&vec![('A', "Ba"), ('B', "b")]),
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses(&vec!['b', 'a', 'a', 'a', 'a'], &grammar, &mut memoize),
//         Ok(HashSet::from([
//             build_rules(&vec![('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "Ba"), ('B', "b")]),
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses(&vec!['b', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize),
//         Ok(HashSet::from([
//             build_rules(&vec![('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "Ba"), ('B', "b")]),
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses(&vec!['c', 'a', 'a'], &grammar, &mut memoize),
//         Ok(HashSet::from([
//             build_rules(&vec![('A', "Ba"), ('B', "Ca"), ('C', "c")]),
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses(&vec!['c', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize),
//         Ok(HashSet::from([
//             build_rules(&vec![('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "Ba"), ('B', "Ca"), ('C', "c")]),
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses(&vec!['c', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize),
//         Ok(HashSet::from([
//             build_rules(&vec![('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "Ba"), ('B', "Ca"), ('C', "c")]),
//         ]))
//     );

//     assert!(relational_parsing::find_parses(&vec!['a', 'a'], &grammar, &mut memoize).is_err());
//     assert!(relational_parsing::find_parses(&vec!['a', 'a', 'a'], &grammar, &mut memoize).is_err());
//     assert!(relational_parsing::find_parses(&vec!['b'], &grammar, &mut memoize).is_err());
//     assert!(relational_parsing::find_parses(&vec!['b', 'a', 'a'], &grammar, &mut memoize).is_err());
//     assert!(relational_parsing::find_parses(&vec!['b', 'a', 'a', 'a'], &grammar, &mut memoize).is_err());
//     assert!(relational_parsing::find_parses(&vec!['c'], &grammar, &mut memoize).is_err());
//     assert!(relational_parsing::find_parses(&vec!['c', 'a'], &grammar, &mut memoize).is_err());
//     assert!(relational_parsing::find_parses(&vec!['c', 'a', 'a', 'a'], &grammar, &mut memoize).is_err());
//     assert!(relational_parsing::find_parses(&vec!['c', 'a', 'a', 'a', 'a'], &grammar, &mut memoize).is_err());

//     assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar),
//         Ok(HashSet::from([
//             build_rules(&vec![('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "a")]),
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a'], &grammar),
//         Ok(HashSet::from([
//             build_rules(&vec![('A', "a")]),
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a', 'a'], &grammar),
//         Ok(HashSet::from([
//             build_rules(&vec![('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "a")]),
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses_no_memo(&vec!['b', 'a'], &grammar),
//         Ok(HashSet::from([
//             build_rules(&vec![('A', "Ba"), ('B', "b")]),
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses_no_memo(&vec!['b', 'a', 'a', 'a', 'a'], &grammar),
//         Ok(HashSet::from([
//             build_rules(&vec![('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "Ba"), ('B', "b")]),
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses_no_memo(&vec!['b', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar),
//         Ok(HashSet::from([
//             build_rules(&vec![('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "Ba"), ('B', "b")]),
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses_no_memo(&vec!['c', 'a', 'a'], &grammar),
//         Ok(HashSet::from([
//             build_rules(&vec![('A', "Ba"), ('B', "Ca"), ('C', "c")]),
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses_no_memo(&vec!['c', 'a', 'a', 'a', 'a', 'a'], &grammar),
//         Ok(HashSet::from([
//             build_rules(&vec![('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "Ba"), ('B', "Ca"), ('C', "c")]),
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses_no_memo(&vec!['c', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar),
//         Ok(HashSet::from([
//             build_rules(&vec![('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "Ba"), ('B', "Ca"), ('C', "c")]),
//         ]))
//     );

//     assert!(relational_parsing::find_parses_no_memo(&vec!['a', 'a'], &grammar).is_err());
//     assert!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a'], &grammar).is_err());
//     assert!(relational_parsing::find_parses_no_memo(&vec!['b'], &grammar).is_err());
//     assert!(relational_parsing::find_parses_no_memo(&vec!['b', 'a', 'a'], &grammar).is_err());
//     assert!(relational_parsing::find_parses_no_memo(&vec!['b', 'a', 'a', 'a'], &grammar).is_err());
//     assert!(relational_parsing::find_parses_no_memo(&vec!['c'], &grammar).is_err());
//     assert!(relational_parsing::find_parses_no_memo(&vec!['c', 'a'], &grammar).is_err());
//     assert!(relational_parsing::find_parses_no_memo(&vec!['c', 'a', 'a', 'a'], &grammar).is_err());
//     assert!(relational_parsing::find_parses_no_memo(&vec!['c', 'a', 'a', 'a', 'a'], &grammar).is_err());
// }

// //#[test]
// //fn strange_recursive_grammar_parse_test() {
//     //let grammar = common::strange_recursive_grammar();
//     //let mut memoize: Memoize = Memoize::new();
//     //grammar.finite_state_automaton.to_dot("highly_ambiguous").expect("error");

//     //relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize);
//     //relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a'], &grammar, &mut memoize);
//     //relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize);
//     //relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize);
//     //relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize);

//     //assert_eq!(relational_parsing::find_parses(&vec!['a'], &grammar, &mut memoize),
//         //Ok(HashSet::from([
//             //build_rules(&vec![('A', "a")]),
//         //]))
//     //);

//     //assert_eq!(relational_parsing::find_parses(&vec!['a', 'a'], &grammar, &mut memoize),
//         //Ok(HashSet::from([
//             //build_rules(&vec![('A', "aA"), ('A', "a")]),
//         //]))
//     //);

//     //assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a'], &grammar, &mut memoize),
//         //Ok(HashSet::from([
//             //build_rules(&vec![('A', "aA"), ('A', "aA"), ('A', "a")]),
//         //]))
//     //);
// //}

// #[test]
// fn mem_count() {
//     let grammars: Vec<(Grammar, &str)> = vec![
//         (common::basic_relational_parsing_example_grammar(), "basic.streams"),
//         (common::e_rule_relational_parsing_example_grammar(), "e_rule.streams"),
//         (common::odd_number_of_a_grammar(), "odd_nr_a.streams"),
//         (common::direct_left_recursive_grammar(), "direct_left.streams"),
//         (common::indirect_left_recursive_grammar(), "indirect_left.streams"),
//         (common::direct_right_recursive_grammar(), "direct_right.streams"),
//         (common::indirect_right_recursive_grammar(), "indirect_right.streams")
//     ];

//     for (grammar, stream) in grammars {
//         println!("{}", stream);
//         if let Ok(streams_file) = File::open(stream) {
//             if let Ok(mut memcount_file) = File::create(format!("{}_memcount.txt", stream)) {
//                 let reader = BufReader::new(streams_file);

//                 for opt_line in reader.lines() {
//                     if let Ok(line) = opt_line {
//                         let tokens: Vec<char> = line.chars().collect();

//                         let (res, memcount) = parse_count_memo(&tokens, &grammar, &mut Memoize::new());
//                         match res {
//                             Ok(_) => {
//                                 memcount_file.write_all(format!("{}, 1, {}\n", tokens.len(), memcount).as_bytes()).expect("could not write");
//                             },
//                             Err(_) => {
//                                 memcount_file.write_all(format!("{}, 0, {}\n", tokens.len(), memcount).as_bytes()).expect("could not write");
//                             }
//                         }
//                     }
//                 }
//             }
//         }
//     }
// }
