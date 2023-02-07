use std::collections::{HashMap, HashSet, VecDeque};

use relational_parsing::{self, Grammar};
use crate::relational_parsing::{Regex, 
    Memoize,
    Symbol, 
    Terminal,
    Nonterminal};

mod common;

#[test]
fn rules_to_regex_rules_test() {
    let grammar = common::basic_relational_parsing_example_grammar();
    let reg = Regex::new(&grammar.terminals, &grammar.rules);
    println!("{}", reg);
    reg.print_with_rules();

    let grammar = common::e_rule_relational_parsing_example_grammar();
    let reg = Regex::new(&grammar.terminals, &grammar.rules);
    println!("{}", reg);
    reg.print_with_rules();

    let grammar = common::extra_e_rule_relational_parsing_example_grammar();
    let reg = Regex::new(&grammar.terminals, &grammar.rules);
    println!("{}", reg);
    reg.print_with_rules();
    //for (_, regexnode) in reg.regex {
    //    for wordnode in regexnode.nodes {
    //        println!("{}", wordnode);
    //        println!("{:?}", wordnode.get_by_base_rules());
    //        println!()
    //    }
    //}

    let grammar = common::three_rule_grammar();
    let reg = Regex::new(&grammar.terminals, &grammar.rules);
    println!("{}", reg);
    reg.print_with_rules();

    let grammar = common::difficult_bottom_up_grammar();
    let reg = Regex::new(&grammar.terminals, &grammar.rules);
    println!("{}", reg);
    reg.print_with_rules();

    let grammar = common::odd_number_of_a_grammar();
    let reg = Regex::new(&grammar.terminals, &grammar.rules);
    println!("{}", reg);
    reg.print_with_rules();

    let grammar = common::direct_left_recursive_grammar();
    let reg = Regex::new(&grammar.terminals, &grammar.rules);
    println!("{}", reg);
    reg.print_with_rules();

    let grammar = common::indirect_left_recursive_grammar();
    let reg = Regex::new(&grammar.terminals, &grammar.rules);
    println!("{}", reg);
    reg.print_with_rules();

    let grammar = common::even_more_indirect_left_recursive_grammar();
    let reg = Regex::new(&grammar.terminals, &grammar.rules);
    println!("{}", reg);
    reg.print_with_rules();

    let grammar = common::direct_right_recursive_grammar();
    let reg = Regex::new(&grammar.terminals, &grammar.rules);
    println!("{}", reg);
    reg.print_with_rules();

    let grammar = common::indirect_right_recursive_grammar();
    let reg = Regex::new(&grammar.terminals, &grammar.rules);
    println!("{}", reg);
    reg.print_with_rules();

    let grammar = common::strange_recursive_grammar();
    let reg = Regex::new(&grammar.terminals, &grammar.rules);
    println!("{}", reg);
    reg.print_with_rules();
}

//#[test]
//fn regex_node_to_states_test() {
//    let grammar = common::e_rule_relational_parsing_example_grammar();
//    let regex = Regex::new(&grammar.terminals, &grammar.rules);
//    if let RegexNode::Node(regex_node) = regex.regex.get(&('S', 'b')).unwrap() {
//        println!("{}", regex_node.nodes.get(1).unwrap());
//        let mut regex_to_state: HashMap<VecDeque<RegexNode>, (State, State)> = HashMap::new();
//        println!("{:?}", FiniteStateAutomaton::atomic_regex_to_states(regex_node.nodes.get(1).unwrap(), None, 0, &mut regex_to_state));
//        //if let RegexNode::Word(word_node) = regex_node.nodes.get(1).unwrap() {
//        //    println!("{:?}", Grammar::word_node_to_states(word_node, 1, 0, 1))
//        //}
//    }
//
//    //println!("{:?}", Grammar::regex_node_to_states(regex.regex.get(&('S', 'b')).unwrap(), 1, 0, 1));
//
//    //let grammar = odd_number_of_a_grammar();
//    //let regex = Regex::new(&grammar.terminals, &grammar.rules);
//    //println!("{}", regex);
//    //println!("{:?}", Grammar::regex_node_to_states(regex.regex.get(&('S', 'a')).unwrap(), 1));
//
//}
//
//#[test]
//fn build_fsa_test() {
//    let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
//    rules.insert('S', HashSet::from([
//        &vec![Symbol::Terminal('a')],
//        &vec![Symbol::Nonterminal('S'), Symbol::Terminal('a')],
//        &vec![Symbol::Nonterminal('S'), Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')]
//    ]));
//    println!("{}", FiniteStateAutomaton::build_fsa(&HashSet::from(['a', 'b', 'c']), 'S', &rules));
//}
//
//
#[test]
fn print_grammar_fsa() {
    let grammar = common::e_rule_relational_parsing_example_grammar();
    //let grammar = common::difficult_bottom_up_grammar();
    //let grammar = common::basic_relational_parsing_example_grammar();
    //let reg = Regex::new(&grammar.terminals, &grammar.rules);
    //reg.print_with_rules();
    println!("{}", grammar.finite_state_automaton);
}

#[test]
fn to_dot() {
    let grammar = common::basic_relational_parsing_example_grammar();
    grammar.finite_state_automaton.to_dot("basic relational").expect("error");

    let grammar = common::e_rule_relational_parsing_example_grammar();
    grammar.finite_state_automaton.to_dot("e-rule relational").expect("error");

    let grammar = common::extra_e_rule_relational_parsing_example_grammar();
    grammar.finite_state_automaton.to_dot("extra e-rule relational").expect("error");
    //for (src, words) in grammar.rules {
    //    for word in words {
    //        print!("{} -> ", src);
    //        for symbol in word {
    //            print!("{}", symbol);
    //        }
    //        println!();
    //    }
    //}

    let grammar = common::three_rule_grammar();
    grammar.finite_state_automaton.to_dot("three-rule").expect("error");

    let grammar = common::difficult_bottom_up_grammar();
    grammar.finite_state_automaton.to_dot("difficult bottom up").expect("error");

    let grammar = common::odd_number_of_a_grammar();
    grammar.finite_state_automaton.to_dot("odd nr of a").expect("error");

    let grammar = common::direct_left_recursive_grammar();
    grammar.finite_state_automaton.to_dot("direct left-recursive").expect("error");

    let grammar = common::indirect_left_recursive_grammar();
    grammar.finite_state_automaton.to_dot("indirect left-recursive").expect("error");

    let grammar = common::even_more_indirect_left_recursive_grammar();
    grammar.finite_state_automaton.to_dot("very indirect left-recursive").expect("error");

    let grammar = common::direct_right_recursive_grammar();
    grammar.finite_state_automaton.to_dot("direct right-recursive").expect("error");

    let grammar = common::indirect_right_recursive_grammar();
    grammar.finite_state_automaton.to_dot("indirect right-recursive").expect("error");
}

#[test]
fn lua_dot() {
    let grammar = common::lua_like_grammar();
    let reg = Regex::new(&grammar.terminals, &grammar.rules);
    println!("{}", reg);
    grammar.finite_state_automaton.to_dot("lua_like").expect("error");
}

#[test]
fn basic_relational_grammar_fsa_test() {
    let grammar = common::basic_relational_parsing_example_grammar();

    let fsa = grammar.finite_state_automaton;

    let (start_state, start_accepting) = fsa.get_start();
    assert!(!start_accepting);
}


#[test]
fn basic_relational_grammar_recog_test() {
    let grammar = common::basic_relational_parsing_example_grammar();
    let mut memoize: Memoize = Memoize::new();

    assert!(relational_parsing::g_accepts_string(&vec!['a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'c'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'c', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'a', 'c'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'b', 'a', 'b', 'a', 'c', 'c', 'c'], &grammar, &mut memoize));

    assert!(!relational_parsing::g_accepts_string(&vec![], &grammar, &mut memoize));
    assert!(!relational_parsing::g_accepts_string(&vec!['a', 'a', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize));
    assert!(!relational_parsing::g_accepts_string(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c'], &grammar, &mut memoize));
    assert!(!relational_parsing::g_accepts_string(&vec!['b', 'c', 'b', 'c', 'b', 'c', 'a'], &grammar, &mut memoize));

    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'b', 'a', 'c'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'b', 'a', 'c', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'b', 'a', 'a', 'c'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'b', 'a', 'b', 'a', 'b', 'a', 'c', 'c', 'c'], &grammar));

    assert!(!relational_parsing::g_accepts_string_no_memo(&vec![], &grammar));
    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a', 'b', 'a', 'c', 'c'], &grammar));
    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c'], &grammar));
    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['b', 'c', 'b', 'c', 'b', 'c', 'a'], &grammar));
}

#[test]
fn e_rule_relational_grammar_recog_test() {
    let grammar = common::e_rule_relational_parsing_example_grammar();
    let mut memoize: Memoize = Memoize::new();

    assert!(relational_parsing::g_accepts_string(&vec![], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'c'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'c', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'a', 'c'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['b', 'b', 'b', 'b', 'c', 'c', 'c', 'c'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['b', 'c', 'b', 'c', 'b', 'c', 'a'], &grammar, &mut memoize));

    assert!(!relational_parsing::g_accepts_string(&vec!['a', 'a', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize));
    assert!(!relational_parsing::g_accepts_string(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c'], &grammar, &mut memoize));

    assert!(relational_parsing::g_accepts_string_no_memo(&vec![], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'b', 'a', 'c'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'b', 'a', 'c', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'b', 'a', 'a', 'c'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['b', 'b', 'b', 'b', 'c', 'c', 'c', 'c'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['b', 'c', 'b', 'c', 'b', 'c', 'a'], &grammar));

    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a', 'b', 'a', 'c', 'c'], &grammar));
    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c'], &grammar));
}

#[test]
fn extra_e_rule_relational_grammar_recog_test() {
    let grammar = common::extra_e_rule_relational_parsing_example_grammar();
    let mut memoize: Memoize = Memoize::new();

    assert!(relational_parsing::g_accepts_string(&vec![], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'c'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'c', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'a', 'c'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'c', 'b', 'c', 'b', 'c', 'a'], &grammar, &mut memoize));

    assert!(!relational_parsing::g_accepts_string(&vec!['a', 'a', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize));
    assert!(!relational_parsing::g_accepts_string(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c'], &grammar, &mut memoize));

    assert!(relational_parsing::g_accepts_string_no_memo(&vec![], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'b', 'a', 'c'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'b', 'a', 'c', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'b', 'a', 'a', 'c'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'b', 'c', 'b', 'c', 'b', 'c', 'a'], &grammar));

    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a', 'b', 'a', 'c', 'c'], &grammar));
    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c'], &grammar));
}

#[test]
fn three_rule_grammar_recog_test() {
    let grammar = common::three_rule_grammar();
    let mut memoize: Memoize = Memoize::new();

    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'a', 'a', 'b'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'a', 'a', 'b', 'a', 'b'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'a', 'a', 'a', 'b', 'b'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'a', 'a', 'a', 'b', 'b', 'a', 'b'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'a', 'a', 'b', 'a', 'b', 'a', 'b'], &grammar, &mut memoize));

    assert!(!relational_parsing::g_accepts_string(&vec!['a', 'a', 'a', 'a', 'a', 'b'], &grammar, &mut memoize));
    assert!(!relational_parsing::g_accepts_string(&vec!['a', 'a', 'a', 'a', 'b', 'b'], &grammar, &mut memoize));
    assert!(!relational_parsing::g_accepts_string(&vec!['a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize));

    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a', 'a', 'b'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a', 'a', 'b', 'a', 'b'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a', 'a', 'a', 'b', 'b'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a', 'a', 'a', 'b', 'b', 'a', 'b'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a', 'a', 'b', 'a', 'b', 'a', 'b'], &grammar));

    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a', 'a', 'a', 'b'], &grammar));
    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a', 'a', 'b', 'b'], &grammar));
    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a', 'a', 'a'], &grammar));
}

#[test]
fn difficult_bottom_up_grammar_recog_test() {
    let grammar = common::difficult_bottom_up_grammar();
    let mut memoize: Memoize = Memoize::new();

    assert!(relational_parsing::g_accepts_string(&vec!['a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', '-', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', '-', 'a', '+', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', '-', 'a', '+', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', '+', 'a', '-', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', '-', 'a', '+', 'a', '-', 'a'], &grammar, &mut memoize));

    assert!(!relational_parsing::g_accepts_string(&vec!['a', 'a'], &grammar, &mut memoize));
    assert!(!relational_parsing::g_accepts_string(&vec!['a', 'a', 'a'], &grammar, &mut memoize));
    assert!(!relational_parsing::g_accepts_string(&vec!['-', '-'], &grammar, &mut memoize));
    assert!(!relational_parsing::g_accepts_string(&vec!['-', '-', 'a'], &grammar, &mut memoize));
    assert!(!relational_parsing::g_accepts_string(&vec!['a', '-', 'a', '+', '+'], &grammar, &mut memoize));
    assert!(!relational_parsing::g_accepts_string(&vec!['a', '-', 'a', 'a', 'a'], &grammar, &mut memoize));
    assert!(!relational_parsing::g_accepts_string(&vec!['a', '-', 'a', 'a', 'a'], &grammar, &mut memoize));

    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', '-', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', '-', 'a', '+', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', '-', 'a', '+', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', '+', 'a', '-', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', '-', 'a', '+', 'a', '-', 'a'], &grammar));

    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a'], &grammar));
    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a'], &grammar));
    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['-', '-'], &grammar));
    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['-', '-', 'a'], &grammar));
    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['a', '-', 'a', '+', '+'], &grammar));
    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['a', '-', 'a', 'a', 'a'], &grammar));
    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['a', '-', 'a', 'a', 'a'], &grammar));

}

#[test]
fn odd_number_of_a_grammar_recog_test() {
    let grammar = common::odd_number_of_a_grammar();
    let mut memoize: Memoize = Memoize::new();

    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize));

    assert!(!relational_parsing::g_accepts_string(&vec!['a', 'a'], &grammar, &mut memoize));
    assert!(!relational_parsing::g_accepts_string(&vec!['a', 'a', 'a', 'a'], &grammar, &mut memoize));
    assert!(!relational_parsing::g_accepts_string(&vec!['a', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize));

    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a', 'a', 'a'], &grammar));

    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a'], &grammar));
    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a', 'a'], &grammar));
    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a', 'a', 'a', 'a'], &grammar));
}

#[test]
fn even_a_middle_b_test() {
    let grammar = common::even_a_middle_b_grammar();
    let mut memoize: Memoize = Memoize::new();

    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'a', 'b', 'a', 'a', 'a'], &grammar, &mut memoize));

    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a', 'b', 'a', 'a', 'a'], &grammar));
}

#[test]
fn direct_left_recursive_grammar_recog_test() {
    let grammar = common::direct_left_recursive_grammar();
    let mut memoize: Memoize = Memoize::new();

    assert!(relational_parsing::g_accepts_string(&vec!['a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'a', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize));

    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a', 'a', 'a'], &grammar));
}

#[test]
fn indirect_left_recursive_grammar_recog_test() {
    let grammar = common::indirect_left_recursive_grammar();
    let mut memoize: Memoize = Memoize::new();

    assert!(relational_parsing::g_accepts_string(&vec!['a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['b', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['b', 'a', 'b', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'b', 'a'], &grammar, &mut memoize));

    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['b', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'b', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['b', 'a', 'b', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'b', 'a', 'b', 'a'], &grammar));
}

#[test]
fn even_more_indirect_left_recursive_grammar_recog_test() {
    let grammar = common::even_more_indirect_left_recursive_grammar();
    let mut memoize: Memoize = Memoize::new();

    assert!(relational_parsing::g_accepts_string(&vec!['a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'a', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['b', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['b', 'a', 'a', 'a', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['b', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['c', 'a', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['c', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['c', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize));

    assert!(!relational_parsing::g_accepts_string(&vec!['a', 'a'], &grammar, &mut memoize));
    assert!(!relational_parsing::g_accepts_string(&vec!['a', 'a', 'a'], &grammar, &mut memoize));
    assert!(!relational_parsing::g_accepts_string(&vec!['b'], &grammar, &mut memoize));
    assert!(!relational_parsing::g_accepts_string(&vec!['b', 'a', 'a'], &grammar, &mut memoize));
    assert!(!relational_parsing::g_accepts_string(&vec!['b', 'a', 'a', 'a'], &grammar, &mut memoize));
    assert!(!relational_parsing::g_accepts_string(&vec!['c'], &grammar, &mut memoize));
    assert!(!relational_parsing::g_accepts_string(&vec!['c', 'a'], &grammar, &mut memoize));
    assert!(!relational_parsing::g_accepts_string(&vec!['c', 'a', 'a', 'a'], &grammar, &mut memoize));
    assert!(!relational_parsing::g_accepts_string(&vec!['c', 'a', 'a', 'a', 'a'], &grammar, &mut memoize));

    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['b', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['b', 'a', 'a', 'a', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['b', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['c', 'a', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['c', 'a', 'a', 'a', 'a', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['c', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar));

    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a'], &grammar));
    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a'], &grammar));
    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['b'], &grammar));
    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['b', 'a', 'a'], &grammar));
    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['b', 'a', 'a', 'a'], &grammar));
    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['c'], &grammar));
    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['c', 'a'], &grammar));
    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['c', 'a', 'a', 'a'], &grammar));
    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['c', 'a', 'a', 'a', 'a'], &grammar));
}

#[test]
fn direct_right_recursive_grammar_recog_test() {
    let grammar = common::direct_right_recursive_grammar();
    let mut memoize: Memoize = Memoize::new();

    assert!(relational_parsing::g_accepts_string(&vec!['a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'a', 'a'], &grammar, &mut memoize));

    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a', 'a'], &grammar));
}

#[test]
fn indirect_right_recursive_grammar_recog_test() {
    let grammar = common::indirect_right_recursive_grammar();
    let mut memoize: Memoize = Memoize::new();

    assert!(relational_parsing::g_accepts_string(&vec!['a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'b'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'b'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'b', 'a', 'b'], &grammar, &mut memoize));

    assert!(!relational_parsing::g_accepts_string(&vec!['b'], &grammar, &mut memoize));
    assert!(!relational_parsing::g_accepts_string(&vec!['a', 'b', 'b'], &grammar, &mut memoize));
    assert!(!relational_parsing::g_accepts_string(&vec!['a', 'a'], &grammar, &mut memoize));

    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'b'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'b', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'b', 'a', 'b'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'b', 'a', 'b', 'a', 'b'], &grammar));

    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['b'], &grammar));
    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['a', 'b', 'b'], &grammar));
    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a'], &grammar));
}

fn build_rule(rule: (Nonterminal, &str)) -> (Nonterminal, Vec<Symbol>) {
    let mut res_rule: Vec<Symbol> = Vec::with_capacity(rule.1.len());

    for char in rule.1.chars() {
        if char == 'e' {
            res_rule.push(Symbol::Epsilon);
        } else if char.is_uppercase() {
            res_rule.push(Symbol::Nonterminal(char));
        } else {
            res_rule.push(Symbol::Terminal(char));
        }
    }

    (rule.0, res_rule)
}

fn build_rules(rules: &Vec<(Nonterminal, &str)>) -> Vec<(Nonterminal, Vec<Symbol>)> {
    let mut res_rules: Vec<(Nonterminal, Vec<Symbol>)> = Vec::with_capacity(rules.len());

    for rule in rules {
        res_rules.push(build_rule(*rule))
    }

    res_rules
}

#[test]
fn basic_relational_grammar_parse_test() {
    let grammar = common::basic_relational_parsing_example_grammar();
    let mut memoize: Memoize = Memoize::new();

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'b', 'a', 'b', 'a', 'b', 'a', 'c', 'c', 'c'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "a"), ('S', "a"), ('S', "a")])
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('S', "a")])
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('S', "Sa"), ('S', "a")])
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('S', "Sa"), ('S', "Sa"), ('S', "a")])
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'b', 'a', 'c'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('S', "SbSc"), ('S', "a"), ('S', "a")])
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'b', 'a', 'c', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('S', "Sa"), ('S', "SbSc"), ('S', "a"), ('S', "a")])
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'b', 'a', 'a', 'c'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('S', "SbSc"), ('S', "Sa"), ('S', "a"), ('S', "a")])
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "a"), ('S', "a")])
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "a"), ('S', "Sa"), ('S', "a")])
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('S', "Sa"), ('S', "Sa"), ('S', "Sa"), ('S', "Sa"), ('S', "Sa"), ('S', "Sa"), ('S', "Sa"), ('S', "a")])
        ]))
    );
    
    assert!(relational_parsing::find_parses(&vec![], &grammar, &mut memoize).is_err());
    assert!(relational_parsing::find_parses(&vec!['a', 'a', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize).is_err());
    assert!(relational_parsing::find_parses(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c'], &grammar, &mut memoize).is_err());
    assert!(relational_parsing::find_parses(&vec!['b', 'c', 'b', 'c', 'b', 'c', 'a'], &grammar, &mut memoize).is_err());

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'b', 'a', 'b', 'a', 'b', 'a', 'c', 'c', 'c'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "a"), ('S', "a"), ('S', "a")])
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('S', "a")])
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('S', "Sa"), ('S', "a")])
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('S', "Sa"), ('S', "Sa"), ('S', "a")])
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'b', 'a', 'c'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('S', "SbSc"), ('S', "a"), ('S', "a")])
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'b', 'a', 'c', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('S', "Sa"), ('S', "SbSc"), ('S', "a"), ('S', "a")])
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'b', 'a', 'a', 'c'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('S', "SbSc"), ('S', "Sa"), ('S', "a"), ('S', "a")])
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "a"), ('S', "a")])
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "a"), ('S', "Sa"), ('S', "a")])
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('S', "Sa"), ('S', "Sa"), ('S', "Sa"), ('S', "Sa"), ('S', "Sa"), ('S', "Sa"), ('S', "Sa"), ('S', "a")])
        ]))
    );
    
    assert!(relational_parsing::find_parses_no_memo(&vec![], &grammar).is_err());
    assert!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a', 'b', 'a', 'c', 'c'], &grammar).is_err());
    assert!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c'], &grammar).is_err());
    assert!(relational_parsing::find_parses_no_memo(&vec!['b', 'c', 'b', 'c', 'b', 'c', 'a'], &grammar).is_err());


}

#[test]
fn e_rule_relational_grammar_parse_test() {
    let grammar = common::e_rule_relational_parsing_example_grammar();
    let mut memoize: Memoize = Memoize::new();

    let tokens: Vec<char> = "babcbbabacbbacbbabbbabacbcbabababbbaababbbbabbabbabaaaaaacaccaaacaaaacacccacacacaaccaacacacaaaaaacccaaccaaaccccbacabacaaa".chars().collect();

    println!("{:?}", relational_parsing::find_parses(&tokens, &grammar, &mut memoize))

    // assert_eq!(relational_parsing::find_parses(&vec!['a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize),
    //     Ok(HashSet::from([
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "a"), ('S', "a")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "a"), ('S', "Sa"), ('S', "e")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "Sa"), ('S', "e"), ('S', "a")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "a"), ('S', "a")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "a"), ('S', "Sa"), ('S', "e")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e"), ('S', "a")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e")]),
    //     ]))
    // );

    // assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize),
    //     Ok(HashSet::from([
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "a"), ('S', "Sa"), ('S', "a")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "a"), ('S', "Sa"), ('S', "Sa"), ('S', "e")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "a")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "a"), ('S', "Sa"), ('S', "a")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "Sa"), ('S', "e")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "a"), ('S', "Sa"), ('S', "Sa"), ('S', "e")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "a")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "Sa"), ('S', "e")]),
    //     ]))
    // );

    // assert_eq!(relational_parsing::find_parses(&vec![], &grammar, &mut memoize),
    //     Ok(HashSet::from([
    //         build_rules(&vec![('S', "e")])
    //     ]))
    // );

    // assert_eq!(relational_parsing::find_parses(&vec!['a'], &grammar, &mut memoize),
    //     Ok(HashSet::from([
    //         build_rules(&vec![('S', "a")]),
    //         build_rules(&vec![('S', "Sa"), ('S', "e")])
    //     ]))
    // );

    // assert_eq!(relational_parsing::find_parses(&vec!['a', 'a'], &grammar, &mut memoize),
    //     Ok(HashSet::from([
    //         build_rules(&vec![('S', "Sa"), ('S', "a")]),
    //         build_rules(&vec![('S', "Sa"), ('S', "Sa"), ('S', "e")])
    //     ]))
    // );

    // assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a'], &grammar, &mut memoize),
    //     Ok(HashSet::from([
    //         build_rules(&vec![('S', "Sa"), ('S', "Sa"), ('S', "a")]),
    //         build_rules(&vec![('S', "Sa"), ('S', "Sa"), ('S', "Sa"), ('S', "e")])
    //     ]))
    // );

    // assert_eq!(relational_parsing::find_parses(&vec!['a', 'b', 'a', 'c'], &grammar, &mut memoize),
    //     Ok(HashSet::from([
    //         build_rules(&vec![('S', "SbSc"), ('S', "a"), ('S', "a")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "a")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "a"), ('S', "Sa"), ('S', "e")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e")])
    //     ]))
    // );

    // assert_eq!(relational_parsing::find_parses(&vec!['a', 'b', 'a', 'c', 'a'], &grammar, &mut memoize),
    //     Ok(HashSet::from([
    //         build_rules(&vec![('S', "Sa"), ('S', "SbSc"), ('S', "a"), ('S', "a")]),
    //         build_rules(&vec![('S', "Sa"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "a")]),
    //         build_rules(&vec![('S', "Sa"), ('S', "SbSc"), ('S', "a"), ('S', "Sa"), ('S', "e")]),
    //         build_rules(&vec![('S', "Sa"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e")])
    //     ]))
    // );

    // assert_eq!(relational_parsing::find_parses(&vec!['a', 'b', 'a', 'a', 'c'], &grammar, &mut memoize),
    //     Ok(HashSet::from([
    //         build_rules(&vec![('S', "SbSc"), ('S', "Sa"), ('S', "a"), ('S', "a")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "Sa"), ('S', "Sa"), ('S', "e"), ('S', "a")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "Sa"), ('S', "a"), ('S', "Sa"), ('S', "e")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "Sa"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e")])
    //     ]))
    // );


    // assert_eq!(relational_parsing::find_parses(&vec!['b', 'b', 'b', 'b', 'c', 'c', 'c', 'c'], &grammar, &mut memoize),
    //     Ok(HashSet::from([
    //         build_rules(&vec![('S', "SbSc"), ('S', "e"), ('S', "SbSc"), ('S', "e"), ('S', "SbSc"), ('S', "e"), ('S', "SbSc"), ('S', "e"), ('S', "e")]),
    //     ]))
    // );

    // assert_eq!(relational_parsing::find_parses(&vec!['b', 'c', 'b', 'c', 'b', 'c', 'a'], &grammar, &mut memoize),
    //     Ok(HashSet::from([
    //         build_rules(&vec![('S', "Sa"), ('S', "SbSc"), ('S', "e"), ('S', "SbSc"), ('S', "e"), ('S', "SbSc"), ('S', "e"), ('S', "e")]),
    //     ]))
    // );

    // assert!(relational_parsing::find_parses(&vec!['a', 'a', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize).is_err());
    // assert!(relational_parsing::find_parses(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c'], &grammar, &mut memoize).is_err());

    // assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar),
    //     Ok(HashSet::from([
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "a"), ('S', "a")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "a"), ('S', "Sa"), ('S', "e")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "Sa"), ('S', "e"), ('S', "a")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "a"), ('S', "a")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "a"), ('S', "Sa"), ('S', "e")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e"), ('S', "a")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e")]),
    //     ]))
    // );

    // assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar),
    //     Ok(HashSet::from([
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "a"), ('S', "Sa"), ('S', "a")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "a"), ('S', "Sa"), ('S', "Sa"), ('S', "e")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "a")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "a"), ('S', "Sa"), ('S', "a")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "Sa"), ('S', "e")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "a"), ('S', "Sa"), ('S', "Sa"), ('S', "e")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "a")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "Sa"), ('S', "e")]),
    //     ]))
    // );

    // //assert_eq!(relational_parsing::find_parses_no_memo(&vec![], &grammar),
    // //    Ok(HashSet::from([
    // //        build_rules(&vec![('S', "e")])
    // //    ]))
    // //);

    // assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a'], &grammar),
    //     Ok(HashSet::from([
    //         build_rules(&vec![('S', "a")]),
    //         build_rules(&vec![('S', "Sa"), ('S', "e")])
    //     ]))
    // );

    // assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a'], &grammar),
    //     Ok(HashSet::from([
    //         build_rules(&vec![('S', "Sa"), ('S', "a")]),
    //         build_rules(&vec![('S', "Sa"), ('S', "Sa"), ('S', "e")])
    //     ]))
    // );

    // assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a'], &grammar),
    //     Ok(HashSet::from([
    //         build_rules(&vec![('S', "Sa"), ('S', "Sa"), ('S', "a")]),
    //         build_rules(&vec![('S', "Sa"), ('S', "Sa"), ('S', "Sa"), ('S', "e")])
    //     ]))
    // );

    // assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'b', 'a', 'c'], &grammar),
    //     Ok(HashSet::from([
    //         build_rules(&vec![('S', "SbSc"), ('S', "a"), ('S', "a")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "a")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "a"), ('S', "Sa"), ('S', "e")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e")])
    //     ]))
    // );

    // assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'b', 'a', 'c', 'a'], &grammar),
    //     Ok(HashSet::from([
    //         build_rules(&vec![('S', "Sa"), ('S', "SbSc"), ('S', "a"), ('S', "a")]),
    //         build_rules(&vec![('S', "Sa"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "a")]),
    //         build_rules(&vec![('S', "Sa"), ('S', "SbSc"), ('S', "a"), ('S', "Sa"), ('S', "e")]),
    //         build_rules(&vec![('S', "Sa"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e")])
    //     ]))
    // );

    // assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'b', 'a', 'a', 'c'], &grammar),
    //     Ok(HashSet::from([
    //         build_rules(&vec![('S', "SbSc"), ('S', "Sa"), ('S', "a"), ('S', "a")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "Sa"), ('S', "Sa"), ('S', "e"), ('S', "a")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "Sa"), ('S', "a"), ('S', "Sa"), ('S', "e")]),
    //         build_rules(&vec![('S', "SbSc"), ('S', "Sa"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e")])
    //     ]))
    // );


    // assert_eq!(relational_parsing::find_parses_no_memo(&vec!['b', 'b', 'b', 'b', 'c', 'c', 'c', 'c'], &grammar),
    //     Ok(HashSet::from([
    //         build_rules(&vec![('S', "SbSc"), ('S', "e"), ('S', "SbSc"), ('S', "e"), ('S', "SbSc"), ('S', "e"), ('S', "SbSc"), ('S', "e"), ('S', "e")]),
    //     ]))
    // );

    // assert_eq!(relational_parsing::find_parses_no_memo(&vec!['b', 'c', 'b', 'c', 'b', 'c', 'a'], &grammar),
    //     Ok(HashSet::from([
    //         build_rules(&vec![('S', "Sa"), ('S', "SbSc"), ('S', "e"), ('S', "SbSc"), ('S', "e"), ('S', "SbSc"), ('S', "e"), ('S', "e")]),
    //     ]))
    // );

    // assert!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a', 'b', 'a', 'c', 'c'], &grammar).is_err());
    // assert!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c'], &grammar).is_err());

}

#[test]
fn three_rule_grammar_parse_test() {
    let grammar = common::three_rule_grammar();
    let mut memoize: Memoize = Memoize::new();

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a', 'b', 'a', 'b', 'a', 'b'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('S', "Sab"), ('S', "Sab"), ('S', "aSb"), ('S', "aaa")]),
            build_rules(&vec![('S', "Sab"), ('S', "Sab"), ('S', "Sab"), ('S', "aaa")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('S', "aaa")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a', 'b'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('S', "aSb"), ('S', "aaa")]),
            build_rules(&vec![('S', "Sab"), ('S', "aaa")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a', 'b', 'a', 'b'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('S', "Sab"), ('S', "aSb"), ('S', "aaa")]),
            build_rules(&vec![('S', "Sab"), ('S', "Sab"), ('S', "aaa")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a', 'a', 'b', 'b'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('S', "aSb"), ('S', "aSb"), ('S', "aaa")]),
            build_rules(&vec![('S', "aSb"), ('S', "Sab"), ('S', "aaa")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a', 'a', 'b', 'b', 'a', 'b'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('S', "Sab"), ('S', "aSb"), ('S', "aSb"), ('S', "aaa")]),
            build_rules(&vec![('S', "Sab"), ('S', "aSb"), ('S', "Sab"), ('S', "aaa")]),
        ]))
    );

    assert!(relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a', 'a', 'b'], &grammar, &mut memoize).is_err());
    assert!(relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a', 'b', 'b'], &grammar, &mut memoize).is_err());
    assert!(relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize).is_err());

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a', 'a', 'b', 'a', 'b', 'a', 'b'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('S', "Sab"), ('S', "Sab"), ('S', "aSb"), ('S', "aaa")]),
            build_rules(&vec![('S', "Sab"), ('S', "Sab"), ('S', "Sab"), ('S', "aaa")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('S', "aaa")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a', 'a', 'b'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('S', "aSb"), ('S', "aaa")]),
            build_rules(&vec![('S', "Sab"), ('S', "aaa")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a', 'a', 'b', 'a', 'b'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('S', "Sab"), ('S', "aSb"), ('S', "aaa")]),
            build_rules(&vec![('S', "Sab"), ('S', "Sab"), ('S', "aaa")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a', 'a', 'a', 'b', 'b'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('S', "aSb"), ('S', "aSb"), ('S', "aaa")]),
            build_rules(&vec![('S', "aSb"), ('S', "Sab"), ('S', "aaa")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a', 'a', 'a', 'b', 'b', 'a', 'b'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('S', "Sab"), ('S', "aSb"), ('S', "aSb"), ('S', "aaa")]),
            build_rules(&vec![('S', "Sab"), ('S', "aSb"), ('S', "Sab"), ('S', "aaa")]),
        ]))
    );

    assert!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a', 'a', 'a', 'b'], &grammar).is_err());
    assert!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a', 'a', 'b', 'b'], &grammar).is_err());
    assert!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a', 'a', 'a'], &grammar).is_err());

}

#[test]
fn difficult_bottom_up_grammar_parse_test() {
    let grammar = common::difficult_bottom_up_grammar();
    let mut memoize: Memoize = Memoize::new();

    assert_eq!(relational_parsing::find_parses(&vec!['a', '-', 'a', '+', 'a', '-', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('E', "EQF"), ('F', "a"), ('Q', "-"), ('E', "EQF"), ('F', "a"), ('Q', "+"), ('E', "EQF"), ('F', "a"), ('Q', "-"), ('S', "E"), ('E', "F"), ('F', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('S', "E"), ('E', "F"), ('F', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a', '-', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('E', "EQF"), ('F', "a"), ('Q', "-"), ('S', "E"), ('E', "F"), ('F', "a")]),
        ]))
    );
    assert_eq!(relational_parsing::find_parses(&vec!['a', '-', 'a', '+', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('E', "EQF"), ('F', "a"), ('Q', "+"), ('E', "EQF"), ('F', "a"), ('Q', "-"), ('S', "E"), ('E', "F"), ('F', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a', '+', 'a', '-', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('E', "EQF"), ('F', "a"), ('Q', "-"), ('E', "EQF"), ('F', "a"), ('Q', "+"), ('S', "E"), ('E', "F"), ('F', "a")]),
        ]))
    );

    assert!(relational_parsing::find_parses(&vec!['a', 'a'], &grammar, &mut memoize).is_err());
    assert!(relational_parsing::find_parses(&vec!['a', 'a', 'a'], &grammar, &mut memoize).is_err());
    assert!(relational_parsing::find_parses(&vec!['-', '-'], &grammar, &mut memoize).is_err());
    assert!(relational_parsing::find_parses(&vec!['-', '-', 'a'], &grammar, &mut memoize).is_err());
    assert!(relational_parsing::find_parses(&vec!['a', '-', 'a', '+', '+'], &grammar, &mut memoize).is_err());
    assert!(relational_parsing::find_parses(&vec!['a', '-', 'a', 'a', 'a'], &grammar, &mut memoize).is_err());
    assert!(relational_parsing::find_parses(&vec!['a', '-', 'a', 'a', 'a'], &grammar, &mut memoize).is_err());

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', '-', 'a', '+', 'a', '-', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('E', "EQF"), ('F', "a"), ('Q', "-"), ('E', "EQF"), ('F', "a"), ('Q', "+"), ('E', "EQF"), ('F', "a"), ('Q', "-"), ('S', "E"), ('E', "F"), ('F', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('S', "E"), ('E', "F"), ('F', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', '-', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('E', "EQF"), ('F', "a"), ('Q', "-"), ('S', "E"), ('E', "F"), ('F', "a")]),
        ]))
    );
    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', '-', 'a', '+', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('E', "EQF"), ('F', "a"), ('Q', "+"), ('E', "EQF"), ('F', "a"), ('Q', "-"), ('S', "E"), ('E', "F"), ('F', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', '+', 'a', '-', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('E', "EQF"), ('F', "a"), ('Q', "-"), ('E', "EQF"), ('F', "a"), ('Q', "+"), ('S', "E"), ('E', "F"), ('F', "a")]),
        ]))
    );

    assert!(relational_parsing::find_parses_no_memo(&vec!['a', 'a'], &grammar).is_err());
    assert!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a'], &grammar).is_err());
    assert!(relational_parsing::find_parses_no_memo(&vec!['-', '-'], &grammar).is_err());
    assert!(relational_parsing::find_parses_no_memo(&vec!['-', '-', 'a'], &grammar).is_err());
    assert!(relational_parsing::find_parses_no_memo(&vec!['a', '-', 'a', '+', '+'], &grammar).is_err());
    assert!(relational_parsing::find_parses_no_memo(&vec!['a', '-', 'a', 'a', 'a'], &grammar).is_err());
    assert!(relational_parsing::find_parses_no_memo(&vec!['a', '-', 'a', 'a', 'a'], &grammar).is_err());
}

#[test]
fn odd_number_of_a_grammar_parse_test() {
    let grammar = common::odd_number_of_a_grammar();
    let mut memoize: Memoize = Memoize::new();

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('S', "aSa"), ('S', "aSa"), ('S', "aSa"), ('S', "aSa"), ('S', "aSa"), ('S', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('S', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('S', "aSa"), ('S', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('S', "aSa"), ('S', "aSa"), ('S', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('S', "aSa"), ('S', "aSa"), ('S', "aSa"), ('S', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('S', "aSa"), ('S', "aSa"), ('S', "aSa"), ('S', "aSa"), ('S', "a")]),
        ]))
    );

    assert!(relational_parsing::find_parses(&vec!['a', 'a'], &grammar, &mut memoize).is_err());
    assert!(relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a'], &grammar, &mut memoize).is_err());
    assert!(relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize).is_err());

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('S', "aSa"), ('S', "aSa"), ('S', "aSa"), ('S', "aSa"), ('S', "aSa"), ('S', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('S', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('S', "aSa"), ('S', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a', 'a', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('S', "aSa"), ('S', "aSa"), ('S', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('S', "aSa"), ('S', "aSa"), ('S', "aSa"), ('S', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('S', "aSa"), ('S', "aSa"), ('S', "aSa"), ('S', "aSa"), ('S', "a")]),
        ]))
    );

    assert!(relational_parsing::find_parses_no_memo(&vec!['a', 'a'], &grammar).is_err());
    assert!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a', 'a'], &grammar).is_err());
    assert!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a', 'a', 'a', 'a'], &grammar).is_err());
}

#[test]
fn even_a_middle_b_parse_test() {
    let grammar = common::even_a_middle_b_grammar();
    let mut memoize: Memoize = Memoize::new();

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a', 'b', 'a', 'a', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('S', "aSa"), ('S', "aSa"), ('S', "aSa"), ('S', "b")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a', 'b', 'a', 'a', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('S', "aSa"), ('S', "aSa"), ('S', "aSa"), ('S', "b")]),
        ]))
    );
}

#[test]
fn direct_left_recursive_grammar_parse_test() {
    let grammar = common::direct_left_recursive_grammar();
    let mut memoize: Memoize = Memoize::new();

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Aa"), ('A', "Aa"), ('A', "Aa"), ('A', "Aa"), ('A', "Aa"), ('A', "e")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Aa"), ('A', "e")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Aa"), ('A', "Aa"), ('A', "e")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Aa"), ('A', "Aa"), ('A', "Aa"), ('A', "e")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Aa"), ('A', "Aa"), ('A', "Aa"), ('A', "Aa"), ('A', "e")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a', 'a', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Aa"), ('A', "Aa"), ('A', "Aa"), ('A', "Aa"), ('A', "Aa"), ('A', "e")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Aa"), ('A', "e")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Aa"), ('A', "Aa"), ('A', "e")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Aa"), ('A', "Aa"), ('A', "Aa"), ('A', "e")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Aa"), ('A', "Aa"), ('A', "Aa"), ('A', "Aa"), ('A', "e")]),
        ]))
    );
}

#[test]
fn indirect_left_recursive_grammar_parse_test() {
    let grammar = common::indirect_left_recursive_grammar();
    let mut memoize: Memoize = Memoize::new();

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'b', 'a', 'b', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Ba"), ('B', "Ab"), ('A', "Ba"), ('B', "Ab"), ('A', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('A', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['b', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Ba"), ('B', "b")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'b', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Ba"), ('B', "Ab"), ('A', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['b', 'a', 'b', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Ba"), ('B', "Ab"), ('A', "Ba"), ('B', "b")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'b', 'a', 'b', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Ba"), ('B', "Ab"), ('A', "Ba"), ('B', "Ab"), ('A', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('A', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['b', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Ba"), ('B', "b")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'b', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Ba"), ('B', "Ab"), ('A', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['b', 'a', 'b', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Ba"), ('B', "Ab"), ('A', "Ba"), ('B', "b")]),
        ]))
    );
}

#[test]
fn even_more_indirect_left_recursive_grammar_parse_test() {
    let grammar = common::even_more_indirect_left_recursive_grammar();
    let mut memoize: Memoize = Memoize::new();

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('A', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['b', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Ba"), ('B', "b")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['b', 'a', 'a', 'a', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "Ba"), ('B', "b")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['b', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "Ba"), ('B', "b")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['c', 'a', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Ba"), ('B', "Ca"), ('C', "c")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['c', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "Ba"), ('B', "Ca"), ('C', "c")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['c', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "Ba"), ('B', "Ca"), ('C', "c")]),
        ]))
    );

    assert!(relational_parsing::find_parses(&vec!['a', 'a'], &grammar, &mut memoize).is_err());
    assert!(relational_parsing::find_parses(&vec!['a', 'a', 'a'], &grammar, &mut memoize).is_err());
    assert!(relational_parsing::find_parses(&vec!['b'], &grammar, &mut memoize).is_err());
    assert!(relational_parsing::find_parses(&vec!['b', 'a', 'a'], &grammar, &mut memoize).is_err());
    assert!(relational_parsing::find_parses(&vec!['b', 'a', 'a', 'a'], &grammar, &mut memoize).is_err());
    assert!(relational_parsing::find_parses(&vec!['c'], &grammar, &mut memoize).is_err());
    assert!(relational_parsing::find_parses(&vec!['c', 'a'], &grammar, &mut memoize).is_err());
    assert!(relational_parsing::find_parses(&vec!['c', 'a', 'a', 'a'], &grammar, &mut memoize).is_err());
    assert!(relational_parsing::find_parses(&vec!['c', 'a', 'a', 'a', 'a'], &grammar, &mut memoize).is_err());

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('A', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['b', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Ba"), ('B', "b")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['b', 'a', 'a', 'a', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "Ba"), ('B', "b")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['b', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "Ba"), ('B', "b")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['c', 'a', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Ba"), ('B', "Ca"), ('C', "c")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['c', 'a', 'a', 'a', 'a', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "Ba"), ('B', "Ca"), ('C', "c")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['c', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "Ba"), ('B', "Ca"), ('C', "Aa"), ('A', "Ba"), ('B', "Ca"), ('C', "c")]),
        ]))
    );

    assert!(relational_parsing::find_parses_no_memo(&vec!['a', 'a'], &grammar).is_err());
    assert!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a'], &grammar).is_err());
    assert!(relational_parsing::find_parses_no_memo(&vec!['b'], &grammar).is_err());
    assert!(relational_parsing::find_parses_no_memo(&vec!['b', 'a', 'a'], &grammar).is_err());
    assert!(relational_parsing::find_parses_no_memo(&vec!['b', 'a', 'a', 'a'], &grammar).is_err());
    assert!(relational_parsing::find_parses_no_memo(&vec!['c'], &grammar).is_err());
    assert!(relational_parsing::find_parses_no_memo(&vec!['c', 'a'], &grammar).is_err());
    assert!(relational_parsing::find_parses_no_memo(&vec!['c', 'a', 'a', 'a'], &grammar).is_err());
    assert!(relational_parsing::find_parses_no_memo(&vec!['c', 'a', 'a', 'a', 'a'], &grammar).is_err());
}

#[test]
fn direct_right_recursive_grammar_parse_test() {
    let grammar = common::direct_right_recursive_grammar();
    let mut memoize: Memoize = Memoize::new();

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('A', "aA"), ('A', "aA"), ('A', "aA"), ('A', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('A', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('A', "aA"), ('A', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('A', "aA"), ('A', "aA"), ('A', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('A', "aA"), ('A', "aA"), ('A', "aA"), ('A', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('A', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('A', "aA"), ('A', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'a', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('A', "aA"), ('A', "aA"), ('A', "a")]),
        ]))
    );
}

#[test]
fn indirect_right_recursive_grammar_parse_test() {
    let grammar = common::indirect_right_recursive_grammar();
    let mut memoize: Memoize = Memoize::new();

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'b', 'a', 'b', 'a', 'b'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('A', "aB"), ('B', "bA"), ('A', "aB"), ('B', "bA"), ('A', "aB"), ('B', "b")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('A', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'b'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('A', "aB"), ('B', "b")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'b', 'a'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('A', "aB"), ('B', "bA"), ('A', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses(&vec!['a', 'b', 'a', 'b'], &grammar, &mut memoize),
        Ok(HashSet::from([
            build_rules(&vec![('A', "aB"), ('B', "bA"), ('A', "aB"), ('B', "b")]),
        ]))
    );

    assert!(relational_parsing::find_parses(&vec!['b'], &grammar, &mut memoize).is_err());
    assert!(relational_parsing::find_parses(&vec!['a', 'b', 'b'], &grammar, &mut memoize).is_err());
    assert!(relational_parsing::find_parses(&vec!['a', 'a'], &grammar, &mut memoize).is_err());

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'b', 'a', 'b', 'a', 'b'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('A', "aB"), ('B', "bA"), ('A', "aB"), ('B', "bA"), ('A', "aB"), ('B', "b")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('A', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'b'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('A', "aB"), ('B', "b")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'b', 'a'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('A', "aB"), ('B', "bA"), ('A', "a")]),
        ]))
    );

    assert_eq!(relational_parsing::find_parses_no_memo(&vec!['a', 'b', 'a', 'b'], &grammar),
        Ok(HashSet::from([
            build_rules(&vec![('A', "aB"), ('B', "bA"), ('A', "aB"), ('B', "b")]),
        ]))
    );

    assert!(relational_parsing::find_parses_no_memo(&vec!['b'], &grammar).is_err());
    assert!(relational_parsing::find_parses_no_memo(&vec!['a', 'b', 'b'], &grammar).is_err());
    assert!(relational_parsing::find_parses_no_memo(&vec!['a', 'a'], &grammar).is_err());
}

//#[test]
//fn strange_recursive_grammar_parse_test() {
    //let grammar = common::strange_recursive_grammar();
    //let mut memoize: Memoize = Memoize::new();
    //grammar.finite_state_automaton.to_dot("highly_ambiguous").expect("error");

    //relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize);
    //relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a'], &grammar, &mut memoize);
    //relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize);
    //relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize);
    //relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize);

    //assert_eq!(relational_parsing::find_parses(&vec!['a'], &grammar, &mut memoize),
        //Ok(HashSet::from([
            //build_rules(&vec![('A', "a")]),
        //]))
    //);

    //assert_eq!(relational_parsing::find_parses(&vec!['a', 'a'], &grammar, &mut memoize),
        //Ok(HashSet::from([
            //build_rules(&vec![('A', "aA"), ('A', "a")]),
        //]))
    //);

    //assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a'], &grammar, &mut memoize),
        //Ok(HashSet::from([
            //build_rules(&vec![('A', "aA"), ('A', "aA"), ('A', "a")]),
        //]))
    //);
//}

