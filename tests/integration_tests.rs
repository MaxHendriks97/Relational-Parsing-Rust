use std::collections::{HashMap, HashSet, VecDeque};

use relational_parsing;
use crate::relational_parsing::{Regex, FiniteStateAutomaton, RegexNode, State, Symbol, Terminal, Nonterminal, Word};

mod common;

#[test]
fn rules_to_regex_rules_test() {
    let grammar = common::basic_relational_parsing_example_grammar();
    println!("{}", Regex::new(&grammar.terminals, &grammar.rules));

    let grammar = common::e_rule_relational_parsing_example_grammar();
    println!("{}", Regex::new(&grammar.terminals, &grammar.rules));

    let grammar = common::three_rule_grammar();
    println!("{}", Regex::new(&grammar.terminals, &grammar.rules));

    let grammar = common::difficult_bottom_up_grammar();
    println!("{}", Regex::new(&grammar.terminals, &grammar.rules));

    let grammar = common::odd_number_of_a_grammar();
    println!("{}", Regex::new(&grammar.terminals, &grammar.rules));

    let grammar = common::direct_left_recursive_grammar();
    println!("{}", Regex::new(&grammar.terminals, &grammar.rules));

    let grammar = common::indirect_left_recursive_grammar();
    println!("{}", Regex::new(&grammar.terminals, &grammar.rules));

    let grammar = common::even_more_indirect_left_recursive_grammar();
    println!("{}", Regex::new(&grammar.terminals, &grammar.rules));

    let grammar = common::direct_right_recursive_grammar();
    println!("{}", Regex::new(&grammar.terminals, &grammar.rules));

    let grammar = common::indirect_right_recursive_grammar();
    println!("{}", Regex::new(&grammar.terminals, &grammar.rules));
}

#[test]
fn regex_node_to_states_test() {
    let grammar = common::e_rule_relational_parsing_example_grammar();
    let regex = Regex::new(&grammar.terminals, &grammar.rules);
    if let RegexNode::Node(regex_node) = regex.regex.get(&('S', 'b')).unwrap() {
        println!("{}", regex_node.nodes.get(1).unwrap());
        let mut regex_to_state: HashMap<VecDeque<RegexNode>, (State, State)> = HashMap::new();
        println!("{:?}", FiniteStateAutomaton::atomic_regex_to_states(regex_node.nodes.get(1).unwrap(), None, 0, &mut regex_to_state));
        //if let RegexNode::Word(word_node) = regex_node.nodes.get(1).unwrap() {
        //    println!("{:?}", Grammar::word_node_to_states(word_node, 1, 0, 1))
        //}
    }

    //println!("{:?}", Grammar::regex_node_to_states(regex.regex.get(&('S', 'b')).unwrap(), 1, 0, 1));

    //let grammar = odd_number_of_a_grammar();
    //let regex = Regex::new(&grammar.terminals, &grammar.rules);
    //println!("{}", regex);
    //println!("{:?}", Grammar::regex_node_to_states(regex.regex.get(&('S', 'a')).unwrap(), 1));

}

#[test]
fn build_fsa_test() {
    let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('S', HashSet::from([
        vec![Symbol::Terminal('a')],
        vec![Symbol::Nonterminal('S'), Symbol::Terminal('a')],
        vec![Symbol::Nonterminal('S'), Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')]
    ]));
    println!("{}", FiniteStateAutomaton::build_fsa(&HashSet::from(['a', 'b', 'c']), Symbol::Nonterminal('S'), &rules));
}

#[test]
fn to_dot() {
    let grammar = common::basic_relational_parsing_example_grammar();
    grammar.finite_state_automaton.to_dot("basic relational").expect("error");

    let grammar = common::e_rule_relational_parsing_example_grammar();
    grammar.finite_state_automaton.to_dot("e-rule relational").expect("error");

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