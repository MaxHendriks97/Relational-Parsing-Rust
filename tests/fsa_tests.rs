mod common;

#[test]
fn build_fsa_test() {
    let grammar = common::basic_relational_parsing_example_grammar();
    println!("{}", grammar.finite_state_automaton);
}


// #[test]
// fn print_grammar_fsa() {
//     let grammar = common::e_rule_relational_parsing_example_grammar();
//     //let grammar = common::difficult_bottom_up_grammar();
//     //let grammar = common::basic_relational_parsing_example_grammar();
//     //let reg = Regex::new(&grammar.terminals, &grammar.rules);
//     //reg.print_with_rules();
//     println!("{}", grammar.finite_state_automaton);
// }

#[test]
fn to_dot() {
    println!("basic relational");
    let grammar = common::basic_relational_parsing_example_grammar();
    grammar.finite_state_automaton.to_dot("basic relational").expect("error");

    println!("e-rule relational");
    let grammar = common::e_rule_relational_parsing_example_grammar();
    grammar.finite_state_automaton.to_dot("e-rule relational").expect("error");

    println!("extra e-rule relational");
    let grammar = common::extra_e_rule_relational_parsing_example_grammar();
    grammar.finite_state_automaton.to_dot("extra e-rule relational").expect("error");

    println!("three-rule");
    let grammar = common::three_rule_grammar();
    grammar.finite_state_automaton.to_dot("three-rule").expect("error");

    println!("difficult bottom up");
    let grammar = common::difficult_bottom_up_grammar();
    grammar.finite_state_automaton.to_dot("difficult bottom up").expect("error");

    println!("odd nr of a");
    let grammar = common::odd_number_of_a_grammar();
    grammar.finite_state_automaton.to_dot("odd nr of a").expect("error");

    println!("direct left-recursive");
    let grammar = common::direct_left_recursive_grammar();
    grammar.finite_state_automaton.to_dot("direct left-recursive").expect("error");

    //let grammar = common::indirect_left_recursive_grammar();
    //grammar.finite_state_automaton.to_dot("indirect left-recursive").expect("error");

    //let grammar = common::even_more_indirect_left_recursive_grammar();
    //grammar.finite_state_automaton.to_dot("very indirect left-recursive").expect("error");

    println!("direct right-recursive");
    let grammar = common::direct_right_recursive_grammar();
    grammar.finite_state_automaton.to_dot("direct right-recursive").expect("error");

    println!("indirect right-recursive");
    let grammar = common::indirect_right_recursive_grammar();
    grammar.finite_state_automaton.to_dot("indirect right-recursive").expect("error");
}

// #[test]
// fn lua_dot() {
//     let grammar = common::lua_like_grammar();
//     let reg = Regex::new(&grammar.terminals, &grammar.rules);
//     println!("{}", reg);
//     println!("{:?}", reg.regex.get(&('G', 'n')).unwrap());
//     grammar.finite_state_automaton.to_dot("lua_like").expect("error");
// }
