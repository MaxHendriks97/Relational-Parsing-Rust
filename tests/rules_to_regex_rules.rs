use relational_parsing::Regex;

mod common;

#[test]
fn rules_to_regex_rules_test() {
    let grammar = common::basic_relational_parsing_example_grammar();
    let reg = Regex::new(&grammar.terminals, &grammar.rules);
    println!("{}", grammar);
    println!("{}", reg);

    let grammar = common::e_rule_relational_parsing_example_grammar();
    let reg = Regex::new(&grammar.terminals, &grammar.rules);
    println!("{}", grammar);
    println!("{}", reg);

    let grammar = common::extra_e_rule_relational_parsing_example_grammar();
    let reg = Regex::new(&grammar.terminals, &grammar.rules);
    println!("{}", grammar);
    println!("{}", reg);

    let grammar = common::three_rule_grammar();
    let reg = Regex::new(&grammar.terminals, &grammar.rules);
    println!("{}", grammar);
    println!("{}", reg);

    let grammar = common::difficult_bottom_up_grammar();
    let reg = Regex::new(&grammar.terminals, &grammar.rules);
    println!("{}", grammar);
    println!("{}", reg);

    let grammar = common::odd_number_of_a_grammar();
    let reg = Regex::new(&grammar.terminals, &grammar.rules);
    println!("{}", grammar);
    println!("{}", reg);

    let grammar = common::direct_left_recursive_grammar();
    let reg = Regex::new(&grammar.terminals, &grammar.rules);
    println!("{}", grammar);
    println!("{}", reg);

    //let grammar = common::indirect_left_recursive_grammar();
    //let reg = Regex::new(&grammar.terminals, &grammar.rules);
    //println!("{}", grammar);
    //println!("{}", reg);

    //let grammar = common::even_more_indirect_left_recursive_grammar();
    //let reg = Regex::new(&grammar.terminals, &grammar.rules);
    //println!("{}", grammar);
    //println!("{}", reg);

    let grammar = common::direct_right_recursive_grammar();
    let reg = Regex::new(&grammar.terminals, &grammar.rules);
    println!("{}", grammar);
    println!("{}", reg);

    let grammar = common::indirect_right_recursive_grammar();
    let reg = Regex::new(&grammar.terminals, &grammar.rules);
    println!("{}", grammar);
    println!("{}", reg);

    let grammar = common::strange_recursive_grammar();
    let reg = Regex::new(&grammar.terminals, &grammar.rules);
    println!("{}", grammar);
    println!("{}", reg);
}
