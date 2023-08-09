use relational_parsing::{
    self,
    RulesSet, Terminal, ParseError, Memoize
};

mod common;

fn get_test_cases() -> [(Vec<Terminal>, Result<RulesSet, ParseError>); 8] {
    [
        (vec!['a'], Ok(RulesSet::from_vec_slice(&[(vec![('A', "a")])]))),
        (vec!['a', 'b'], Ok(RulesSet::from_vec_slice(&[(vec![('A', "aB"), ('B', "b")])]))),
        (vec!['a', 'b', 'a'], Ok(RulesSet::from_vec_slice(&[(vec![('A', "aB"), ('B', "bA"), ('A', "a")])]))),
        (vec!['a', 'b', 'a', 'b'], Ok(RulesSet::from_vec_slice(&[(vec![('A', "aB"), ('B', "bA"), ('A', "aB"), ('B', "b")])]))),
        (vec!['a', 'b', 'a', 'b', 'a', 'b'], Ok(RulesSet::from_vec_slice(&[(vec![('A', "aB"), ('B', "bA"), ('A', "aB"), ('B', "bA"), ('A', "aB"), ('B', "b")])]))),

        (vec!['b'], Err(ParseError)),
        (vec!['a', 'b', 'b'], Err(ParseError)),
        (vec!['a', 'a'], Err(ParseError)),
    ]
}

#[test]
fn memo_recog() {
    let grammar = common::indirect_right_recursive_grammar();
    let test_cases = get_test_cases();

    for (input, expected) in test_cases {
        assert!(relational_parsing::g_accepts_string(&input, &grammar, &mut Memoize::new()) == expected.is_ok());
    }
}

#[test]
fn memo_parse() {
    let grammar = common::indirect_right_recursive_grammar();
    let test_cases = get_test_cases();

    for (input, expected) in test_cases {
        assert_eq!(relational_parsing::find_parses(&input, &grammar, &mut Memoize::new()), expected);
    }
}

#[test]
fn indirect_right_recursive_grammar_recog_test() {
    let grammar = common::indirect_right_recursive_grammar();
    //     let mut memoize: Memoize = Memoize::new();

    let test_cases = get_test_cases();

    for (input, expected) in test_cases {
        assert!(relational_parsing::g_accepts_string_no_memo(&input, &grammar) == expected.is_ok());
    }
}

#[test]
fn indirect_right_recursive_parse_test() {
    let grammar = common::indirect_right_recursive_grammar();
    //let mut memoize: Memoize = Memoize::new();

    let test_cases = get_test_cases();
    for (input, expected) in test_cases {
        assert_eq!(relational_parsing::find_parses_no_memo(&input, &grammar), expected);
    }
}
