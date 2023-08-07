use relational_parsing::{
    self,
    RulesSet, Terminal, ParseError
};

mod common;

fn get_test_cases() -> [(Vec<Terminal>, Result<RulesSet, ParseError>); 4] {
    [
        (vec!['a'], Ok(RulesSet::from_vec_slice(&[(vec![('A', "a")])]))),
        (vec!['a', 'a'], Ok(RulesSet::from_vec_slice(&[(vec![('A', "aA"), ('A', "a")])]))),
        (vec!['a', 'a', 'a'], Ok(RulesSet::from_vec_slice(&[(vec![('A', "aA"), ('A', "aA"), ('A', "a")])]))),
        (vec!['a', 'a', 'a', 'a'], Ok(RulesSet::from_vec_slice(&[(vec![('A', "aA"), ('A', "aA"), ('A', "aA"), ('A', "a")])]))),
    ]
}

#[test]
fn direct_right_recursive_grammar_recog_test() {
    let grammar = common::direct_right_recursive_grammar();
    //     let mut memoize: Memoize = Memoize::new();

    let test_cases = get_test_cases();

    for (input, expected) in test_cases {
        assert!(relational_parsing::g_accepts_string_no_memo(&input, &grammar) == expected.is_ok());
    }
}

#[test]
fn direct_right_recursive_parse_test() {
    let grammar = common::direct_right_recursive_grammar();
    //let mut memoize: Memoize = Memoize::new();

    let test_cases = get_test_cases();
    for (input, expected) in test_cases {
        assert_eq!(relational_parsing::find_parses_no_memo(&input, &grammar), expected);
    }
}
