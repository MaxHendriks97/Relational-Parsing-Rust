use relational_parsing::{
    self,
    RulesSet, Terminal, ParseError
};

mod common;

fn get_test_cases() -> [(Vec<Terminal>, Result<RulesSet, ParseError>); 5] {
    [
        (vec!['a'], Ok(RulesSet::from_vec_slice(&[(vec![('A', "Aa"), ('A', "e")])]))),
        (vec!['a', 'a'], Ok(RulesSet::from_vec_slice(&[(vec![('A', "Aa"), ('A', "Aa"), ('A', "e")])]))),
        (vec!['a', 'a', 'a'], Ok(RulesSet::from_vec_slice(&[(vec![('A', "Aa"), ('A', "Aa"), ('A', "Aa"), ('A', "e")])]))),
        (vec!['a', 'a', 'a', 'a'], Ok(RulesSet::from_vec_slice(&[(vec![('A', "Aa"), ('A', "Aa"), ('A', "Aa"), ('A', "Aa"), ('A', "e")])]))),
        (vec!['a', 'a', 'a', 'a', 'a'], Ok(RulesSet::from_vec_slice(&[(vec![('A', "Aa"), ('A', "Aa"), ('A', "Aa"), ('A', "Aa"), ('A', "Aa"), ('A', "e")])]))),
    ]
}

#[test]
fn direct_left_recursive_grammar_recog_test() {
    let grammar = common::direct_left_recursive_grammar();
    //     let mut memoize: Memoize = Memoize::new();

    let test_cases = get_test_cases();

    for (input, expected) in test_cases {
        assert!(relational_parsing::g_accepts_string_no_memo(&input, &grammar) == expected.is_ok());
    }
}

#[test]
fn direct_left_recursive_parse_test() {
    let grammar = common::direct_left_recursive_grammar();
    //let mut memoize: Memoize = Memoize::new();

    let test_cases = get_test_cases();
    for (input, expected) in test_cases {
        assert_eq!(relational_parsing::find_parses_no_memo(&input, &grammar), expected);
    }
}
