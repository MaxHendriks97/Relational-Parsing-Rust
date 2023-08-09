use relational_parsing::{
    RulesSet, Rules, Terminal, ParseError, Memoize
};

mod common;


fn get_test_cases() -> [(Vec<Terminal>, Result<RulesSet, ParseError>); 13] {
    [
        (vec!['a'], Ok(RulesSet::from_vec_slice(&[(vec![('S', "a")])]))),
        (vec!['a', 'a'], Ok(RulesSet::from_vec_slice(&[(vec![('S', "Sa"), ('S', "a")])]))),
        (vec!['a', 'a', 'a'], Ok(RulesSet::from_vec_slice(&[(vec![('S', "Sa"), ('S', "Sa"), ('S', "a")])]))),
        (vec!['a', 'b', 'a', 'c'], Ok(RulesSet::from_vec_slice(&[(vec![('S', "SbSc"), ('S', "a"), ('S', "a")])]))),
        (vec!['a', 'b', 'a', 'c', 'a'], Ok(RulesSet::from_vec_slice(&[(vec![('S', "Sa"), ('S', "SbSc"), ('S', "a"), ('S', "a")])]))),
        (vec!['a', 'b', 'a', 'a', 'c'], Ok(RulesSet::from_vec_slice(&[(vec![('S', "SbSc"), ('S', "Sa"), ('S', "a"), ('S', "a")])]))),
        (vec!['a', 'b', 'a', 'b', 'a', 'c', 'c'], Ok(RulesSet::from_vec_slice(&[(vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "a"), ('S', "a")])]))),
        (vec!['a', 'a', 'b', 'a', 'b', 'a', 'c', 'c'], Ok(RulesSet::from_vec_slice(&[(vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "a"), ('S', "Sa"), ('S', "a")])]))),
        (vec!['a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], Ok(RulesSet::from_vec_slice(&[(vec![('S', "Sa"), ('S', "Sa"), ('S', "Sa"), ('S', "Sa"), ('S', "Sa"), ('S', "Sa"), ('S', "Sa"), ('S', "a")])]))),
        (vec!['a', 'b', 'a', 'b', 'a', 'b', 'a', 'c', 'c', 'c'], Ok(RulesSet::from_rules(Rules::from_string_vec(vec![('S', "SbSc"), ('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "a"), ('S', "a"), ('S', "a")])))),
        (vec![], Err(ParseError)),
        (vec!['a', 'a', 'a', 'b', 'a', 'c', 'c'], Err(ParseError)),
        (vec!['a', 'a', 'b', 'a', 'b', 'a', 'c'], Err(ParseError)),
    ]
}

#[test]
fn memo_reg() {
    let grammar = common::basic_relational_parsing_example_grammar();

    for (input, expected) in get_test_cases() {
        assert!(relational_parsing::g_accepts_string(&input, &grammar, &mut Memoize::new()) == expected.is_ok());
    }
}

#[test]
fn no_memo_reg() {
    let grammar = common::basic_relational_parsing_example_grammar();
    let test_cases = get_test_cases();

    for (input, expected) in test_cases {
        assert!(relational_parsing::g_accepts_string_no_memo(&input, &grammar) == expected.is_ok());
    }
}

#[test]
fn memo_parse() {
    let grammar = common::basic_relational_parsing_example_grammar();
    let test_cases = get_test_cases();

    for (input, expected) in test_cases {
        assert!(relational_parsing::g_accepts_string(&input, &grammar, &mut Memoize::new()) == expected.is_ok());
    }
}

#[test]
fn no_memo_parse() {
    let grammar = common::basic_relational_parsing_example_grammar();
    let test_cases = get_test_cases();

    for (input, expected) in test_cases {
        assert_eq!(relational_parsing::find_parses_no_memo(&input, &grammar), expected);
    }
}
