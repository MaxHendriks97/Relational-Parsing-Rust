use relational_parsing::{
    self,
    RulesSet, Terminal, ParseError
};

mod common;

fn get_test_cases() -> [(Vec<Terminal>, Result<RulesSet, ParseError>); 7] {
    [
        (vec!['a'], Ok(RulesSet::from_vec_slice(&[(vec![('S', "a")])]))),
        (vec!['a', 'a', 'a'], Ok(RulesSet::from_vec_slice(&[(vec![('S', "aSa"), ('S', "a")])]))),
        (vec!['a', 'a', 'a', 'a', 'a'], Ok(RulesSet::from_vec_slice(&[(vec![('S', "aSa"), ('S', "aSa"), ('S', "a")])]))),
        (vec!['a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], Ok(RulesSet::from_vec_slice(&[(vec![('S', "aSa"), ('S', "aSa"), ('S', "aSa"), ('S', "aSa"), ('S', "aSa"), ('S', "a")])]))),

        (vec!['a', 'a'], Err(ParseError)),
        (vec!['a', 'a', 'a', 'a'], Err(ParseError)),
        (vec!['a', 'a', 'a', 'a', 'a', 'a'], Err(ParseError)),
    ]
}

#[test]
fn odd_nr_of_a_grammar_recog_test() {
    let grammar = common::odd_number_of_a_grammar();
    //     let mut memoize: Memoize = Memoize::new();

    let test_cases = get_test_cases();

    for (input, expected) in test_cases {
        assert!(relational_parsing::g_accepts_string_no_memo(&input, &grammar) == expected.is_ok());
    }

}


#[test]
fn odd_nr_of_a_parse_test() {
    let grammar = common::odd_number_of_a_grammar();
    //let mut memoize: Memoize = Memoize::new();

    let test_cases = get_test_cases();
    for (input, expected) in test_cases {
        assert_eq!(relational_parsing::find_parses_no_memo(&input, &grammar), expected);
    }
}
