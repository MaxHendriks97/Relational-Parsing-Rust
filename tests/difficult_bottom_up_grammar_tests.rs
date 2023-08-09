use relational_parsing::{
    self,
    RulesSet, Terminal, ParseError, Memoize
};

mod common;

fn get_test_cases() -> [(Vec<Terminal>, Result<RulesSet, ParseError>); 12] {
    [
        (vec!['a'], Ok(RulesSet::from_vec_slice(&[(vec![('S', "E"), ('E', "F"), ('F', "a")])]))),
        (vec!['a', '-', 'a'], Ok(RulesSet::from_vec_slice(&[(vec![('E', "EQF"), ('F', "a"), ('Q', "-"), ('S', "E"), ('E', "F"), ('F', "a")])]))),
        (vec!['a', '-', 'a', '+', 'a'], Ok(RulesSet::from_vec_slice(&[(vec![('E', "EQF"), ('F', "a"), ('Q', "+"), ('E', "EQF"), ('F', "a"), ('Q', "-"), ('S', "E"), ('E', "F"), ('F', "a")])]))),
        (vec!['a', '+', 'a', '-', 'a'], Ok(RulesSet::from_vec_slice(&[(vec![('E', "EQF"), ('F', "a"), ('Q', "-"), ('E', "EQF"), ('F', "a"), ('Q', "+"), ('S', "E"), ('E', "F"), ('F', "a")])]))),
        (vec!['a', '-', 'a', '+', 'a', '-', 'a'], Ok(RulesSet::from_vec_slice(&[(vec![('E', "EQF"), ('F', "a"), ('Q', "-"), ('E', "EQF"), ('F', "a"), ('Q', "+"), ('E', "EQF"), ('F', "a"), ('Q', "-"), ('S', "E"), ('E', "F"), ('F', "a")])]))),

        (vec!['a', 'a'], Err(ParseError)),
        (vec!['a', 'a', 'a'], Err(ParseError)),
        (vec!['-', '-'], Err(ParseError)),
        (vec!['-', '-', 'a'], Err(ParseError)),
        (vec!['a', '-', 'a', '+', '+'], Err(ParseError)),
        (vec!['a', '-', 'a', 'a', 'a'], Err(ParseError)),
        (vec!['a', '-', 'a', 'a', 'a'], Err(ParseError)),
    ]
}

#[test]
fn memo_recog() {
    let grammar = common::difficult_bottom_up_grammar();
    let test_cases = get_test_cases();

    for (input, expected) in test_cases {
        println!("input: {:?}", input);
        assert!(relational_parsing::g_accepts_string(&input, &grammar, &mut Memoize::new()) == expected.is_ok());
    }
}

#[test]
fn memo_parse() {
    let grammar = common::difficult_bottom_up_grammar();
    let test_cases = get_test_cases();

    for (input, expected) in test_cases {
        assert_eq!(relational_parsing::find_parses(&input, &grammar, &mut Memoize::new()), expected);
    }
}

#[test]
fn difficult_bottom_up_grammar_recog_test() {
    let grammar = common::difficult_bottom_up_grammar();
//     let mut memoize: Memoize = Memoize::new();

    let test_cases = get_test_cases();

    for (input, expected) in test_cases {
        assert!(relational_parsing::g_accepts_string_no_memo(&input, &grammar) == expected.is_ok());
    }

}


#[test]
fn difficult_bottom_up_grammar_parse_test() {
    let grammar = common::difficult_bottom_up_grammar();
    grammar.finite_state_automaton.to_dot("test");
    //let mut memoize: Memoize = Memoize::new();

    let test_cases = get_test_cases();
    for (input, expected) in test_cases {
        assert_eq!(relational_parsing::find_parses_no_memo(&input, &grammar), expected);
    }
}
