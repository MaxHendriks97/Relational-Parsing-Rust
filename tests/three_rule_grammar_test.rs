use relational_parsing::{
    self,
    RulesSet, Terminal, ParseError, Memoize
};

mod common;

fn get_test_cases() -> [(Vec<Terminal>, Result<RulesSet, ParseError>); 9] {
    [
        (vec!['a', 'a', 'a'],
        Ok(RulesSet::from_vec_slice(&[
            (vec![('S', "aaa")])
        ]))
        ),
        (vec!['a', 'a', 'a', 'a', 'b'],
        Ok(RulesSet::from_vec_slice(&[
            (vec![('S', "aSb"), ('S', "aaa")]),
            (vec![('S', "Sab"), ('S', "aaa")])
        ]))
        ),
        (vec!['a', 'a', 'a', 'a', 'b', 'a', 'b'],
        Ok(RulesSet::from_vec_slice(&[
            (vec![('S', "Sab"), ('S', "aSb"), ('S', "aaa")]),
            (vec![('S', "Sab"), ('S', "Sab"), ('S', "aaa")])
        ]))
        ),
        (vec!['a', 'a', 'a', 'a', 'a', 'b', 'b'],
        Ok(RulesSet::from_vec_slice(&[
            (vec![('S', "aSb"), ('S', "aSb"), ('S', "aaa")]),
            (vec![('S', "aSb"), ('S', "Sab"), ('S', "aaa")])
        ]))
        ),
        (vec!['a', 'a', 'a', 'a', 'a', 'b', 'b', 'a', 'b'],
        Ok(RulesSet::from_vec_slice(&[
            (vec![('S', "Sab"), ('S', "aSb"), ('S', "aSb"), ('S', "aaa")]),
            (vec![('S', "Sab"), ('S', "aSb"), ('S', "Sab"), ('S', "aaa")])
        ]))
        ),
        (vec!['a', 'a', 'a', 'a', 'b', 'a', 'b', 'a', 'b'],
        Ok(RulesSet::from_vec_slice(&[
            (vec![('S', "Sab"), ('S', "Sab"), ('S', "aSb"), ('S', "aaa")]),
            (vec![('S', "Sab"), ('S', "Sab"), ('S', "Sab"), ('S', "aaa")])
        ]))
        ),

        (vec!['a', 'a', 'a', 'a', 'a', 'b'], Err(ParseError)),
        (vec!['a', 'a', 'a', 'a', 'b', 'b'], Err(ParseError)),
        (vec!['a', 'a', 'a', 'a', 'a'], Err(ParseError)),
    ]
}

#[test]
fn memo_recog() {
    let grammar = common::three_rule_grammar();
    let test_cases = get_test_cases();

    for (input, expected) in test_cases {
        assert!(relational_parsing::g_accepts_string(&input, &grammar, &mut Memoize::new()) == expected.is_ok());
    }
}

#[test]
fn memo_parse() {
    let grammar = common::three_rule_grammar();
    let test_cases = get_test_cases();

    for (input, expected) in test_cases {
        assert_eq!(relational_parsing::find_parses(&input, &grammar, &mut Memoize::new()), expected);
    }
}

#[test]
fn three_rule_grammar_recog_test() {
    let grammar = common::three_rule_grammar();
    let test_cases = get_test_cases();

    for (input, expected) in test_cases {
        assert!(relational_parsing::g_accepts_string_no_memo(&input, &grammar) == expected.is_ok());
    }
}

 #[test]
 fn three_rule_grammar_parse_test() {
     let grammar = common::three_rule_grammar();
     let test_cases = get_test_cases();

     for (input, expected) in test_cases {
         assert_eq!(relational_parsing::find_parses_no_memo(&input, &grammar), expected);
     }

 }
