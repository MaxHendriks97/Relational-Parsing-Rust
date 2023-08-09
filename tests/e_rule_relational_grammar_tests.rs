use relational_parsing::{
    self,
    RulesSet, Terminal, ParseError, Memoize
};

mod common;

fn get_test_cases() -> [(Vec<Terminal>, Result<RulesSet, ParseError>); 13] {
    [
        (vec![], Ok(RulesSet::from_vec_slice(&[vec![('S', "e")]]))),
        (vec!['a'],
         Ok(RulesSet::from_vec_slice(&[
             (vec![('S', "a")]),
             (vec![('S', "Sa"), ('S', "e")])
         ]))
        ),
        (vec!['a', 'a'],
         Ok(RulesSet::from_vec_slice(&[
             (vec![('S', "Sa"), ('S', "a")]),
             (vec![('S', "Sa"), ('S', "Sa"), ('S', "e")])
         ])),
        ),
        (vec!['a', 'a', 'a'],
         Ok(RulesSet::from_vec_slice(&[
             (vec![('S', "Sa"), ('S', "Sa"), ('S', "a")]),
             (vec![('S', "Sa"), ('S', "Sa"), ('S', "Sa"), ('S', "e")])
         ])),
        ),
        (vec!['a', 'b', 'a', 'c'],
         Ok(RulesSet::from_vec_slice(&[
             (vec![('S', "SbSc"), ('S', "a"), ('S', "a")]),
             (vec![('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "a")]),
             (vec![('S', "SbSc"), ('S', "a"), ('S', "Sa"), ('S', "e")]),
             (vec![('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e")])
         ])),
        ),
        (vec!['a', 'b', 'a', 'c', 'a'],
         Ok(RulesSet::from_vec_slice(&[
             (vec![('S', "Sa"), ('S', "SbSc"), ('S', "a"), ('S', "a")]),
             (vec![('S', "Sa"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "a")]),
             (vec![('S', "Sa"), ('S', "SbSc"), ('S', "a"), ('S', "Sa"), ('S', "e")]),
             (vec![('S', "Sa"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e")])
         ])),
        ),
        (vec!['a', 'b', 'a', 'a', 'c'],
         Ok(RulesSet::from_vec_slice(&[
             (vec![('S', "SbSc"), ('S', "Sa"), ('S', "a"), ('S', "a")]),
             (vec![('S', "SbSc"), ('S', "Sa"), ('S', "Sa"), ('S', "e"), ('S', "a")]),
             (vec![('S', "SbSc"), ('S', "Sa"), ('S', "a"), ('S', "Sa"), ('S', "e")]),
             (vec![('S', "SbSc"), ('S', "Sa"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e")])
         ])),
        ),
        (vec!['a', 'b', 'a', 'b', 'a', 'c', 'c'],
         Ok(RulesSet::from_vec_slice(&[
             (vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "a"), ('S', "a")]),
             (vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "a"), ('S', "Sa"), ('S', "e")]),
             (vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "Sa"), ('S', "e"), ('S', "a")]),
             (vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "a"), ('S', "a")]),
             (vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e")]),
             (vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "a"), ('S', "Sa"), ('S', "e")]),
             (vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e"), ('S', "a")]),
             (vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e")]),
         ])),
        ),
        (vec!['a', 'a', 'b', 'a', 'b', 'a', 'c', 'c'],
         Ok(RulesSet::from_vec_slice(&[
             (vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "a"), ('S', "Sa"), ('S', "a")]),
             (vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "a"), ('S', "Sa"), ('S', "Sa"), ('S', "e")]),
             (vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "a")]),
             (vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "a"), ('S', "Sa"), ('S', "a")]),
             (vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "Sa"), ('S', "e")]),
             (vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "a"), ('S', "Sa"), ('S', "Sa"), ('S', "e")]),
             (vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "a")]),
             (vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "Sa"), ('S', "e")]),
         ])),
        ),
        (vec!['b', 'b', 'b', 'b', 'c', 'c', 'c', 'c'],
         Ok(RulesSet::from_vec_slice(&[
             (vec![('S', "SbSc"), ('S', "e"), ('S', "SbSc"), ('S', "e"), ('S', "SbSc"), ('S', "e"), ('S', "SbSc"), ('S', "e"), ('S', "e")]),
         ])),
        ),
        (vec!['b', 'c', 'b', 'c', 'b', 'c', 'a'],
         Ok(RulesSet::from_vec_slice(&[
             (vec![('S', "Sa"), ('S', "SbSc"), ('S', "e"), ('S', "SbSc"), ('S', "e"), ('S', "SbSc"), ('S', "e"), ('S', "e")]),
         ]))
        ),
        (vec!['a', 'a', 'a', 'b', 'a', 'c', 'c'], Err(ParseError)),
        (vec!['a', 'a', 'b', 'a', 'b', 'a', 'c'], Err(ParseError)),
    ]
}

#[test]
fn e_rule_relational_grammar_recog_test() {
    let grammar = common::e_rule_relational_parsing_example_grammar();
    let test_cases = get_test_cases();

    for (input, expected) in test_cases {
        assert!(relational_parsing::g_accepts_string_no_memo(&input, &grammar) == expected.is_ok());
    }
}

#[test]
fn memo_recog() {
    let grammar = common::e_rule_relational_parsing_example_grammar();
    let test_cases = get_test_cases();

    for (input, expected) in test_cases {
        assert!(relational_parsing::g_accepts_string(&input, &grammar, &mut Memoize::new()) == expected.is_ok());
    }
}

#[test]
fn memo_parse() {
    let grammar = common::e_rule_relational_parsing_example_grammar();
    let test_cases = get_test_cases();

    for (input, expected) in test_cases {
        assert_eq!(relational_parsing::find_parses(&input, &grammar, &mut Memoize::new()), expected);
    }
}

 #[test]
 fn e_rule_relational_grammar_parse_test() {
     let grammar = common::e_rule_relational_parsing_example_grammar();
     let test_cases = get_test_cases();

     for (input, expected) in test_cases {
         assert_eq!(relational_parsing::find_parses_no_memo(&input, &grammar), expected);
     }
}
