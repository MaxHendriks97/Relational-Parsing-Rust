use relational_parsing::{
    self,
    RulesSet, Terminal, ParseError
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

//     let mut memoize: Memoize = Memoize::new();

//     assert!(relational_parsing::g_accepts_string(&vec![], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['a'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['a', 'a'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'a'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'c'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'c', 'a'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'a', 'c'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['b', 'b', 'b', 'b', 'c', 'c', 'c', 'c'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['b', 'c', 'b', 'c', 'b', 'c', 'a'], &grammar, &mut memoize));

//     assert!(!relational_parsing::g_accepts_string(&vec!['a', 'a', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize));
//     assert!(!relational_parsing::g_accepts_string(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c'], &grammar, &mut memoize));

    for (input, expected) in test_cases {
        assert!(relational_parsing::g_accepts_string_no_memo(&input, &grammar) == expected.is_ok());
    }
}

 #[test]
 fn e_rule_relational_grammar_parse_test() {
     let grammar = common::e_rule_relational_parsing_example_grammar();
     let test_cases = get_test_cases();

     for (input, expected) in test_cases {
         assert_eq!(relational_parsing::find_parses_no_memo(&input, &grammar), expected);
     }
//     let mut memoize: Memoize = Memoize::new();

//     let tokens: Vec<char> = "babcbbabacbbacbbabbbabacbcbabababbbaababbbbabbabbabaaaaaacaccaaacaaaacacccacacacaaccaacacacaaaaaacccaaccaaaccccbacabacaaa".chars().collect();

//     println!("{:?}", relational_parsing::find_parses(&tokens, &grammar, &mut memoize))

//     // assert_eq!(relational_parsing::find_parses(&vec!['a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize),
//     //     Ok(HashSet::from([
//     //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "a"), ('S', "a")]),
//     //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "a"), ('S', "Sa"), ('S', "e")]),
//     //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "Sa"), ('S', "e"), ('S', "a")]),
//     //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "a"), ('S', "a")]),
//     //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e")]),
//     //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "a"), ('S', "Sa"), ('S', "e")]),
//     //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e"), ('S', "a")]),
//     //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e")]),
//     //     ]))
//     // );

//     // assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize),
//     //     Ok(HashSet::from([
//     //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "a"), ('S', "Sa"), ('S', "a")]),
//     //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "a"), ('S', "Sa"), ('S', "Sa"), ('S', "e")]),
//     //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "a")]),
//     //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "a"), ('S', "Sa"), ('S', "a")]),
//     //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "Sa"), ('S', "e")]),
//     //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "a"), ('S', "Sa"), ('S', "Sa"), ('S', "e")]),
//     //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "a")]),
//     //         build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "Sa"), ('S', "e")]),
//     //     ]))
//     // );

//     // assert_eq!(relational_parsing::find_parses(&vec![], &grammar, &mut memoize),
//     //     Ok(HashSet::from([
//     //         build_rules(&vec![('S', "e")])
//     //     ]))
//     // );

//     // assert_eq!(relational_parsing::find_parses(&vec!['a'], &grammar, &mut memoize),
//     //     Ok(HashSet::from([
//     //         build_rules(&vec![('S', "a")]),
//     //         build_rules(&vec![('S', "Sa"), ('S', "e")])
//     //     ]))
//     // );

//     // assert_eq!(relational_parsing::find_parses(&vec!['a', 'a'], &grammar, &mut memoize),
//     //     Ok(HashSet::from([
//     //         build_rules(&vec![('S', "Sa"), ('S', "a")]),
//     //         build_rules(&vec![('S', "Sa"), ('S', "Sa"), ('S', "e")])
//     //     ]))
//     // );

//     // assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a'], &grammar, &mut memoize),
//     //     Ok(HashSet::from([
//     //         build_rules(&vec![('S', "Sa"), ('S', "Sa"), ('S', "a")]),
//     //         build_rules(&vec![('S', "Sa"), ('S', "Sa"), ('S', "Sa"), ('S', "e")])
//     //     ]))
//     // );

//     // assert_eq!(relational_parsing::find_parses(&vec!['a', 'b', 'a', 'c'], &grammar, &mut memoize),
//     //     Ok(HashSet::from([
//     //         build_rules(&vec![('S', "SbSc"), ('S', "a"), ('S', "a")]),
//     //         build_rules(&vec![('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "a")]),
//     //         build_rules(&vec![('S', "SbSc"), ('S', "a"), ('S', "Sa"), ('S', "e")]),
//     //         build_rules(&vec![('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e")])
//     //     ]))
//     // );

//     // assert_eq!(relational_parsing::find_parses(&vec!['a', 'b', 'a', 'c', 'a'], &grammar, &mut memoize),
//     //     Ok(HashSet::from([
//     //         build_rules(&vec![('S', "Sa"), ('S', "SbSc"), ('S', "a"), ('S', "a")]),
//     //         build_rules(&vec![('S', "Sa"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "a")]),
//     //         build_rules(&vec![('S', "Sa"), ('S', "SbSc"), ('S', "a"), ('S', "Sa"), ('S', "e")]),
//     //         build_rules(&vec![('S', "Sa"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e")])
//     //     ]))
//     // );

//     // assert_eq!(relational_parsing::find_parses(&vec!['a', 'b', 'a', 'a', 'c'], &grammar, &mut memoize),
//     //     Ok(HashSet::from([
//     //         build_rules(&vec![('S', "SbSc"), ('S', "Sa"), ('S', "a"), ('S', "a")]),
//     //         build_rules(&vec![('S', "SbSc"), ('S', "Sa"), ('S', "Sa"), ('S', "e"), ('S', "a")]),
//     //         build_rules(&vec![('S', "SbSc"), ('S', "Sa"), ('S', "a"), ('S', "Sa"), ('S', "e")]),
//     //         build_rules(&vec![('S', "SbSc"), ('S', "Sa"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e")])
//     //     ]))
//     // );


//     // assert_eq!(relational_parsing::find_parses(&vec!['b', 'b', 'b', 'b', 'c', 'c', 'c', 'c'], &grammar, &mut memoize),
//     //     Ok(HashSet::from([
//     //         build_rules(&vec![('S', "SbSc"), ('S', "e"), ('S', "SbSc"), ('S', "e"), ('S', "SbSc"), ('S', "e"), ('S', "SbSc"), ('S', "e"), ('S', "e")]),
//     //     ]))
//     // );

//     // assert_eq!(relational_parsing::find_parses(&vec!['b', 'c', 'b', 'c', 'b', 'c', 'a'], &grammar, &mut memoize),
//     //     Ok(HashSet::from([
//     //         build_rules(&vec![('S', "Sa"), ('S', "SbSc"), ('S', "e"), ('S', "SbSc"), ('S', "e"), ('S', "SbSc"), ('S', "e"), ('S', "e")]),
//     //     ]))
//     // );

//     // assert!(relational_parsing::find_parses(&vec!['a', 'a', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize).is_err());
//     // assert!(relational_parsing::find_parses(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c'], &grammar, &mut memoize).is_err());


}
