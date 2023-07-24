use relational_parsing::{
    RulesSet, Rules, Terminal, ParseError
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
//     let mut memoize: Memoize = Memoize::new();

//     assert!(relational_parsing::g_accepts_string(&vec!['a'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['a', 'a'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'a'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'c'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'c', 'a'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'a', 'c'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize));
//     assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'b', 'a', 'b', 'a', 'c', 'c', 'c'], &grammar, &mut memoize));

//     assert!(!relational_parsing::g_accepts_string(&vec![], &grammar, &mut memoize));
//     assert!(!relational_parsing::g_accepts_string(&vec!['a', 'a', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize));
//     assert!(!relational_parsing::g_accepts_string(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c'], &grammar, &mut memoize));
//     assert!(!relational_parsing::g_accepts_string(&vec!['b', 'c', 'b', 'c', 'b', 'c', 'a'], &grammar, &mut memoize));
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
//     let mut memoize: Memoize = Memoize::new();

//     assert_eq!(relational_parsing::find_parses(&vec!['a', 'b', 'a', 'b', 'a', 'b', 'a', 'c', 'c', 'c'], &grammar, &mut memoize),
//         Ok(HashSet::from([
//             build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "a"), ('S', "a"), ('S', "a")])
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses(&vec!['a'], &grammar, &mut memoize),
//         Ok(HashSet::from([
//             build_rules(&vec![('S', "a")])
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses(&vec!['a', 'a'], &grammar, &mut memoize),
//         Ok(HashSet::from([
//             build_rules(&vec![('S', "Sa"), ('S', "a")])
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a'], &grammar, &mut memoize),
//         Ok(HashSet::from([
//             build_rules(&vec![('S', "Sa"), ('S', "Sa"), ('S', "a")])
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses(&vec!['a', 'b', 'a', 'c'], &grammar, &mut memoize),
//         Ok(HashSet::from([
//             build_rules(&vec![('S', "SbSc"), ('S', "a"), ('S', "a")])
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses(&vec!['a', 'b', 'a', 'c', 'a'], &grammar, &mut memoize),
//         Ok(HashSet::from([
//             build_rules(&vec![('S', "Sa"), ('S', "SbSc"), ('S', "a"), ('S', "a")])
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses(&vec!['a', 'b', 'a', 'a', 'c'], &grammar, &mut memoize),
//         Ok(HashSet::from([
//             build_rules(&vec![('S', "SbSc"), ('S', "Sa"), ('S', "a"), ('S', "a")])
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses(&vec!['a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize),
//         Ok(HashSet::from([
//             build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "a"), ('S', "a")])
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize),
//         Ok(HashSet::from([
//             build_rules(&vec![('S', "SbSc"), ('S', "SbSc"), ('S', "a"), ('S', "a"), ('S', "Sa"), ('S', "a")])
//         ]))
//     );

//     assert_eq!(relational_parsing::find_parses(&vec!['a', 'a', 'a', 'a', 'a', 'a', 'a', 'a'], &grammar, &mut memoize),
//         Ok(HashSet::from([
//             build_rules(&vec![('S', "Sa"), ('S', "Sa"), ('S', "Sa"), ('S', "Sa"), ('S', "Sa"), ('S', "Sa"), ('S', "Sa"), ('S', "a")])
//         ]))
//     );

//     assert!(relational_parsing::find_parses(&vec![], &grammar, &mut memoize).is_err());
//     assert!(relational_parsing::find_parses(&vec!['a', 'a', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize).is_err());
//     assert!(relational_parsing::find_parses(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c'], &grammar, &mut memoize).is_err());
//     assert!(relational_parsing::find_parses(&vec!['b', 'c', 'b', 'c', 'b', 'c', 'a'], &grammar, &mut memoize).is_err());
}

#[test]
fn no_memo_parse() {
    let grammar = common::basic_relational_parsing_example_grammar();
    let test_cases = get_test_cases();

    for (input, expected) in test_cases {
        assert_eq!(relational_parsing::find_parses_no_memo(&input, &grammar), expected);
    }
}
