use relational_parsing::{
    self,
    Regex, RulesSet, Rules, Terminal, ParseError
};

mod common;

fn get_inputs() -> ([Vec<Terminal>; 11], [Vec<Terminal>; 2]) {
    ([
        vec![],
        vec!['a'],
        vec!['a', 'a'],
        vec!['a', 'a', 'a'],
        vec!['a', 'b', 'a', 'c'],
        vec!['a', 'b', 'a', 'c', 'a'],
        vec!['a', 'b', 'a', 'a', 'c'],
        vec!['a', 'b', 'a', 'b', 'a', 'c', 'c'],
        vec!['a', 'a', 'b', 'a', 'b', 'a', 'c', 'c'],
        vec!['b', 'b', 'b', 'b', 'c', 'c', 'c', 'c'],
        vec!['b', 'c', 'b', 'c', 'b', 'c', 'a'],
    ],
    [
        vec!['a', 'a', 'a', 'b', 'a', 'c', 'c'],
        vec!['a', 'a', 'b', 'a', 'b', 'a', 'c'],
    ])
}

fn get_parses() -> [Result<RulesSet, ParseError>; 11] {
    [
        Ok(RulesSet::from_vec_slice(&[vec![('S', "e")]])),
        Ok(RulesSet::from_vec_slice(&[
           (vec![('S', "a")]),
           (vec![('S', "Sa"), ('S', "e")])
        ])),
        Ok(RulesSet::from_vec_slice(&[
           (vec![('S', "Sa"), ('S', "a")]),
           (vec![('S', "Sa"), ('S', "Sa"), ('S', "e")])
        ])),
        Ok(RulesSet::from_vec_slice(&[
           (vec![('S', "Sa"), ('S', "Sa"), ('S', "a")]),
           (vec![('S', "Sa"), ('S', "Sa"), ('S', "Sa"), ('S', "e")])
        ])),
        Ok(RulesSet::from_vec_slice(&[
           (vec![('S', "SbSc"), ('S', "a"), ('S', "a")]),
           (vec![('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "a")]),
           (vec![('S', "SbSc"), ('S', "a"), ('S', "Sa"), ('S', "e")]),
           (vec![('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e")])
        ])),
        Ok(RulesSet::from_vec_slice(&[
           (vec![('S', "Sa"), ('S', "SbSc"), ('S', "a"), ('S', "a")]),
           (vec![('S', "Sa"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "a")]),
           (vec![('S', "Sa"), ('S', "SbSc"), ('S', "a"), ('S', "Sa"), ('S', "e")]),
           (vec![('S', "Sa"), ('S', "SbSc"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e")])
        ])),
        Ok(RulesSet::from_vec_slice(&[
           (vec![('S', "SbSc"), ('S', "Sa"), ('S', "a"), ('S', "a")]),
           (vec![('S', "SbSc"), ('S', "Sa"), ('S', "Sa"), ('S', "e"), ('S', "a")]),
           (vec![('S', "SbSc"), ('S', "Sa"), ('S', "a"), ('S', "Sa"), ('S', "e")]),
           (vec![('S', "SbSc"), ('S', "Sa"), ('S', "Sa"), ('S', "e"), ('S', "Sa"), ('S', "e")])
        ])),
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
        Ok(RulesSet::from_vec_slice(&[
           (vec![('S', "SbSc"), ('S', "e"), ('S', "SbSc"), ('S', "e"), ('S', "SbSc"), ('S', "e"), ('S', "SbSc"), ('S', "e"), ('S', "e")]),
        ])),
        Ok(RulesSet::from_vec_slice(&[
           (vec![('S', "Sa"), ('S', "SbSc"), ('S', "e"), ('S', "SbSc"), ('S', "e"), ('S', "SbSc"), ('S', "e"), ('S', "e")]),
        ]))
    ]
}

#[test]
fn e_rule_relational_grammar_recog_test() {
    let grammar = common::e_rule_relational_parsing_example_grammar();
    let (accepted_inputs, rejected_inputs) = get_inputs();
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

    for input in accepted_inputs.iter() {
        assert!(relational_parsing::g_accepts_string_no_memo(input, &grammar));
    }

    for input in rejected_inputs.iter() {
        assert!(!relational_parsing::g_accepts_string_no_memo(input, &grammar));
    }
}

 #[test]
 fn e_rule_relational_grammar_parse_test() {
     let grammar = common::e_rule_relational_parsing_example_grammar();
     let (accepted_inputs, rejected_inputs) = get_inputs();
     let accepted_parses = get_parses();
     for (input, parse) in accepted_inputs.iter().zip(accepted_parses) {
         println!("input: {:?}", input);
         let res = relational_parsing::find_parses_no_memo(input, &grammar);
         println!("parse: {}", parse.as_ref().unwrap());
         println!("res: {}", res.as_ref().unwrap());
         assert_eq!(res, parse);
     }

     for input in rejected_inputs {
         println!("input: {:?}", input);
         assert!(relational_parsing::find_parses_no_memo(&input, &grammar).is_err());
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
