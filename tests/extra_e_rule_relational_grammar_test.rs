use relational_parsing::{self, Memoize};

mod common;

#[test]
fn extra_e_rule_relational_grammar_recog_test() {
    let grammar = common::extra_e_rule_relational_parsing_example_grammar();
    let mut memoize: Memoize = Memoize::new();

    assert!(relational_parsing::g_accepts_string(&vec![], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'c'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'c', 'a'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'a', 'c'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize));
    assert!(relational_parsing::g_accepts_string(&vec!['a', 'b', 'c', 'b', 'c', 'b', 'c', 'a'], &grammar, &mut memoize));

    assert!(!relational_parsing::g_accepts_string(&vec!['a', 'a', 'a', 'b', 'a', 'c', 'c'], &grammar, &mut memoize));
    assert!(!relational_parsing::g_accepts_string(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c'], &grammar, &mut memoize));

    assert!(relational_parsing::g_accepts_string_no_memo(&vec![], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'b', 'a', 'c'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'b', 'a', 'c', 'a'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'b', 'a', 'a', 'c'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c', 'c'], &grammar));
    assert!(relational_parsing::g_accepts_string_no_memo(&vec!['a', 'b', 'c', 'b', 'c', 'b', 'c', 'a'], &grammar));

    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'a', 'b', 'a', 'c', 'c'], &grammar));
    assert!(!relational_parsing::g_accepts_string_no_memo(&vec!['a', 'a', 'b', 'a', 'b', 'a', 'c'], &grammar));
}
