//use relational_parsing::{Grammar, TokenStreams, generate_tokens, big_stream_generator};
//mod common;
//
//#[test]
//fn test_gen() {
//    let grammar: Grammar = common::difficult_bottom_up_grammar();
//    let streams: TokenStreams = generate_tokens(&grammar,  10, 10);
//
//    println!("{}", streams);
//    // if let Err(e) = streams.to_file("e_rule.streams") {
//    //     println!("{}", e);
//    // }
//}
//
//#[test]
//fn gen_all_grammars() {
//    let grammars: Vec<(Grammar, &str)> = vec![
//        (common::basic_relational_parsing_example_grammar(), "basic.streams"),
//        // (common::e_rule_relational_parsing_example_grammar(), "e_rule.streams"),
//        // (common::odd_number_of_a_grammar(), "odd_nr_a.streams"),
//        (common::direct_left_recursive_grammar(), "direct_left.streams"),
//        (common::indirect_left_recursive_grammar(), "indirect_left.streams"),
//        (common::direct_right_recursive_grammar(), "direct_right.streams"),
//        (common::indirect_right_recursive_grammar(), "indirect_right.streams")
//    ];
//
//    for (grammar, name) in grammars {
//        println!("Generating {}", name);
//        let streams: TokenStreams = generate_tokens(&grammar, 10, 10);
//        if let Err(e) = streams.to_file(name) {
//            println!("{}", e);
//        }
//    }
//}
//
//#[test]
//fn test_big_gen() {
//    let grammar: Grammar = common::basic_relational_parsing_example_grammar();
//    big_stream_generator(&grammar, 25000, "basic.smallstream");
//    //big_stream_generator(&grammar, 50000, "basic.midstream");
//    //big_stream_generator(&grammar, 100000, "basic.bigstream");
//}
