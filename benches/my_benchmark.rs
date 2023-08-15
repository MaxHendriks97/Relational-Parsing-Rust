use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId, PlotConfiguration, PlottingBackend};

use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{prelude::*, BufReader};

use std::time::{Duration, Instant};

use relational_parsing::{Symbol, Nonterminal, Terminal, Word, Grammar, find_parses_no_memo, GrammarRules};
use relational_parsing::find_parses;


pub fn basic_relational_parsing_example_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a', 'b', 'c']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['S']);
    let start: Nonterminal = 'S';
    let mut grammar_rules: GrammarRules = GrammarRules::new();
    grammar_rules.insert_word_set('S', HashSet::from([
        Word::from(vec![Symbol::Terminal('a')]),
        Word::from(vec![Symbol::Nonterminal('S'), Symbol::Terminal('a')]),
        Word::from(vec![Symbol::Nonterminal('S'), Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')])
    ]));
    Grammar::new(terminals, nonterminals, start, grammar_rules)
}

pub fn e_rule_relational_parsing_example_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a', 'b', 'c']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['S']);
    let start: Nonterminal = 'S';
    let mut grammar_rules: GrammarRules = GrammarRules::new();
    grammar_rules.insert_word_set('S', HashSet::from([
        Word::from(vec![Symbol::Terminal('a')]),
        Word::from(vec![Symbol::Nonterminal('S'), Symbol::Terminal('a')]),
        Word::from(vec![Symbol::Nonterminal('S'), Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')]),
        Word::from(vec![Symbol::Epsilon])
    ]));
    Grammar::new(terminals, nonterminals, start, grammar_rules)
}

pub fn three_rule_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a', 'b']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['S']);
    let start: Nonterminal = 'S';
    let mut grammar_rules: GrammarRules = GrammarRules::new();
    grammar_rules.insert_word_set('S', HashSet::from([
        Word::from(vec![Symbol::Terminal('a'), Symbol::Nonterminal('S'), Symbol::Terminal('b')]),
        Word::from(vec![Symbol::Nonterminal('S'), Symbol::Terminal('a'), Symbol::Terminal('b')]),
        Word::from(vec![Symbol::Terminal('a'), Symbol::Terminal('a'), Symbol::Terminal('a')]),
    ]));
    Grammar::new(terminals, nonterminals, start, grammar_rules)
}

pub fn difficult_bottom_up_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a', '+', '-']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['S', 'E', 'F', 'Q']);
    let start: Nonterminal = 'S';
    let mut grammar_rules: GrammarRules = GrammarRules::new();
    grammar_rules.insert_word_set('S', HashSet::from([Word::from(vec![Symbol::Nonterminal('E')])]));
    grammar_rules.insert_word_set('E', HashSet::from([
        Word::from(vec![Symbol::Nonterminal('E'), Symbol::Nonterminal('Q'), Symbol::Nonterminal('F')]),
        Word::from(vec![Symbol::Nonterminal('F')])
    ]));
    grammar_rules.insert_word_set('F', HashSet::from([Word::from(vec![Symbol::Terminal('a')])]));
    grammar_rules.insert_word_set('Q', HashSet::from([
        Word::from(vec![Symbol::Terminal('+')]),
        Word::from(vec![Symbol::Terminal('-')]),
    ]));
    Grammar::new(terminals, nonterminals, start, grammar_rules)
}

pub fn odd_number_of_a_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['S']);
    let start: Nonterminal = 'S';
    let mut grammar_rules: GrammarRules = GrammarRules::new();
    grammar_rules.insert_word_set('S', HashSet::from([
        Word::from(vec![Symbol::Terminal('a'), Symbol::Nonterminal('S'), Symbol::Terminal('a')]),
        Word::from(vec![Symbol::Terminal('a')])
    ]));
    Grammar::new(terminals, nonterminals, start, grammar_rules)
}

pub fn direct_left_recursive_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['A']);
    let start: Nonterminal = 'A';
    let mut grammar_rules: GrammarRules = GrammarRules::new();
    grammar_rules.insert_word_set('A', HashSet::from([
        Word::from(vec![Symbol::Nonterminal('A'), Symbol::Terminal('a')]),
        Word::from(vec![Symbol::Epsilon]),
    ]));
    Grammar::new(terminals, nonterminals, start, grammar_rules)
}


pub fn direct_right_recursive_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['A']);
    let start: Nonterminal = 'A';
    let mut grammar_rules: GrammarRules = GrammarRules::new();
    grammar_rules.insert_word_set('A', HashSet::from([
        Word::from(vec![Symbol::Terminal('a'), Symbol::Nonterminal('A')]),
        Word::from(vec![Symbol::Terminal('a')]),
    ]));
    Grammar::new(terminals, nonterminals, start, grammar_rules)
}

pub fn indirect_right_recursive_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a', 'b']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['A', 'B']);
    let start: Nonterminal = 'A';
    let mut grammar_rules: GrammarRules = GrammarRules::new();
    grammar_rules.insert_word_set('A', HashSet::from([
        Word::from(vec![Symbol::Terminal('a'), Symbol::Nonterminal('B')]),
        Word::from(vec![Symbol::Terminal('a')]),
    ]));
    grammar_rules.insert_word_set('B', HashSet::from([
        Word::from(vec![Symbol::Terminal('b'), Symbol::Nonterminal('A')]),
        Word::from(vec![Symbol::Terminal('b')]),
    ]));
    Grammar::new(terminals, nonterminals, start, grammar_rules)
}


fn parse_grammars(c: &mut Criterion) {
    let grammars: Vec<(Grammar, &str)> = vec![
        (basic_relational_parsing_example_grammar(), "basic.streams"),
        (e_rule_relational_parsing_example_grammar(), "e_rule.streams"),
        (odd_number_of_a_grammar(), "odd_nr_a.streams"),
        (direct_left_recursive_grammar(), "direct_left.streams"),
        (direct_right_recursive_grammar(), "direct_right.streams"),
        (indirect_right_recursive_grammar(), "indirect_right.streams")
    ];

    for (grammar, stream_file) in grammars.iter() {

        if let Ok(file) = File::open(stream_file) {
            let reader = BufReader::new(file);

            let plot_config = PlotConfiguration::default();

            let mut group = c.benchmark_group(*stream_file);
            group.sampling_mode(criterion::SamplingMode::Flat);
            group.measurement_time(Duration::from_secs(30));
            group.plot_config(plot_config);

            for opt_line in reader.lines() {
                if let Ok(line) = opt_line {
                    let tokens: Vec<char> = line.chars().collect();

                    group.bench_with_input(BenchmarkId::new("Parse", tokens.len()), &tokens, |b, tokens| {
                        b.iter(|| find_parses(tokens, &grammar, &mut relational_parsing::Memoize::new()));
                    });

                    group.bench_with_input(BenchmarkId::new("Parse_no_memo", tokens.len()), &tokens, |b, tokens| {
                        b.iter(|| find_parses_no_memo(tokens, &grammar));
                    });

                }
            }
            group.finish();
        }
    }
}

// fn parse_big_grammar(c: &mut Criterion) {
//     let mut memoize = relational_parsing::Memoize::new();
//     let grammar: Grammar = basic_relational_parsing_example_grammar();

//     println!("populating");
//     if let Ok(file) = File::open("basic.streams") {
//         let reader = BufReader::new(file);
//         for opt_line in reader.lines() {
//             if let Ok(line) = opt_line {
//                 println!("{}", line);
//                 let tokens: Vec<char> = line.chars().collect();

//                 find_parses(&tokens, &grammar, &mut memoize);
//             }
//         }
//     }
//     println!("done populating");

//     let mut group = c.benchmark_group("basic big");
//     if let Ok(file) = File::open("basic.smallstream") {
//         let mut reader = BufReader::new(file);
//         let mut res: String = String::new();

//         if let Ok(_) = reader.read_line(&mut res) {
//             let tokens: Vec<char> = res.chars().collect();

//             let start = Instant::now();
//             find_parses(&tokens, &grammar, &mut memoize);
//             let duration = start.elapsed();

//             println!("Time elapsed is {:?}", duration);

//             let start = Instant::now();
//             find_parses_no_memo(&tokens, &grammar);
//             let duration = start.elapsed();

//             println!("Time elapsed is {:?}", duration);

//             // group.bench_with_input(BenchmarkId::new("Parse", "Big stream"), &tokens, |b, tokens| {
//             //     b.iter(|| find_parses(tokens, &grammar, &mut memoize));
//             // });

//             // group.bench_with_input(BenchmarkId::new("Parse_no_memo", "Big stream"), &tokens, |b, tokens| {
//             //     b.iter(|| find_parses_no_memo(tokens, &grammar));
//             // });
//         }
//     }

// }

pub fn plotting_criterion() -> Criterion {
    Criterion::default().with_plots()
}

criterion_group!{
    name = benches;
    config = plotting_criterion();
    targets = parse_grammars
}
criterion_main!(benches);
