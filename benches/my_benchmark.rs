use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId, PlotConfiguration, PlottingBackend};

use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{prelude::*, BufReader};

use std::time::{Duration, Instant};

use relational_parsing::{Symbol, Nonterminal, Terminal, Word, Grammar, find_parses_no_memo, parse_count_memo};
use relational_parsing::find_parses;

pub fn basic_relational_parsing_example_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a', 'b', 'c']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['S']);
    let start: Nonterminal = 'S';
    let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('S', HashSet::from([
        vec![Symbol::Terminal('a')],
        vec![Symbol::Nonterminal('S'), Symbol::Terminal('a')],
        vec![Symbol::Nonterminal('S'), Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')]
    ]));
    Grammar::new(terminals, nonterminals, start, rules)
}

pub fn e_rule_relational_parsing_example_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a', 'b', 'c']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['S']);
    let start: Nonterminal = 'S';
    let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('S', HashSet::from([
        vec![Symbol::Terminal('a')],
        vec![Symbol::Nonterminal('S'), Symbol::Terminal('a')],
        vec![Symbol::Nonterminal('S'), Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')],
        vec![Symbol::Epsilon]
    ]));
    Grammar::new(terminals, nonterminals, start, rules)
}

pub fn odd_number_of_a_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['S']);
    let start: Nonterminal = 'S';
    let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('S', HashSet::from([
        vec![Symbol::Terminal('a'), Symbol::Nonterminal('S'), Symbol::Terminal('a')],
        vec![Symbol::Terminal('a')]
    ]));
    Grammar::new(terminals, nonterminals, start, rules)
}

pub fn direct_left_recursive_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['A']);
    let start: Nonterminal = 'A';
    let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('A', HashSet::from([
        vec![Symbol::Nonterminal('A'), Symbol::Terminal('a')],
        vec![Symbol::Epsilon]
    ]));
    Grammar::new(terminals, nonterminals, start, rules)
}

pub fn indirect_left_recursive_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a', 'b']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['A', 'B']);
    let start: Nonterminal = 'A';
    let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('A', HashSet::from([
        vec![Symbol::Nonterminal('B'), Symbol::Terminal('a')],
        vec![Symbol::Terminal('a')],
    ]));
    rules.insert('B', HashSet::from([
        vec![Symbol::Nonterminal('A'), Symbol::Terminal('b')],
        vec![Symbol::Terminal('b')],
    ]));
    Grammar::new(terminals, nonterminals, start, rules) 
}

pub fn direct_right_recursive_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['A']);
    let start: Nonterminal = 'A';
    let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('A', HashSet::from([
        vec![Symbol::Terminal('a'), Symbol::Nonterminal('A')],
        vec![Symbol::Terminal('a')],
    ]));
    Grammar::new(terminals, nonterminals, start, rules) 
}

pub fn indirect_right_recursive_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a', 'b']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['A', 'B']);
    let start: Nonterminal = 'A';
    let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('A', HashSet::from([
        vec![Symbol::Terminal('a'), Symbol::Nonterminal('B')],
        vec![Symbol::Terminal('a')],
    ]));
    rules.insert('B', HashSet::from([
        vec![Symbol::Terminal('b'), Symbol::Nonterminal('A')],
        vec![Symbol::Terminal('b')],
    ]));
    Grammar::new(terminals, nonterminals, start, rules)
}

fn parse_grammars(c: &mut Criterion) {
    let grammars: Vec<(Grammar, &str)> = vec![
        (basic_relational_parsing_example_grammar(), "basic.streams"),
        (e_rule_relational_parsing_example_grammar(), "e_rule.streams"),
        (odd_number_of_a_grammar(), "odd_nr_a.streams"),
        (direct_left_recursive_grammar(), "direct_left.streams"),
        (indirect_left_recursive_grammar(), "indirect_left.streams"),
        (direct_right_recursive_grammar(), "direct_right.streams"),
        (indirect_right_recursive_grammar(), "indirect_right.streams")
    ];

    for (grammar, stream_file) in grammars.iter() {

        if let Ok(file) = File::open(stream_file) {
            let reader = BufReader::new(file);

            let plot_config = PlotConfiguration::default();

            let mut group = c.benchmark_group(*stream_file);
            group.sampling_mode(criterion::SamplingMode::Flat);
            group.measurement_time(Duration::from_secs(20));
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