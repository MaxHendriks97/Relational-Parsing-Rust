use std::collections::{HashMap, HashSet};

use relational_parsing::{Symbol, Nonterminal, Terminal, Word, Grammar};

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

pub fn extra_e_rule_relational_parsing_example_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a', 'b', 'c']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['S']);
    let start: Nonterminal = 'S';
    let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('S', HashSet::from([
        vec![Symbol::Terminal('a')],
        vec![Symbol::Nonterminal('S'), Symbol::Terminal('a')],
        vec![Symbol::Nonterminal('S'), Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('c')],
        vec![Symbol::Nonterminal('S'), Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Nonterminal('S'), Symbol::Terminal('c')],
        vec![Symbol::Epsilon]
    ]));
    Grammar::new(terminals, nonterminals, start, rules)
}

pub fn three_rule_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a', 'b']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['S']);
    let start: Nonterminal = 'S';
    let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('S', HashSet::from([
        vec![Symbol::Terminal('a'), Symbol::Nonterminal('S'), Symbol::Terminal('b')],
        vec![Symbol::Nonterminal('S'), Symbol::Terminal('a'), Symbol::Terminal('b')],
        vec![Symbol::Terminal('a'), Symbol::Terminal('a'), Symbol::Terminal('a')],
    ]));
    Grammar::new(terminals, nonterminals, start, rules)
}

pub fn difficult_bottom_up_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a', '+', '-']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['S', 'E', 'F', 'Q']);
    let start: Nonterminal = 'S';
    let mut rules:  HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('S', HashSet::from([vec![Symbol::Nonterminal('E')]]));
    rules.insert('E', HashSet::from([
        vec![Symbol::Nonterminal('E'), Symbol::Nonterminal('Q'), Symbol::Nonterminal('F')],
        vec![Symbol::Nonterminal('F')]
    ]));
    rules.insert('F', HashSet::from([vec![Symbol::Terminal('a')]]));
    rules.insert('Q', HashSet::from([
        vec![Symbol::Terminal('+')],
        vec![Symbol::Terminal('-')],
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

pub fn even_a_middle_b_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a', 'b']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['S']);
    let start: Nonterminal = 'S';
    let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('S', HashSet::from([
        vec![Symbol::Terminal('a'), Symbol::Nonterminal('S'), Symbol::Terminal('a')],
        vec![Symbol::Terminal('b')]
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

pub fn even_more_indirect_left_recursive_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a', 'b', 'c']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['A', 'B', 'C']);
    let start: Nonterminal = 'A';
    let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('A', HashSet::from([
        vec![Symbol::Nonterminal('B'), Symbol::Terminal('a')],
        vec![Symbol::Terminal('a')],
    ]));
    rules.insert('B', HashSet::from([
        vec![Symbol::Nonterminal('C'), Symbol::Terminal('a')],
        vec![Symbol::Terminal('b')],
    ]));
    rules.insert('C', HashSet::from([
        vec![Symbol::Nonterminal('A'), Symbol::Terminal('a')],
        vec![Symbol::Terminal('c')],
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

pub fn test_grammar() -> Grammar {
    let terminals: HashSet<Terminal> = HashSet::from(['a']);
    let nonterminals: HashSet<Nonterminal> = HashSet::from(['A']);
    let start: Nonterminal = 'A';
    let mut rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    rules.insert('A', HashSet::from([
        vec![Symbol::Nonterminal('A'), Symbol::Terminal('a'), Symbol::Nonterminal('A')],
        vec![Symbol::Epsilon],
    ]));
    Grammar::new(terminals, nonterminals, start, rules)
}