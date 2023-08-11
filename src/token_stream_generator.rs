use std::fmt;
use std::fs::File;
use std::io::Write;
use std::collections::{HashSet, HashMap, VecDeque};
use rand::prelude::*;
use crate::{Terminal, Nonterminal, Word, Symbol, Grammar};

pub struct TokenStreams {
    streams: Vec<Vec<Terminal>>,
}

impl TokenStreams {
    pub fn insert(&mut self, stream: Vec<Terminal>) {
        self.streams.push(stream);
    }
    
    pub fn to_file(self, filename: &str) -> std::io::Result<()> {
        let mut file: File = File::create(filename)?;

        for stream in self.streams {
            for token in stream {
                write!(file, "{}", token)?;
            }
            write!(file, "\n")?;
        }

        Ok(())
    }
}

impl fmt::Display for TokenStreams {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for stream in &self.streams {
            for token in stream {
                write!(f, "{}", token)?;
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

pub fn generate_tokens(grammar: &Grammar, max_streams: usize, min_len: usize) -> TokenStreams {
    let mut finished_streams: TokenStreams = TokenStreams { streams: Vec::new() };

    while finished_streams.streams.len() < max_streams {
        finished_streams.insert(generate(grammar, min_len + (finished_streams.streams.len() * 10)));
    }

    finished_streams
}

pub fn generate(grammar: &Grammar, min_len: usize) -> Vec<Terminal> {
    let mut res: Vec<Terminal> = Vec::new();

    let (positive_rules, neutral_rules, negative_rules) = generate_rule_sets(grammar);

    let mut todo: VecDeque<Symbol> = VecDeque::from([Symbol::Nonterminal(grammar.start)]);
    let mut nonterm_count: usize = 1;
    let mut rng = rand::thread_rng();

    while let Some(curr_symb) = todo.pop_front() {
        match curr_symb {
            Symbol::Epsilon => continue,
            Symbol::Terminal(t) => {
                res.push(t);
            },
            Symbol::Nonterminal(nt) => {
                handle_nonterminal(res.len() + todo.len() >= min_len, nonterm_count <= 3, &positive_rules, &neutral_rules, &negative_rules, &nt, &mut todo, &mut nonterm_count, &mut rng);
            }
        }
    }

    res
}

pub fn big_stream_generator(grammar: &Grammar, min_length: usize, filename: &str) -> std::io::Result<()> {
    let mut file: File = File::create(filename)?;

    let (positive_rules, neutral_rules, negative_rules) = generate_rule_sets(grammar);

    let mut todo: VecDeque<Symbol> = VecDeque::from([Symbol::Nonterminal(grammar.start)]);
    let mut count: usize = 0;
    let mut nonterm_count: usize = 1;
    let mut rng = rand::thread_rng();

    while let Some(curr_symb) = todo.pop_front() {
        match curr_symb {
            Symbol::Epsilon => continue,
            Symbol::Terminal(t) => {
                count += 1;
                write!(file, "{}", t)?;
            },
            Symbol::Nonterminal(nt) => {
                handle_nonterminal(count + todo.len() >= min_length, nonterm_count <= 3, &positive_rules, &neutral_rules, &negative_rules, &nt, &mut todo, &mut nonterm_count, &mut rng);
            }
        }
    }
    Ok(())
}

fn generate_rule_sets(grammar: &Grammar) -> (HashMap<Nonterminal, HashSet<(Word, usize)>>, HashMap<Nonterminal, HashSet<Word>>, HashMap<Nonterminal, HashSet<Word>>) {
    let mut positive_rules: HashMap<Nonterminal, HashSet<(Word, usize)>> = HashMap::new();
    let mut neutral_rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();
    let mut negative_rules: HashMap<Nonterminal, HashSet<Word>> = HashMap::new();

    for (nonterm, word_set) in grammar.rules.iter() {
        for word in word_set {
            let mut curr_nonterm_count: usize = 0;
            for symb in word.iter() {
                if let Symbol::Nonterminal(_) = symb {
                    curr_nonterm_count += 1;
                }
            }
            if curr_nonterm_count > 1 {
                positive_rules.entry(*nonterm).or_default().insert((word.clone(), curr_nonterm_count - 1));
            } else if curr_nonterm_count == 1 {
                neutral_rules.entry(*nonterm).or_default().insert(word.clone());
            } else {
                negative_rules.entry(*nonterm).or_default().insert(word.clone());
            }
        }
    }

    (positive_rules, neutral_rules, negative_rules)
}

fn handle_nonterminal(reduce: bool, add: bool, positive_rules: &HashMap<Nonterminal, HashSet<(Word, usize)>>, neutral_rules: &HashMap<Nonterminal, HashSet<Word>>, negative_rules: &HashMap<Nonterminal, HashSet<Word>>, nonterminal: &Nonterminal, todo: &mut VecDeque<Symbol>, nonterm_count: &mut usize, rng: &mut ThreadRng) {
    if reduce {
        try_reduce(positive_rules, neutral_rules, negative_rules, nonterminal, todo, nonterm_count, rng);
    } else if add {
        try_add(positive_rules, neutral_rules, negative_rules, nonterminal, todo, nonterm_count, rng);
    } else {
        if rng.gen_range(0..100) >= 30 {
            try_reduce(positive_rules, neutral_rules, negative_rules, nonterminal, todo, nonterm_count, rng);
        } else {
            try_add(positive_rules, neutral_rules, negative_rules, nonterminal, todo, nonterm_count, rng);
        }
    }
}

fn try_reduce(positive_rules: &HashMap<Nonterminal, HashSet<(Word, usize)>>, neutral_rules: &HashMap<Nonterminal, HashSet<Word>>, negative_rules: &HashMap<Nonterminal, HashSet<Word>>, nonterminal: &Nonterminal, todo: &mut VecDeque<Symbol>, nonterm_count: &mut usize, rng: &mut ThreadRng) {
    if let Some(reducing_rule_set) = negative_rules.get(nonterminal) {
        push_reducing_rule(reducing_rule_set, todo, nonterm_count, rng);
    } else if let Some(neutral_rule_set) = neutral_rules.get(nonterminal){
        push_neutral_rule(neutral_rule_set, todo, rng);
    }
    else if let Some(adding_rule_set) = positive_rules.get(nonterminal) {
        push_adding_rule(adding_rule_set, todo, nonterm_count, rng);
    }
}

fn try_add(positive_rules: &HashMap<Nonterminal, HashSet<(Word, usize)>>, neutral_rules: &HashMap<Nonterminal, HashSet<Word>>, negative_rules: &HashMap<Nonterminal, HashSet<Word>>, nonterminal: &Nonterminal, todo: &mut VecDeque<Symbol>, nonterm_count: &mut usize, rng: &mut ThreadRng) {
    if let Some(adding_rule_set) = positive_rules.get(nonterminal) {
        push_adding_rule(adding_rule_set, todo, nonterm_count, rng);
    } else if let Some(neutral_rule_set) = neutral_rules.get(nonterminal) {
        push_neutral_rule(neutral_rule_set, todo, rng);
    } else if let Some(reducing_rule_set) = negative_rules.get(nonterminal) {
        push_reducing_rule(reducing_rule_set, todo, nonterm_count, rng);
    }
}

fn push_reducing_rule(rule_set: &HashSet<Word>, todo: &mut VecDeque<Symbol>, nonterm_count: &mut usize, rng: &mut ThreadRng) {
    let sampling_vec: Vec<&Word> = rule_set.into_iter().collect();
    if let Some(random_rule) = sampling_vec.get(rng.gen_range(0..sampling_vec.len())) {
        prepend_rule(random_rule, todo);
        *nonterm_count -= 1;
    }
}

fn push_adding_rule(rule_set: &HashSet<(Word, usize)>, todo: &mut VecDeque<Symbol>, nonterm_count: &mut usize, rng: &mut ThreadRng) {
    let sampling_vec: Vec<&(Word, usize)> = rule_set.into_iter().collect();
    if let Some((random_rule, new_nonterm_count)) = sampling_vec.get(rng.gen_range(0..sampling_vec.len())) {
        prepend_rule(random_rule, todo);
        *nonterm_count += new_nonterm_count;
    }
}

fn push_neutral_rule(rule_set: &HashSet<Word>, todo: &mut VecDeque<Symbol>, rng: &mut ThreadRng) {
    let sampling_vec: Vec<&Word> = rule_set.into_iter().collect();
    if let Some(random_rule) = sampling_vec.get(rng.gen_range(0..sampling_vec.len())) {
        prepend_rule(random_rule, todo);
    }
}

fn prepend_rule(rule: &Word, todo: &mut VecDeque<Symbol>) {
    let mut temprule: Vec<Symbol> = rule.to_vec();
    while let Some(symbol) = temprule.pop() {
        todo.push_front(symbol);
    }
}
