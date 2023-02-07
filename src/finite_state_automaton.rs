//! # Finite state automaton
//! 
//! The `finite_state_automaton` module defines the data structure used to represent finite state automatons.
//! Additionally, it defines the methods necessary to build finite state automata from the atomic languages belonging to a given grammar,
//! as well as methods used to simulate finite state automata during parsing.
use std::collections::{HashSet, HashMap, VecDeque};
use std::fmt;
use std::fs::File;
use std::io::Write;

use crate::word::*;
use crate::regex::*;

pub type State = usize;

#[derive(Debug)]
pub struct FiniteStateAutomaton {
    states: HashSet<State>,
    accepting_states: HashSet<State>,
    start: State,
    edges: HashMap<State, HashMap<Symbol, HashSet<(State, Rules)>>>,
    atomic_to_state: HashMap<(Symbol, Terminal), (State, HashSet<Rules>)>,
}

impl fmt::Display for FiniteStateAutomaton {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "States: ")?;
        for state in &self.states {
            write!(f, "{} ", state)?;
        }
        write!(f, "\nAccepting states: ")?;
        for state in &self.accepting_states {
            write!(f, "{} ", state)?;
        }
        write!(f, "\nStart state: {}\n", &self.start)?;
        write!(f, "edges:\n")?;
        for (state, edge_list) in &self.edges {
            write!(f, "{}: ", state)?;
            for (symbol, destinations) in edge_list {
                for (state, _) in destinations {
                    write!(f, "|to {} via {}| ", state, symbol)?;
                }
            }
            write!(f, "\n")?;
        }
        write!(f, "Edge to rules:\n")?;
        for (state, edge_list) in &self.edges {
            for (symbol, destinations) in edge_list {
                for (_, rules) in destinations {
                    write!(f, "{}: {}: ", state, symbol)?;
                    for (nonterminal, rule) in rules {
                        write!(f, "[{} -> ", nonterminal)?;
                        for symbol in rule {
                            write!(f, "{}", symbol)?;
                        }
                        write!(f, "] ")?;
                    }
                    write!(f, "\n")?;
                }
            }
        }
        write!(f, "Atomic to state:\n")?;
        for ((symbol, terminal), (state, rule_set)) in &self.atomic_to_state {
            write!(f, "[{}]^({}) {} ", symbol, terminal, state)?;
            for rules in rule_set {
                write!(f, "| ")?;
                for (nonterminal, rule) in rules {
                    write!(f, "[{} -> ", nonterminal)?;
                    for symbol in rule {
                        write!(f, "{}", symbol)?;
                    }
                    write!(f, "] ")?;
                }
                write!(f, "|")?;
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

impl FiniteStateAutomaton {
    pub fn build_fsa(terminals: &HashSet<Terminal>, start_nt: Nonterminal, rules: &HashMap<Nonterminal, HashSet<Word>>) -> FiniteStateAutomaton {
        let start: State = 0;
        let epsilon: State = 1;
        let mut highest_state: State = 1;
        let mut states: HashSet<State> = HashSet::from([start, epsilon]);
        let mut accepting_states: HashSet<State> = HashSet::from([epsilon]);
        let mut edges: HashMap<State, HashMap<Symbol, HashSet<(State, Rules)>>> = HashMap::new();
        let mut atomic_to_state: HashMap<(Symbol, Terminal), (State, HashSet<Rules>)> = HashMap::new();

        edges.insert(start, HashMap::from([(Symbol::Nonterminal(start_nt), HashSet::from([(epsilon, Vec::new())]))]));

        // If starting symbol can be nulled, make starting state accepting. Does not actually parse correctly at the moment, unfortunately.
        if rules.get(&start_nt).unwrap().contains(&vec![Symbol::Epsilon]) {
            highest_state += 1;
            states.insert(highest_state);
            edges.entry(start).or_default().insert(Symbol::Epsilon, HashSet::from([(highest_state, Vec::from([(start_nt, Vec::from([Symbol::Epsilon]))]))]));
            accepting_states.insert(highest_state);
        }

        // add terminal derivations to atomic_to_state
        for terminal in terminals {
            atomic_to_state.insert((Symbol::Terminal(*terminal), *terminal), (epsilon, HashSet::new()));
        }

        let atomic_regex: Regex = Regex::new(terminals, rules);
        let mut regex_to_state: HashMap<VecDeque<WordNode>, (State, State)> = HashMap::new();

        for ((nonterminal, terminal), node) in atomic_regex.regex {
            if let (true, rules) = node.is_e_node_get_rules() {
                atomic_to_state.insert((Symbol::Nonterminal(nonterminal), terminal), (epsilon, rules));
                continue;
            }

            let mut wordnode_queue: Vec<WordNode> = node.nodes.clone();
            let mut regex_to_state_key: VecDeque<WordNode> = VecDeque::with_capacity(wordnode_queue.len());
            let mut atomic_rules: HashSet<Rules> = HashSet::new();

            let mut node_end: State;

            while let Some(wordnode) = wordnode_queue.pop() {

                let prev_key = regex_to_state_key.clone();
                regex_to_state_key.push_front(wordnode.clone());

                let node_start: State;

                if let Some((dest, end)) = regex_to_state.get(&regex_to_state_key) {
                    let (dest, end) = (*dest, *end);
                    if let (true, rule_set) = wordnode.is_e_node_get_rules() {
                        atomic_rules = rule_set;
                        regex_to_state_key.push_front(wordnode.clone());
                        regex_to_state.insert(regex_to_state_key.clone(), (dest, end));
                        continue;
                    } 
                    continue;
                } else {
                    if let Some((dest, end)) = regex_to_state.get(&prev_key) {
                        node_end = *dest;
                        let end = *end;
                        if let (true, rule_set) = wordnode.is_e_node_get_rules() {
                            atomic_rules = rule_set;
                            regex_to_state.insert(regex_to_state_key.clone(), (node_end, end));
                            continue;
                        }
                        highest_state += 1;
                        node_start = highest_state;
                    } else {
                        highest_state += 1;
                        node_start = highest_state;
                        if let (true, rule_set) = wordnode.is_e_node_get_rules() {
                            atomic_rules = rule_set;
                            regex_to_state.insert(regex_to_state_key.clone(), (node_start, node_start));
                            continue;
                        }
                        if wordnode.kleene_star {
                            node_end = node_start;
                        } else {
                            highest_state += 1;
                            node_end = highest_state;
                        }
                    }
                }
                regex_to_state.insert(regex_to_state_key.clone(), (node_start, node_end));

                for (rules, wordnodeword_set) in wordnode.get_by_base_rules() {
                    let mut sub_states: Vec<State> = vec![node_start];
                    let mut opt_penultimate_state: Option<State> = None;

                    for wordnodeword in wordnodeword_set {
                        let mut source: State = node_start;
                        let mut target: State;
                        let mut carried_rules: Rules = Vec::new();

                        for index in 0..wordnodeword.len() {
                            if index == sub_states.len() - 1 {
                                if index == wordnodeword.len() - 1 {
                                    sub_states.push(node_end);
                                } else {
                                    highest_state += 1;
                                    sub_states.push(highest_state);
                                }
                            }

                            if let WordNodeSymbol::Rules(word_rules) = &wordnodeword[index] {
                                carried_rules = [word_rules.clone(), carried_rules].concat();
                                target = sub_states[index+1];
                                if target == node_end {
                                    let entry = edges.entry(source).or_default().entry(Symbol::Epsilon).or_default();
                                    
                                    entry.insert((target, [rules.clone(), carried_rules].concat()));
                                    carried_rules = Vec::new();
                                }
                            } else {
                                let entry = edges.entry(source).or_default().entry(
                                    match wordnodeword[index] {
                                        WordNodeSymbol::Nonterminal(nt) => Symbol::Nonterminal(nt),
                                        WordNodeSymbol::Terminal(t) => Symbol::Terminal(t),
                                        WordNodeSymbol::Epsilon => Symbol::Epsilon,
                                        _ => continue,
                                    }
                                ).or_default();

                                target = sub_states[index+1];
                                if target == node_end {
                                    if let WordNodeSymbol::Nonterminal(_) = wordnodeword[index] {
                                        let penultimate_state;
                                        if let Some(state) = opt_penultimate_state {
                                            penultimate_state = state;
                                        } else {
                                            highest_state += 1;
                                            penultimate_state = highest_state;
                                            opt_penultimate_state = Some(penultimate_state);
                                        }
                                        if carried_rules.len() > 0 {
                                            entry.insert((penultimate_state, carried_rules));
                                        } else {
                                            entry.insert((penultimate_state, Vec::new()));
                                        }
                                        edges.entry(penultimate_state).or_default().entry(Symbol::Epsilon).or_default().insert((target, rules.clone()));
                                    } else {
                                        entry.insert((target, [rules.clone(), carried_rules].concat()));
                                    }
                                    carried_rules = Vec::new();
                                }
                                else if carried_rules.len() > 0 {
                                    entry.insert((target, carried_rules));
                                    carried_rules = Vec::new();
                                } else {
                                    entry.insert((target, Vec::new()));
                                }
                                source = sub_states[index+1];
                            }
                        }
                    }
                    states.extend(sub_states);
                    if let Some(state) = opt_penultimate_state {
                        states.insert(state);
                    }
                }


            }
            accepting_states.insert(regex_to_state.get(&regex_to_state_key).unwrap().1);
            atomic_to_state.insert((Symbol::Nonterminal(nonterminal), terminal), (regex_to_state.get(&regex_to_state_key).unwrap().0, atomic_rules));
        }

        FiniteStateAutomaton{states, accepting_states, start, edges, atomic_to_state}
    }

    pub fn to_dot(&self, filename: &str) -> std::io::Result<()> {
        let mut file = File::create(format!("{}.dot", filename))?;
        write!(file, "digraph G {{\n")?;
        let mut state_to_shape: HashMap<State, &str> = HashMap::new();
        for state in &self.states {
            if self.accepting_states.contains(state) {
                state_to_shape.insert(*state, "doublecircle");
            } else {
                state_to_shape.insert(*state, "circle");
            }
        }
        for state in &self.states {
            write!(file, "{} [ shape={} ]\n", state, state_to_shape.get(state).unwrap())?;
        }
        for ((symbol, terminal), (state, rule_set)) in &self.atomic_to_state {
            match symbol {
                Symbol::Nonterminal(nonterm) => {
                    write!(file, "\"[{}]^({})\" [ shape=rectangle ]\n\"[{}]^({})\" -> {}", nonterm, terminal, nonterm, terminal, state)?;
                    if rule_set.len() > 0 {
                        write!(file, "[ label=\"")?;
                        for rules in rule_set {
                            write!(file, "(")?;
                            for rule in rules {
                                write!(file, "[{} -> ", &rule.0)?;
                                for symbol in &rule.1 {
                                    write!(file, "{}", symbol)?;
                                }
                                write!(file, "]")?;
                            }
                            write!(file, ")")?;
                        }
                        write!(file, "\" ]")?;
                    }
                    write!(file, "\n")?;
                },
                Symbol::Terminal(term) => write!(file, "\"[{}]^({})\" [ shape=rectangle ]\n\"[{}]^({})\" -> {}\n", term, terminal, term, terminal, state)?,
                _ => {},
            }
        }
        for (source, edge_list) in &self.edges {
            for (symbol, destinations) in edge_list {
                for (dest, rules) in destinations {
                    match symbol {
                        Symbol::Epsilon => write!(file, "{} -> {} [ label=\"e ", source, dest)?,
                        Symbol::Nonterminal(nonterminal) => write!(file, "{} -> {} [ label=\"{} ", source, dest, nonterminal)?,
                        Symbol::Terminal(terminal) => write!(file, "{} -> {} [ label=\"{} ", source, dest, terminal)?,
                    }
                    for rule in rules {
                        write!(file, "[{} -> ", &rule.0)?;
                        for symbol in &rule.1 {
                            write!(file, "{}", symbol)?;
                        }
                        write!(file, "] ")?;
                    }
                    write!(file, "\" ]\n")?;
                }
            }
        }
        write!(file, "}}")
    }

    pub fn simulate(&self, curr_state: &State, symbol: Symbol) -> Option<HashSet<(&State, &Rules, bool)>> {
        self.edges.get(&curr_state)?
            .get(&symbol)
            .map(|destinations| {
                let mut res: HashSet<(&State, &Rules, bool)> = HashSet::new();
                for (dest, rules) in destinations {
                    res.insert((dest, rules, self.is_accepting(dest)));
                }
                res
            })
    }

    pub fn is_accepting(&self, curr_state: &State) -> bool {
        self.accepting_states.contains(curr_state)
    }

    pub fn get_start(&self) -> (State, bool) {
        (self.start, self.is_accepting(&self.start))
    }

    pub fn get_atomic(&self, symbol: Symbol, terminal: Terminal) -> Option<(&State, &HashSet<Rules>, bool)> {
        self.atomic_to_state.get(&(symbol, terminal))
            .map(|(dest, rules_set)| (dest, rules_set, self.is_accepting(dest)))
    }

    pub fn has_edge(&self, curr_state: &State) -> bool {
        self.edges.get(curr_state)
            .map_or(false, |trans_list| !trans_list.is_empty())
    }

}