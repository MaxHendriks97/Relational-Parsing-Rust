//! # Finite state automaton
//! 
//! The `finite_state_automaton` module defines the data structure used to represent finite state automatons.
//! Additionally, it defines the methods necessary to build finite state automata from the atomic languages belonging to a given grammar,
//! as well as methods used to simulate finite state automata during parsing.
use std::collections::{HashSet, HashMap};
use std::fmt;
use std::fs::File;
use std::io::Write;
use std::ops::Add;

use crate::{word::*, GrammarRules, RegexSymbol};
use crate::regex::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct State(usize);

impl fmt::Display for State {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Add<usize> for State {
    type Output = State;

    fn add(self, rhs: usize) -> Self::Output {
        State(self.0 + rhs)
    }
}

impl State {
    pub fn new(id: usize) -> State {
        State(id)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct States(HashSet<State>);

impl fmt::Display for States {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for state in &self.0 {
            write!(f, "{} ", state)?;
        }
        Ok(())
    }
}

impl States {
    pub fn new() -> States {
        States(HashSet::new())
    }

    pub fn from(states: Vec<State>) -> States {
        States(HashSet::from_iter(states))
    }

    pub fn insert(&mut self, state: State) {
        self.0.insert(state);
    }

    pub fn contains(&self, state: &State) -> bool {
        self.0.contains(state)
    }

    pub fn iter(&self) -> std::collections::hash_set::Iter<State> {
        self.0.iter()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Edge {
    pub symbol: Symbol,
    pub destination: State,
    pub opt_rules: Option<Rules>,
}

impl fmt::Display for Edge {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if self.opt_rules.is_some() {
            write!(f, "{} -> {} using {}", self.symbol, self.destination, self.opt_rules.clone().unwrap())
        } else {
            write!(f, "{} -> {}", self.symbol, self.destination)
        }
    }
}

impl Edge {
    pub fn new(symbol: Symbol, destination: State, opt_rules: Option<Rules>) -> Edge {
        Edge {
            symbol,
            destination,
            opt_rules,
        }
    }
}

#[derive(Debug)]
pub struct EdgeSet(HashSet<Edge>);

impl fmt::Display for EdgeSet {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{{")?;
        for edge in &self.0 {
            write!(f, "{} ", edge)?;
        }
        write!(f, "}}")
    }
}

impl EdgeSet {
    pub fn new() -> EdgeSet {
        EdgeSet(HashSet::new())
    }

    pub fn add(&mut self, edge: Edge) {
        self.0.insert(edge);
    }

    pub fn insert(&mut self, symbol: Symbol, destination: State, opt_rules: Option<Rules>) {
        self.0.insert(Edge::new(symbol, destination, opt_rules));
    }

    pub fn contains(&self, edge: &Edge) -> bool {
        self.0.contains(edge)
    }

    pub fn get_edges(&self, symbol: &Symbol) -> Vec<Edge> {
        self.0.iter().filter(|edge| edge.symbol == *symbol).cloned().collect()
    }

    pub fn iter(&self) -> std::collections::hash_set::Iter<Edge> {
        self.0.iter()
    }
}

#[derive(Debug)]
pub struct Edges(HashMap<State, EdgeSet>);

impl fmt::Display for Edges {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (state, edge_set) in &self.0 {
            write!(f, "{}: {}\n", state, edge_set)?;
        }
        Ok(())
    }
}

impl Edges {
    pub fn new() -> Edges {
        Edges(HashMap::new())
    }

    pub fn add(&mut self, state: State, edge: Edge) {
        self.0.entry(state).or_insert(EdgeSet::new()).add(edge);
    }

    pub fn insert(&mut self, state: State, symbol: Symbol, destination: State, opt_rules: Option<Rules>) {
        self.0.entry(state).or_insert(EdgeSet::new()).insert(symbol, destination, opt_rules);
    }

    pub fn get(&self, state: &State) -> Option<&EdgeSet> {
        self.0.get(state)
    }

    pub fn contains(&self, state: &State, edge: &Edge) -> bool {
        if let Some(edge_set) = self.0.get(state) {
            edge_set.contains(edge)
        } else {
            false
        }
    }

    pub fn has_edge(&self, state: &State) -> bool {
        self.0.contains_key(state)
    }

    pub fn iter(&self) -> std::collections::hash_map::Iter<State, EdgeSet> {
        self.0.iter()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Atomic {
    pub symbol: Symbol,
    pub terminal: Terminal
}

impl fmt::Display for Atomic {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[{}]^({})", self.symbol, self.terminal)
    }
}

impl Atomic {
    pub fn new(symbol: Symbol, terminal: Terminal) -> Atomic {
        Atomic{symbol, terminal}
    }
}

#[derive(Debug)]
pub struct AtomicToState(HashMap<Atomic, (State, RulesSet)>);

impl fmt::Display for AtomicToState {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (atomic, (state, rules_set)) in &self.0 {
            if rules_set.is_empty() {
                write!(f, "{}: {}\n", atomic, state)?;
            } else {
                write!(f, "{}: {} -> {}\n", atomic, state, rules_set)?;
            }
        }
        Ok(())
    }
}

impl AtomicToState {
    pub fn new() -> AtomicToState {
        AtomicToState(HashMap::new())
    }

    pub fn add(&mut self, atomic: Atomic, state: State, rules_set: RulesSet) {
        self.0.insert(atomic, (state, rules_set));
    }

    pub fn insert_rule(&mut self, atomic: Atomic, state: State, rule: Rule) {
        self.0.entry(atomic).or_insert((state, RulesSet::new())).1.insert_rule(rule);
    }

    pub fn get(&self, atomic: &Atomic) -> Option<&(State, RulesSet)> {
        self.0.get(atomic)
    }

    pub fn iter(&self) -> std::collections::hash_map::Iter<Atomic, (State, RulesSet)> {
        self.0.iter()
    }
}

#[derive(Debug)]
pub struct FiniteStateAutomaton {
    states: States,
    accepting_states: States,
    start: State,
    edges: Edges,
    atomic_to_state: AtomicToState,
}

impl fmt::Display for FiniteStateAutomaton {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "States: {}\nAccepting States: {}\nStart: {}\nEdges:\n{}\nAtomic to State:\n{}", self.states, self.accepting_states, self.start, self.edges, self.atomic_to_state)
    }
}

impl FiniteStateAutomaton {
    pub fn build_fsa(terminals: &HashSet<Terminal>, start_nt: Nonterminal, rules: &GrammarRules) -> FiniteStateAutomaton {
        let start: State = State::new(0);
        let epsilon: State = State::new(1);
        let mut highest_state: State = epsilon;
        let mut states: States = States::from(vec![start, epsilon]);
        let mut accepting_states: States = States::from(vec![epsilon]);
        let mut edges: Edges = Edges::new();
        let mut atomic_to_state: AtomicToState = AtomicToState::new();

        edges.insert(start, Symbol::Nonterminal(start_nt), epsilon, None);

        // If starting symbol can be nulled, make starting state accepting. Does not actually parse correctly at the moment, unfortunately.
        if rules.nt_has_word(start_nt, &Word::from_single(Symbol::Epsilon)) {
            edges.insert(start, Symbol::Epsilon, epsilon, Some(Rules::from_single(start_nt, Word::from_single(Symbol::Epsilon))));
        }

        // add terminal derivations to atomic_to_state
        for terminal in terminals {
            atomic_to_state.add(Atomic::new(Symbol::Terminal(*terminal), *terminal), epsilon, RulesSet::new());
        }

        let Regex(atomic_regex): Regex = Regex::new(terminals, rules);

        // For every atomic language, build a FSA or (partially) hook into an existing one
        for ((nonterminal, terminal), node) in atomic_regex {
            // If the regex is null, add the atomic language to the accepting node
            if let Some(rules) = node.get_nulling_rules() {
                atomic_to_state.add(Atomic::new(Symbol::Nonterminal(nonterminal), terminal), epsilon, rules);
                continue;
            }

            // If the regex is not null, build a FSA for it
            let starting_state: State = highest_state + 1;
            highest_state = starting_state;
            states.insert(starting_state);
            let (_, new_highest_state, accepting_state, atomic_rules) = FiniteStateAutomaton::node_to_states(&node, starting_state, None, highest_state, &mut states, &mut accepting_states, &mut edges);
            highest_state = new_highest_state;
            accepting_states.insert(accepting_state);

            atomic_to_state.add(Atomic::new(Symbol::Nonterminal(nonterminal), terminal), starting_state, atomic_rules);
        }

        FiniteStateAutomaton{states, accepting_states, start, edges, atomic_to_state}
    }

    // Returns, ending state, highest state, accepting state, starting rules
    pub fn node_to_states(node: &Node, starting_state: State, ending_state: Option<State>, highest_state: State, states: &mut States, accepting_states: &mut States, edges: &mut Edges) -> (State, State, State, RulesSet) {
        let mut curr_highest_state: State = highest_state;
        let mut accepting_state: State = State::new(1); //initialise to epsilon
        let mut starting_rules: RulesSet = RulesSet::new();

        match node{
            Node::Opt { nodes, kleene } => {
                if *kleene {
                    // Ending state is starting state
                    for node in nodes {
                        if node.is_e_node() {
                            let new_starting_rules: RulesSet;
                            (_, _, _, new_starting_rules) = FiniteStateAutomaton::node_to_states(node, starting_state, None, curr_highest_state, states, accepting_states, edges);
                            starting_rules.extend(new_starting_rules);
                            accepting_states.insert(starting_state);
                            continue;
                        }
                        let new_starting_rules: RulesSet;
                        (_, curr_highest_state, accepting_state, new_starting_rules) = FiniteStateAutomaton::node_to_states(node, starting_state, Some(starting_state), curr_highest_state, states, accepting_states, edges);
                        starting_rules.extend(new_starting_rules);
                    }
                    return (starting_state, curr_highest_state, accepting_state, starting_rules);
                } else {
                    // Ending state is end_state.
                    // This may be null in the first iteration, so it may create a new state to be the ending state.
                    // Because all opt nodes should end at the same ending state, we set the ending state of every new loop to be the ending state of the previous loop.
                    let mut curr_ending_state: Option<State> = ending_state;
                    for node in nodes {
                        if node.is_e_node() {
                            // if no ending state yet and more than one opt_node, generate an ending state and create an epsilon transition to it from the starting state
                            if curr_ending_state.is_none() {
                                curr_highest_state = curr_highest_state + 1;
                                curr_ending_state = Some(curr_highest_state);
                                states.insert(curr_ending_state.unwrap());
                                accepting_states.insert(curr_ending_state.unwrap());
                            }
                            let (_, _, _, epsilon_rules) = FiniteStateAutomaton::node_to_states(node, starting_state, curr_ending_state, curr_highest_state, states, accepting_states, edges);
                            for rules in epsilon_rules.into_iter() {
                                edges.insert(starting_state, Symbol::Epsilon, curr_ending_state.unwrap(), Some(rules));
                            }
                            continue;
                        }
                        let new_ending_state: State;
                        let new_starting_rules: RulesSet;
                        (new_ending_state, curr_highest_state, accepting_state, new_starting_rules) = FiniteStateAutomaton::node_to_states(node, starting_state, curr_ending_state, curr_highest_state, states, accepting_states, edges);
                        curr_ending_state = Some(new_ending_state);
                        starting_rules.extend(new_starting_rules);
                    }
                    (curr_ending_state.unwrap_or_else(|| starting_state), curr_highest_state, accepting_state, starting_rules)
                }
            },
            Node::Seq { nodes, kleene } => {
                let mut curr_starting_state: State = starting_state;
                if *kleene {
                    // Ending state is None until last iteration, then ending state is starting_state
                    // curr_starting_state is the ending state of the previous iteration
                    let mut peekable_nodes = nodes.iter().peekable();
                    while let Some(node) = peekable_nodes.next() {
                        if node.is_e_node() {
                            let new_starting_rules: RulesSet;
                            (_, _, _, new_starting_rules) = FiniteStateAutomaton::node_to_states(node, curr_starting_state, None, curr_highest_state, states, accepting_states, edges);
                            starting_rules.extend(new_starting_rules);
                            continue;
                        }
                        if peekable_nodes.peek().is_some() {
                            let new_starting_state: State;
                            let new_starting_rules: RulesSet;
                            (new_starting_state, curr_highest_state, accepting_state, new_starting_rules) = FiniteStateAutomaton::node_to_states(node, curr_starting_state, None, curr_highest_state, states, accepting_states, edges);
                            curr_starting_state = new_starting_state;
                            starting_rules.extend(new_starting_rules);
                        } else {
                            let new_starting_rules: RulesSet;
                            (_, curr_highest_state, accepting_state, new_starting_rules) = FiniteStateAutomaton::node_to_states(node, curr_starting_state, Some(starting_state), curr_highest_state, states, accepting_states, edges);
                            starting_rules.extend(new_starting_rules);
                        }
                    }
                    (starting_state, curr_highest_state, accepting_state, starting_rules)
                } else {
                    // Ending state is None until last iteration, then ending state is end_state.
                    // curr_starting_state is the ending state of the previous iteration
                    let mut peekable_nodes = nodes.iter().peekable();
                    while let Some(node) = peekable_nodes.next() {
                        if node.is_e_node() {
                            let new_starting_rules: RulesSet;
                            (_, _, _, new_starting_rules) = FiniteStateAutomaton::node_to_states(node, curr_starting_state, None, curr_highest_state, states, accepting_states, edges);
                            starting_rules.extend(new_starting_rules);
                            continue;
                        }
                        if peekable_nodes.peek().is_some() {
                            let new_starting_state: State;
                            let new_starting_rules: RulesSet;
                            (new_starting_state, curr_highest_state, accepting_state, new_starting_rules) = FiniteStateAutomaton::node_to_states(node, curr_starting_state, None, curr_highest_state, states, accepting_states, edges);
                            curr_starting_state = new_starting_state;
                            starting_rules.extend(new_starting_rules);
                        } else {
                            let new_starting_rules: RulesSet;
                            (_, curr_highest_state, accepting_state, new_starting_rules) = FiniteStateAutomaton::node_to_states(node, curr_starting_state, ending_state, curr_highest_state, states, accepting_states, edges);
                            starting_rules.extend(new_starting_rules);
                        }
                    }
                    (starting_state, curr_highest_state, accepting_state, starting_rules)
                }
            },
            Node::Word { word, rules, kleene } => {
                let mut curr_state: State = starting_state;
                let mut peekable_word = word.iter().peekable();
                let mut collected_rules: Rules = Rules::new();
                let mut final_state: State = State::new(1); // initialise to epsilon
                while let Some(symbol) = peekable_word.next() {
                    if peekable_word.peek().is_some() {
                        match symbol {
                            RegexSymbol::AtomicLanguage(_, _) => panic!("Atomic language should not be in word"),
                            RegexSymbol::Nulled(nulled_rules) => collected_rules.extend(nulled_rules.clone()),
                            RegexSymbol::Nonterminal(nt) => {
                                let new_state = curr_highest_state + 1;
                                curr_highest_state = new_state;
                                states.insert(new_state);
                                if collected_rules.is_empty() {
                                    edges.insert(curr_state, Symbol::Nonterminal(*nt), new_state, None);
                                } else {
                                    edges.insert(curr_state, Symbol::Nonterminal(*nt), new_state, Some(collected_rules));
                                    collected_rules = Rules::new();
                                }
                                curr_state = new_state;
                            },
                            RegexSymbol::Terminal(t) => {
                                let new_state = curr_highest_state + 1;
                                curr_highest_state = new_state;
                                states.insert(new_state);
                                if collected_rules.is_empty() {
                                    edges.insert(curr_state, Symbol::Terminal(*t), new_state, None);
                                } else {
                                    edges.insert(curr_state, Symbol::Terminal(*t), new_state, Some(collected_rules));
                                    collected_rules = Rules::new();
                                }
                                curr_state = new_state;
                            },
                            RegexSymbol::Epsilon => panic!("Epsilon should not be followed by other symbols"),
                        }
                    } else {
                        match symbol {
                            RegexSymbol::AtomicLanguage(_, _) => panic!("Atomic language should not be in word"),
                            RegexSymbol::Nulled(nulled_rules) => {
                                let mut res_rules = rules.clone();
                                res_rules.extend(collected_rules);
                                collected_rules = Rules::new();
                                res_rules.extend(nulled_rules.clone());
                                if *kleene {
                                    final_state = starting_state;
                                    edges.insert(curr_state, Symbol::Epsilon, final_state, Some(res_rules));
                                    curr_state = starting_state;
                                } else {
                                    final_state = ending_state.unwrap_or_else(|| {
                                        // If ending state is None, then we need to create a new state
                                        let new_state = curr_highest_state + 1;
                                        curr_highest_state = new_state;
                                        states.insert(new_state);
                                        new_state
                                    });
                                    edges.insert(curr_state, Symbol::Epsilon, final_state, Some(res_rules));
                                    curr_state = final_state;
                                }
                            },
                            RegexSymbol::Nonterminal(nt) => {
                                // First create an edge to a new state for the nonterminal
                                let mut new_state = curr_highest_state + 1;
                                curr_highest_state = new_state;
                                states.insert(new_state);
                                if collected_rules.is_empty() {
                                    edges.insert(curr_state, Symbol::Nonterminal(*nt), new_state, None);
                                } else {
                                    edges.insert(curr_state, Symbol::Nonterminal(*nt), new_state, Some(collected_rules));
                                    collected_rules = Rules::new();
                                }
                                curr_state = new_state;

                                // Then create an edge from the new state to the ending state with Epsilon and rules
                                if *kleene {
                                    final_state = starting_state;
                                    edges.insert(curr_state, Symbol::Epsilon, final_state, Some(rules.clone()));
                                    curr_state = final_state;
                                } else {
                                    final_state = ending_state.unwrap_or_else(|| {
                                        // If ending state is None, then we need to create a new state
                                        new_state = curr_highest_state + 1;
                                        curr_highest_state = new_state;
                                        states.insert(new_state);
                                        new_state
                                    });
                                    edges.insert(curr_state, Symbol::Epsilon, final_state, Some(rules.clone()));
                                    curr_state = new_state;
                                }
                            },
                            RegexSymbol::Terminal(t) => {
                                let mut res_rules: Rules = rules.clone();
                                res_rules.extend(collected_rules);
                                collected_rules = Rules::new();
                                if *kleene {
                                    final_state = starting_state;
                                    edges.insert(curr_state, Symbol::Terminal(*t), final_state, Some(res_rules));
                                    curr_state = starting_state;
                                } else {
                                    final_state = ending_state.unwrap_or_else(|| {
                                        // If ending state is None, then we need to create a new state
                                        let new_state = curr_highest_state + 1;
                                        curr_highest_state = new_state;
                                        states.insert(new_state);
                                        new_state
                                    });
                                    edges.insert(curr_state, Symbol::Terminal(*t), final_state, Some(res_rules));
                                    curr_state = final_state;
                                }
                            },
                            RegexSymbol::Epsilon => {
                                return (curr_state, curr_highest_state, final_state, RulesSet::from_rules(rules.clone()))
                            },
                        }
                    }
                }
                (curr_state, curr_highest_state, final_state, RulesSet::new())
            }
        }
    }

    pub fn to_dot(&self, filename: &str) -> std::io::Result<()> {
        let mut file = File::create(format!("{}.dot", filename))?;
        write!(file, "digraph G {{\n")?;
        let mut state_to_shape: HashMap<State, &str> = HashMap::new();
        for state in self.states.iter() {
            if self.accepting_states.contains(state) {
                state_to_shape.insert(*state, "doublecircle");
            } else {
                state_to_shape.insert(*state, "circle");
            }
        }
        for state in self.states.iter() {
            write!(file, "{} [ shape={} ]\n", state, state_to_shape.get(state).unwrap())?;
        }
        for (atomic, (state, rule_set)) in self.atomic_to_state.iter() {
            match atomic.symbol {
                Symbol::Nonterminal(nonterm) => {
                    write!(file, "\"[{}]^({})\" [ shape=rectangle ]\n\"[{}]^({})\" -> {}", nonterm, atomic.terminal, nonterm, atomic.terminal, state)?;
                    if rule_set.len() > 0 {
                        write!(file, "[ label=\"")?;
                        for rules in rule_set.iter() {
                            write!(file, "(")?;
                            for rule in rules.iter() {
                                write!(file, "{}", rule)?;
                            }
                            write!(file, ")")?;
                        }
                        write!(file, "\" ]")?;
                    }
                    write!(file, "\n")?;
                },
                Symbol::Terminal(term) => write!(file, "\"[{}]^({})\" [ shape=rectangle ]\n\"[{}]^({})\" -> {}\n", term, atomic.terminal, term, atomic.terminal, state)?,
                _ => {},
            }
        }
        for (source, edge_set) in self.edges.iter() {
            for edge in edge_set.iter() {
                match edge.symbol {
                    Symbol::Epsilon => write!(file, "{} -> {} [ label=\"e ", source, edge.destination)?,
                    Symbol::Nonterminal(nonterminal) => write!(file, "{} -> {} [ label=\"{} ", source, edge.destination, nonterminal)?,
                    Symbol::Terminal(terminal) => write!(file, "{} -> {} [ label=\"{} ", source, edge.destination, terminal)?,
                }
                if let Some(rules) = &edge.opt_rules {
                    write!(file, "{}", rules)?;
                }
                write!(file, "\" ]\n")?;
            }
        }
        write!(file, "}}")
    }

    pub fn simulate(&self, curr_state: &State, symbol: Symbol) -> Option<HashSet<(&State, &Option<Rules>, bool)>> {
        let mut res: HashSet<(&State, &Option<Rules>, bool)> = HashSet::new();
        if let Some(edges) = self.edges.get(&curr_state) {
            for edge in edges.iter() {
                if edge.symbol == symbol {
                    res.insert((&edge.destination, &edge.opt_rules, self.is_accepting(&edge.destination)));
                }
            }
        }
        if res.is_empty() {
            None
        } else {
            Some(res)
        }
    }

    pub fn is_accepting(&self, curr_state: &State) -> bool {
        self.accepting_states.contains(curr_state)
    }

    pub fn get_start(&self) -> (State, bool) {
        (self.start, self.is_accepting(&self.start))
    }

    pub fn get_atomic(&self, symbol: Symbol, terminal: Terminal) -> Option<(&State, &RulesSet, bool)> {
        self.atomic_to_state.get(&Atomic::new(symbol, terminal))
            .map(|(dest, rules_set)| (dest, rules_set, self.is_accepting(dest)))
    }

    pub fn has_edge(&self, state: &State) -> bool {
        self.edges.has_edge(state)
    }

}

#[cfg(test)]
mod tests {
    use std::collections::BTreeSet;

    use crate::RegexWord;

    use super::*;

    fn execute_node_to_states_test(node: Node) {
        let mut states: States = States::new();
        let mut accepting_states: States = States::new();
        let mut edges: Edges = Edges::new();
        let start_state: State = State::new(0);
        states.insert(start_state);
        FiniteStateAutomaton::node_to_states(&node, start_state, None, start_state, &mut states, &mut accepting_states, &mut edges);
        println!("{}\n{}", states, edges);

    }

    #[test]
    fn test_node_to_states() {
        let rules: Rules = Rules::new();

        let only_term: RegexWord = RegexWord::new(vec![RegexSymbol::Terminal('a'), RegexSymbol::Terminal('b')]);
        let end_nonterm: RegexWord = RegexWord::new(vec![RegexSymbol::Terminal('a'), RegexSymbol::Nonterminal('A')]);
        let nulled: RegexWord = RegexWord::new(vec![RegexSymbol::Nulled(Rules::from_single('S', Word::from_single(Symbol::Epsilon))), RegexSymbol::Terminal('a')]);


        // Test wordnode with only terminals
        println!("Testing node: ({})", only_term);
        let node: Node = Node::new_word(only_term.clone(), rules.clone(), false);
        execute_node_to_states_test(node);

        println!("Testing node: ({})*", only_term);
        let node: Node = Node::new_word(only_term.clone(), rules.clone(), true);
        execute_node_to_states_test(node);

        // Test wordnode ending with nonterminal
        println!("Testing node: ({})", end_nonterm);
        let node: Node = Node::new_word(end_nonterm.clone(), rules.clone(), false);
        execute_node_to_states_test(node);

        println!("Testing word: ({})*", end_nonterm);
        let node: Node = Node::new_word(end_nonterm.clone(), rules.clone(), true);
        execute_node_to_states_test(node);

        // Test wordnode with nulled
        println!("Testing node: ({})", nulled);
        let node: Node = Node::new_word(nulled.clone(), rules.clone(), false);
        execute_node_to_states_test(node);

        // Test sequence node
        println!("Testing sequence: (({})({}))", only_term, only_term);
        let word_node1: Node = Node::new_word(only_term.clone(), rules.clone(), false);
        let word_node2: Node = Node::new_word(end_nonterm.clone(), rules.clone(), false);
        let node: Node = Node::new_seq(vec![word_node1.clone(), word_node1.clone()], false);
        execute_node_to_states_test(node);

        println!("Testing sequence: (({})({}))*", only_term, only_term);
        let node: Node = Node::new_seq(vec![word_node1.clone(), word_node1.clone()], true);
        execute_node_to_states_test(node);

        println!("Testing sequence: (({})({}))", only_term, end_nonterm);
        let node: Node = Node::new_seq(vec![word_node1.clone(), word_node2.clone()], false);
        execute_node_to_states_test(node);

        println!("Testing sequence: (({})({}))*", only_term, end_nonterm);
        let node: Node = Node::new_seq(vec![word_node1.clone(), word_node2.clone()], true);
        execute_node_to_states_test(node);

        // Test choice node
        println!("Testing choice: (({})+({}))", only_term, only_term);
        let node: Node = Node::new_opt(BTreeSet::from([word_node1.clone(), word_node1.clone()]), false);
        execute_node_to_states_test(node);

        println!("Testing choice: (({})+({}))*", only_term, only_term);
        let node: Node = Node::new_opt(BTreeSet::from([word_node1.clone(), word_node1.clone()]), true);
        execute_node_to_states_test(node);

        println!("Testing choice: (({})+({}))", only_term, end_nonterm);
        let node: Node = Node::new_opt(BTreeSet::from([word_node1.clone(), word_node2.clone()]), false);
        execute_node_to_states_test(node);

        println!("Testing choice: (({})+({}))*", only_term, end_nonterm);
        let node: Node = Node::new_opt(BTreeSet::from([word_node1.clone(), word_node2.clone()]), true);
        execute_node_to_states_test(node);
    }
}
