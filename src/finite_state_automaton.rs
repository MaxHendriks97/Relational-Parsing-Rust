use std::collections::{HashSet, HashMap, BTreeSet, VecDeque};
use std::fmt;
use std::fs::File;
use std::io::Write;

use crate::word::*;
use crate::regex::*;
use crate::grammar::*;

pub type State = u32;

enum StateErrors {
    StartNotInStates(State),
    AccNotInStates(State),
    SrcNotInStates(State),
    DestNotInStates(State),
    AtomicStateNotInStates(State),
}


pub struct FiniteStateAutomaton {
    states: HashSet<State>,
    accepting_states: HashSet<State>,
    start: State,
    transitions: HashMap<State, HashSet<(Symbol, State)>>,
    atomic_to_state: HashMap<(Symbol, Terminal), State>,
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
        write!(f, "Transitions:\n")?;
        for (state, transition_list) in &self.transitions {
            write!(f, "{}: ", state)?;
            for transition in transition_list {
                write!(f, "|to {} via {}| ", transition.1, transition.0)?;
            }
            write!(f, "\n")?;
        }
        write!(f, "\nAtomic to state: ")?;
        for ((symbol, terminal), state) in &self.atomic_to_state {
            write!(f, "|[{}]^({}) {}| ", symbol, terminal, state)?;
        }
        Ok(())
    }
}

impl FiniteStateAutomaton {
    fn new(states: HashSet<State>, accepting_states: HashSet<State>, start: State, transitions: HashMap<State, HashSet<(Symbol, State)>>, atomic_to_state: HashMap<(Symbol, Terminal), State>) -> Result<FiniteStateAutomaton, StateErrors> {
        if !states.contains(&start) {
            return Err(StateErrors::StartNotInStates(start));
        }
        for acc_state in &accepting_states {
            if !states.contains(acc_state) {
                return Err(StateErrors::AccNotInStates(*acc_state));
            }
        }
        for src_state in transitions.keys() {
            if !states.contains(src_state) {
                return Err(StateErrors::SrcNotInStates(*src_state));
            }
        }
        for transition_list in transitions.values() {
            for (symb, dest_state) in transition_list {
                if !states.contains(dest_state) {
                    return Err(StateErrors::DestNotInStates(*dest_state));
                }
            }
        }
        for atomic_state in atomic_to_state.values() {
            if !states.contains(atomic_state) {
                return Err(StateErrors::AtomicStateNotInStates(*atomic_state));
            }
        }
        Ok(FiniteStateAutomaton{states, accepting_states, start, transitions, atomic_to_state})
    }

    pub fn build_fsa(terminals: &HashSet<Terminal>, start_nt: Symbol, rules: &HashMap<Nonterminal, HashSet<Word>>) -> FiniteStateAutomaton {
        let start: State = 0;
        let epsilon: State = 1;
        let mut states: HashSet<State> = HashSet::from([start, epsilon]); // All states in the atomic language, contains at least start state Sigma_epsilon (state 0) and state epsilon (state 1)
        let mut accepting_states: HashSet<State> = HashSet::from([epsilon]); // All states containing symbol epsilon, state 1 is epsilon itself
        let mut transitions: HashMap<State, HashSet<(Symbol, State)>> = HashMap::new();

        // Add transition from Sigma_epsilon to epsilon by start symbol
        transitions.insert(start, HashSet::from([(start_nt, epsilon)]));

        let mut highest_state: State = 1;
        let mut atomic_to_state: HashMap<(Symbol, Terminal), State> = HashMap::new();

        // add terminal derivations to atomic_to_state
        for terminal in terminals {
            atomic_to_state.insert((Symbol::Terminal(*terminal), *terminal), epsilon);
        }

        let atomic_regex: Regex = Regex::new(terminals, rules);
        let mut regex_to_state: HashMap<VecDeque<RegexNode>, (State, State)> = HashMap::new();

        for ((nonterminal, terminal), node) in atomic_regex.regex {
            if node == RegexNode::Word(WordNode{words: BTreeSet::from([vec![Symbol::Epsilon]]), kleene_star: false}) {
                atomic_to_state.insert((Symbol::Nonterminal(nonterminal), terminal), 1);
                continue;
            }
            let (new_states, start_state, end_state, new_highest_state, new_transitions) = FiniteStateAutomaton::atomic_regex_to_states(&node, None, highest_state,  &mut regex_to_state);
            states.extend(new_states);
            atomic_to_state.insert((Symbol::Nonterminal(nonterminal), terminal), start_state);
            accepting_states.insert(end_state);
            highest_state = new_highest_state;
            transitions.extend(new_transitions);
        }

        FiniteStateAutomaton{states, accepting_states, start, transitions, atomic_to_state}
    }

    pub fn atomic_regex_to_states(node: &RegexNode, end_state: Option<State>, highest_state: State, regex_to_state: &mut HashMap<VecDeque<RegexNode>, (State, State)>) -> (HashSet<State>, State, State, State, HashMap<State, HashSet<(Symbol, State)>>) {
        match node {
            RegexNode::Node(nodenode) => {
                //let mut curr_state: State = curr_state;
                let mut start_state: State = 0;
                let mut transitions: HashMap<State, HashSet<(Symbol, State)>> = HashMap::new();
                let mut states: HashSet<State> = HashSet::new();
                let mut highest_state: State = highest_state;
                let mut regex_to_state_key: VecDeque<RegexNode> = VecDeque::new();
                let mut nodenodequeue: VecDeque<RegexNode> = nodenode.nodes.clone().into();
                let mut end_state: Option<State> = end_state;
                let mut final_end_state: State = 0;

                while let Some(n) = nodenodequeue.pop_back() {
                    if n.is_e_node() {
                        continue;
                    }
                    regex_to_state_key.push_front(n.clone());
                    //println!("{:?}", regex_to_state_key);
                    if let Some((source, dest)) = regex_to_state.get(&regex_to_state_key) {
                        start_state = *source;
                        end_state = Some(*source);
                        if final_end_state == 0 {
                            final_end_state = *dest;
                        }
                    } else {
                        let (new_states, new_start_state, new_end_state, new_highest_state, new_transitions) = FiniteStateAutomaton::atomic_regex_to_states(&n, end_state, highest_state, regex_to_state);
                        start_state = new_start_state;
                        states.extend(new_states);
                        end_state = Some(new_start_state);
                        highest_state = new_highest_state;
                        transitions.extend(new_transitions);
                        if final_end_state == 0 {
                            final_end_state = new_end_state;
                        }
                        regex_to_state.insert(regex_to_state_key.clone(), (new_start_state, final_end_state));
                    }
                }
                //println!("{:?}, {}, {}, {}, {:?}", states, start_state, final_end_state, highest_state, transitions);
                (states, start_state, final_end_state, highest_state, transitions)
            },
            RegexNode::Word(wordnode) => {
                if let Some(state) = end_state {
                    FiniteStateAutomaton::word_node_to_states(wordnode, highest_state + 1, state, highest_state + 1)
                    //(states, start_state, end_state, highest_state, transitions)
                } else {
                    FiniteStateAutomaton::word_node_to_states(wordnode, highest_state + 1, highest_state + 1, highest_state + 1)
                    //(states, start_state, end_state, highest_state, transitions)
                }
            },
        }
        //unimplemented!()
    }

    fn word_node_to_states(node: &WordNode, curr_state: State, end_state: State, highest_state: State) -> (HashSet<State>, State, State, State, HashMap<State, HashSet<(Symbol, State)>>) {
        //println!("{:?}, {}, {}, {}", node, curr_state, end_state, highest_state);
        let start_state: State = curr_state;
        let mut end_state: State = end_state;
        let mut states: HashSet<State> = HashSet::from([start_state]);
        let mut transitions: HashMap<State, HashSet<(Symbol, State)>> = HashMap::new();

        let mut highest_state: State = highest_state;
        let mut curr_state: State;
        // TODO: move to calling function
        if node.kleene_star {
            end_state = start_state;
        }

        for word in node.words.iter() {
            curr_state = start_state;
            let mut peekable_word = word.iter().peekable();
            'word: while let Some(symbol) = peekable_word.next() {
                if let Symbol::Epsilon = symbol {
                    continue;
                } else if let Some(transition_list) = transitions.get(&curr_state) {
                    for (transition_symbol, dest) in transition_list {
                        if transition_symbol == symbol {
                            curr_state = *dest;
                            continue 'word;
                        }
                    }
                }
                if peekable_word.peek() != None {
                    highest_state += 1;
                    transitions.entry(curr_state).or_insert(HashSet::new()).insert((*symbol, highest_state));
                    states.insert(highest_state);
                    curr_state = highest_state;
                } else {
                    if end_state == 0 {
                        highest_state += 1;
                        end_state = highest_state;
                        states.insert(end_state);
                    }
                    transitions.entry(curr_state).or_insert(HashSet::new()).insert((*symbol, end_state));
                }
                
            }
        }
        if end_state == 0 {
            end_state = start_state;
        }

        //println!("{:?}, {}, {}, {}, {:?}", states, start_state, end_state, highest_state, transitions);
        (states, start_state, end_state, highest_state, transitions)
    }

    pub fn to_dot(&self, filename: &str) -> std::io::Result<()> {
        let mut file = File::create(format!("{}.dot", filename))?;
        write!(file, "digraph G {{\n")?;
        //write!(file, "{} [ label=\"start\" ]\n", &self.start)?;
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
        for ((symbol, terminal), state) in &self.atomic_to_state {
            match symbol {
                Symbol::Nonterminal(nonterm) => write!(file, "\"[{}]^({})\" [ shape=rectangle ]\n\"[{}]^({})\" -> {}\n", nonterm, terminal, nonterm, terminal, state)?,
                Symbol::Terminal(term) => write!(file, "\"[{}]^({})\" [ shape=rectangle ]\n\"[{}]^({})\" -> {}\n", term, terminal, term, terminal, state)?,
                _ => {},
            }
        }
        for (source, transition_list) in &self.transitions {
            for (symbol, dest) in transition_list {
                match symbol {
                    Symbol::Epsilon => write!(file, "{} -> {} [ label=\"e\" ]\n", source, dest)?,
                    Symbol::Nonterminal(nonterminal) => write!(file, "{} -> {} [ label=\"{}\" ]\n", source, dest, nonterminal)?,
                    Symbol::Terminal(terminal) => write!(file, "{} -> {} [ label=\"{}\" ]\n", source, dest, terminal)?,
                }
            }
        }
        write!(file, "}}")
    }
}