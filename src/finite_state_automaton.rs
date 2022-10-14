use std::collections::{HashSet, HashMap, BTreeSet, VecDeque};
use std::fmt;
use std::fs::File;
use std::io::Write;

use crate::word::*;
use crate::regex::*;

pub type State = u32;

#[derive(Debug)]
enum StateErrors {
    StartNotInStates(State),
    AccNotInStates(State),
    SrcNotInStates(State),
    DestNotInStates(State),
    AtomicStateNotInStates(State),
}


#[derive(Debug)]
pub struct FiniteStateAutomaton {
    states: HashSet<State>,
    accepting_states: HashSet<State>,
    start: State,
    transitions: HashMap<State, HashMap<Symbol, (State, Option<Rules>)>>,
    atomic_to_state: HashMap<(Symbol, Terminal), (State, Option<HashSet<Rules>>)>,
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
            for (symbol, (state, _)) in transition_list {
                write!(f, "|to {} via {}| ", state, symbol)?;
            }
            write!(f, "\n")?;
        }
        write!(f, "Transition to rules:\n")?;
        for (state, transition_list) in &self.transitions {
            for (symbol, (_, opt_rules)) in transition_list {
                if let Some(rules) = opt_rules {
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
        for ((symbol, terminal), (state, opt_rules)) in &self.atomic_to_state {
            write!(f, "[{}]^({}) {} ", symbol, terminal, state)?;
            if let Some(rule_set) = opt_rules {
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
           }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

impl FiniteStateAutomaton {
    fn new(states: HashSet<State>, accepting_states: HashSet<State>, start: State, transitions: HashMap<State, HashMap<Symbol, (State, Option<Rules>)>>, atomic_to_state: HashMap<(Symbol, Terminal), (State, Option<HashSet<Rules>>)>) -> Result<FiniteStateAutomaton, StateErrors> {
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
            for (symb, (dest_state, _)) in transition_list {
                if !states.contains(dest_state) {
                    return Err(StateErrors::DestNotInStates(*dest_state));
                }
            }
        }
        for (atomic_state, _) in atomic_to_state.values() {
            if !states.contains(atomic_state) {
                return Err(StateErrors::AtomicStateNotInStates(*atomic_state));
            }
        }
        Ok(FiniteStateAutomaton{states, accepting_states, start, transitions, atomic_to_state})
    }

    pub fn build_fsa(terminals: &HashSet<Terminal>, start_nt: Nonterminal, rules: &HashMap<Nonterminal, HashSet<Word>>) -> FiniteStateAutomaton {
        let start: State = 0;
        let epsilon: State = 1;
        let mut states: HashSet<State> = HashSet::from([start, epsilon]); // All states in the atomic language, contains at least start state Sigma_epsilon (state 0) and state epsilon (state 1)
        let mut accepting_states: HashSet<State> = HashSet::from([epsilon]); // All states containing symbol epsilon, state 1 is epsilon itself
        let mut atomic_to_state: HashMap<(Symbol, Terminal), (State, Option<HashSet<Rules>>)> = HashMap::new();
        let mut transitions: HashMap<State, HashMap<Symbol, (State, Option<Rules>)>> = HashMap::new();

        // Add transition from Sigma_epsilon to epsilon by start symbol
        transitions.insert(start, HashMap::from([(Symbol::Nonterminal(start_nt), (epsilon, None))]));

        if rules.get(&start_nt).unwrap().contains(&vec![Symbol::Epsilon]) {
            accepting_states.insert(start);
        }

        let mut highest_state: State = 1;

        // add terminal derivations to atomic_to_state
        for terminal in terminals {
            atomic_to_state.insert((Symbol::Terminal(*terminal), *terminal), (epsilon, None));
        }

        let atomic_regex: Regex = Regex::new(terminals, rules);
        let mut regex_to_state: HashMap<VecDeque<RegexNode>, (State, State)> = HashMap::new();

        for ((nonterminal, terminal), node) in atomic_regex.regex {
            println!("[{}]^({}): {}", nonterminal, terminal, node);
            if let RegexNode::Word(wordnode) = &node {
                if let (true, Some(rules)) = wordnode.is_e_node_get_rules() {
                    atomic_to_state.insert((Symbol::Nonterminal(nonterminal), terminal), (1, Some(rules)));
                    continue;
                }
            }
            let (new_states, opt_new_accepting_states, start_state, end_state, new_highest_state, new_transitions, atomic_rules) = FiniteStateAutomaton::atomic_regex_to_states(&node, None, highest_state,  &mut regex_to_state);
            states.extend(new_states);
            atomic_to_state.insert((Symbol::Nonterminal(nonterminal), terminal), (start_state, atomic_rules)); // TODO EDit
            accepting_states.insert(end_state);
            if let Some(new_accepting_states) = opt_new_accepting_states {
                accepting_states.extend(new_accepting_states);
            }
            highest_state = new_highest_state;
            transitions.extend(new_transitions);
        }

        FiniteStateAutomaton::new(states, accepting_states, start, transitions, atomic_to_state).unwrap()
    }

    pub fn atomic_regex_to_states(node: &RegexNode, end_state: Option<State>, highest_state: State, regex_to_state: &mut HashMap<VecDeque<RegexNode>, (State, State)>) -> (HashSet<State>, Option<HashSet<State>>, State, State, State, HashMap<State, HashMap<Symbol, (State, Option<Rules>)>>, Option<HashSet<Rules>>) {
        match node {
            RegexNode::Node(nodenode) => {
                //let mut curr_state: State = curr_state;
                let mut start_state: State = 0;
                let mut transitions: HashMap<State, HashMap<Symbol, (State, Option<Rules>)>> = HashMap::new();
                let mut atomic_rules: Option<HashSet<Rules>> = None;
                let mut states: HashSet<State> = HashSet::new();
                let mut accepting_states: HashSet<State> = HashSet::new();
                let mut highest_state: State = highest_state;
                let mut regex_to_state_key: VecDeque<RegexNode> = VecDeque::new();
                let mut nodenodequeue: VecDeque<RegexNode> = nodenode.nodes.clone().into();
                let mut end_state: Option<State> = end_state;
                let mut final_end_state: State = 0;

                while let Some(n) = nodenodequeue.pop_back() {
                    println!("{}", n);
                    if let RegexNode::Word(wordnode) = &n {
                        println!("Rules: {:?}", wordnode);
                        if let (true, Some(rules)) = wordnode.is_e_node_get_rules() {
                            //println!("Rules: {:?}", wordnode);
                            atomic_rules = Some(rules);
                            //println!("atomic {:?}", atomic_rules);
                            continue;
                        }
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
                        let (new_states, opt_new_accepting_states, new_start_state, new_end_state, new_highest_state, new_transitions, new_atomic_rules) = FiniteStateAutomaton::atomic_regex_to_states(&n, end_state, highest_state, regex_to_state);
                        start_state = new_start_state;
                        states.extend(new_states);
                        if let Some(new_accepting_states) = opt_new_accepting_states {
                            accepting_states.extend(new_accepting_states);
                        }
                        end_state = Some(new_start_state);
                        highest_state = new_highest_state;
                        transitions.extend(new_transitions);
                        if let Some(rules) = new_atomic_rules {
                            atomic_rules = Some(rules);
                        }
                        if final_end_state == 0 {
                            final_end_state = new_end_state;
                        }
                        regex_to_state.insert(regex_to_state_key.clone(), (new_start_state, final_end_state));
                    }
                }
                //println!("{:?}, {}, {}, {}, {:?}", states, start_state, final_end_state, highest_state, transitions);
                //println!("{:?}", &atomic_rules);
                (states, Some(accepting_states), start_state, final_end_state, highest_state, transitions, atomic_rules)
            },
            RegexNode::Word(wordnode) => {
                if let Some(state) = end_state {
                    if wordnode.kleene_star {
                        let (states, start_state, end_state, highest_state, transitions) = FiniteStateAutomaton::word_node_to_states(wordnode, state, state, highest_state + 1);
                        (states, None, start_state, end_state, highest_state, transitions, None)
                    } else {
                        let (states, start_state, end_state, highest_state, transitions) = FiniteStateAutomaton::word_node_to_states(wordnode, highest_state + 1, state, highest_state + 1);
                        (states, None, start_state, end_state, highest_state, transitions, None)
                    }
                    //(states, start_state, end_state, highest_state, transitions)
                } else {
                    let (states, start_state, end_state, highest_state, transitions) = FiniteStateAutomaton::word_node_to_states(wordnode, highest_state + 1, 0, highest_state + 1);
                    (states, None, start_state, end_state, highest_state, transitions, None)
                }
            },
        }
        //unimplemented!()
    }

    //TODO: When word == vec![Symbol::Epsilon], rules needs to be applied to atomic language call instead of transition
    fn word_node_to_states(node: &WordNode, curr_state: State, end_state: State, highest_state: State) -> (HashSet<State>, State, State, State, HashMap<State, HashMap<Symbol, (State, Option<Rules>)>>) {
        //println!("{:?}, {}, {}, {}", node, curr_state, end_state, highest_state);
        let start_state: State = curr_state;
        let mut end_state: State = end_state;
        let mut states: HashSet<State> = HashSet::from([start_state]);
        let mut transitions: HashMap<State, HashMap<Symbol, (State, Option<Rules>)>> = HashMap::new();

        let mut highest_state: State = highest_state;
        let mut curr_state: State;
        // TODO: move to calling function
        if node.kleene_star {
            end_state = start_state;
        }

        for (word, rules) in node.words.iter() {
            //if word == &vec![Symbol::Epsilon] {
            //    println!("Rules  IMPORTANT: {:?}", rules);
            //    accepting_states.insert(start_state);
            //    atomic_rules = Some(HashSet::from([rules.clone()]));
            //    continue;
            //}
            curr_state = start_state;
            let mut rule_applied: bool = false;
            let mut dragged_rules: Option<Rules> = None;
            let mut peekable_word = word.iter().peekable();
            'word: while let Some(symbol) = peekable_word.next() {
                if let Symbol::Epsilon = symbol {
                    continue;
                } else if let Some(transition_list) = transitions.get_mut(&curr_state) {
                    for (transition_symbol, (dest, opt_prev_rules)) in transition_list {
                        if transition_symbol == symbol {
                            //Drag previously applied rule to new location
                            dragged_rules = opt_prev_rules.take();
                            curr_state = *dest;
                            continue 'word;
                        }
                    }
                }
                //place dragged rule
                if let (Some(transition_list), Some(_)) = (transitions.get_mut(&curr_state), &mut dragged_rules) {
                    for (_, (_, opt_rules)) in transition_list {
                        *opt_rules = dragged_rules.take();
                    }
                }
                if peekable_word.peek() != None {
                    highest_state += 1;
                    if !rule_applied {
                        transitions.entry(curr_state).or_insert(HashMap::new()).insert(*symbol, (highest_state, Some(rules.clone())));
                        rule_applied = true;
                    } else {
                        transitions.entry(curr_state).or_insert(HashMap::new()).insert(*symbol, (highest_state, None));
                    }
                    states.insert(highest_state);
                    curr_state = highest_state;
                } else {
                    if end_state == 0 {
                        highest_state += 1;
                        end_state = highest_state;
                        states.insert(end_state);
                    }
                    if !rule_applied {
                        transitions.entry(curr_state).or_insert(HashMap::new()).insert(*symbol, (highest_state, Some(rules.clone())));
                        rule_applied = true;
                    } else {
                        transitions.entry(curr_state).or_insert(HashMap::new()).insert(*symbol, (end_state, None));
                    }
                }
                
            }
        }

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
        for ((symbol, terminal), (state, opt_rule_set)) in &self.atomic_to_state {
            match symbol {
                Symbol::Nonterminal(nonterm) => {
                    write!(file, "\"[{}]^({})\" [ shape=rectangle ]\n\"[{}]^({})\" -> {}", nonterm, terminal, nonterm, terminal, state)?;
                    if let Some(rule_set) = opt_rule_set {
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
        for (source, transition_list) in &self.transitions {
            for (symbol, (dest, opt_rules)) in transition_list {
                match symbol {
                    Symbol::Epsilon => write!(file, "{} -> {} [ label=\"e ", source, dest)?,
                    Symbol::Nonterminal(nonterminal) => write!(file, "{} -> {} [ label=\"{} ", source, dest, nonterminal)?,
                    Symbol::Terminal(terminal) => write!(file, "{} -> {} [ label=\"{} ", source, dest, terminal)?,
                }
                if let Some(rules) = opt_rules {
                    for rule in rules {
                        write!(file, "[{} -> ", &rule.0)?;
                        for symbol in &rule.1 {
                            write!(file, "{}", symbol)?;
                        }
                        write!(file, "] ")?;
                    }
                }
                write!(file, "\" ]\n")?;
            }
        }
        write!(file, "}}")
    }

    pub fn simulate(&self, curr_state: &State, symbol: Symbol) -> Option<(&State, &Option<Rules>, bool)> {
        self.transitions.get(&curr_state)?
            .get(&symbol)
            .map_or(None, |(dest, opt_rules)| Some((dest, opt_rules, self.is_accepting(dest))))
    }

    pub fn is_accepting(&self, curr_state: &State) -> bool {
        self.accepting_states.contains(curr_state)
    }

    pub fn get_start(&self) -> (State, bool) {
        (self.start, self.is_accepting(&self.start))
    }

    pub fn get_atomic(&self, symbol: Symbol, terminal: Terminal) -> Option<(&State, &Option<HashSet<Rules>>, bool)> {
        self.atomic_to_state.get(&(symbol, terminal))
            .map_or(None, |(dest, opt_rules_set)| Some((dest, opt_rules_set, self.is_accepting(dest))))
    }

    pub fn has_transition(&self, curr_state: &State) -> bool {
        self.transitions.get(curr_state)
            .map_or(false, |trans_list| !trans_list.is_empty())
    }

}