use std::collections::{HashSet, HashMap, BTreeSet, VecDeque};
use std::fmt;
use std::fs::File;
use std::io::Write;

use crate::word::{*, self};
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
    transitions: HashMap<State, HashMap<Symbol, HashSet<(State, Option<Rules>)>>>,
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
            for (symbol, destinations) in transition_list {
                for (state, _) in destinations {
                    write!(f, "|to {} via {}| ", state, symbol)?;
                }
            }
            write!(f, "\n")?;
        }
        write!(f, "Transition to rules:\n")?;
        for (state, transition_list) in &self.transitions {
            for (symbol, destinations) in transition_list {
                for (_, opt_rules) in destinations {
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
    fn new(states: HashSet<State>, accepting_states: HashSet<State>, start: State, transitions: HashMap<State, HashMap<Symbol, HashSet<(State, Option<Rules>)>>>, atomic_to_state: HashMap<(Symbol, Terminal), (State, Option<HashSet<Rules>>)>) -> Result<FiniteStateAutomaton, StateErrors> {
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
            for (symb, destinations) in transition_list {
                for (dest_state, _) in destinations {
                    if !states.contains(dest_state) {
                        return Err(StateErrors::DestNotInStates(*dest_state));
                    }
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
        let mut states: HashSet<State> = HashSet::from([start, epsilon]);
        let mut accepting_states: HashSet<State> = HashSet::from([epsilon]);
        let mut transitions: HashMap<State, HashMap<Symbol, HashSet<(State, Option<Rules>)>>> = HashMap::new();
        let mut atomic_to_state: HashMap<(Symbol, Terminal), (State, Option<HashSet<Rules>>)> = HashMap::new();

        transitions.insert(start, HashMap::from([(Symbol::Nonterminal(start_nt), HashSet::from([(epsilon, None)]))]));

        if rules.get(&start_nt).unwrap().contains(&vec![Symbol::Epsilon]) {
            accepting_states.insert(start);
        }

        // add terminal derivations to atomic_to_state
        for terminal in terminals {
            atomic_to_state.insert((Symbol::Terminal(*terminal), *terminal), (epsilon, None));
        }

        let atomic_regex: Regex = Regex::new(terminals, rules);
        let mut regex_to_state: HashMap<VecDeque<WordNode>, (State, State)> = HashMap::new();
        let mut highest_state: State = 1;

        for ((nonterminal, terminal), node) in atomic_regex.regex {
            let mut regex_to_state_key: VecDeque<WordNode> = VecDeque::new();
            let mut wordnode_queue: Vec<WordNode> = node.nodes.clone();
            let mut atomic_rules: Option<HashSet<Rules>> = None;

            println!("[{}]^({})", nonterminal, terminal);

            let mut node_end: State;
            while let Some(wordnode) = wordnode_queue.pop() {
                let prev_key = regex_to_state_key.clone();
                regex_to_state_key.push_front(wordnode.clone());

                print!("Regex_to_state: ");
                for (wordnodes, (src, dest)) in &regex_to_state {
                    for wordnode in wordnodes {
                        print!("{}", wordnode);
                    }
                    println!(": ({}, {})", src, dest);
                }
                print!("Regex_to_state_key: ");
                for wordnode in &regex_to_state_key {
                    print!("{}", wordnode);
                }
                println!();
                let node_start: State;

                if let Some((dest, end)) = regex_to_state.get(&regex_to_state_key) {
                    let (dest, end) = (*dest, *end);
                    if let (true, opt_rule_set) = wordnode.is_e_node_get_rules() {
                        atomic_rules = opt_rule_set;
                        regex_to_state_key.push_front(wordnode.clone());
                        regex_to_state.insert(regex_to_state_key.clone(), (dest, end));
                        continue;
                    } 
                    continue;
                } else {
                    if let Some((dest, end)) = regex_to_state.get(&prev_key) {
                        node_end = *dest;
                        let end = *end;
                        if let (true, opt_rule_set) = wordnode.is_e_node_get_rules() {
                            atomic_rules = opt_rule_set;
                            regex_to_state.insert(regex_to_state_key.clone(), (node_end, end));
                            continue;
                        }
                        highest_state += 1;
                        node_start = highest_state;
                    } else {
                        highest_state += 1;
                        node_start = highest_state;
                        if let (true, opt_rule_set) = wordnode.is_e_node_get_rules() {
                            atomic_rules = opt_rule_set;
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
                //if let Some(final_state) = final_end {
                //    regex_to_state.insert(regex_to_state_key.clone(), (node_start, final_state));
                //} else {
                //    regex_to_state.insert(regex_to_state_key.clone(), (node_start, node_end));
                //}

                for (rules, wordnodeword_set) in wordnode.get_by_base_rules() {
                    let mut sub_states: Vec<State> = vec![node_start];
                    for wordnodeword in wordnodeword_set {
                        if wordnodeword == vec![WordNodeSymbol::Epsilon] {
                            accepting_states.insert(node_start);
                            continue;
                        }
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
                                    let entry = transitions.entry(source).or_default().entry(Symbol::Epsilon).or_default();
                                    
                                    if source == node_start {
                                        entry.insert((target, Some([carried_rules, rules.clone()].concat())));
                                    } else {
                                        entry.insert((target, Some(carried_rules)));
                                    }
                                    source = sub_states[index+1];
                                    carried_rules = Vec::new();
                                }
                            } else {
                                let entry = transitions.entry(source).or_default().entry(
                                    match wordnodeword[index] {
                                        WordNodeSymbol::Nonterminal(nt) => Symbol::Nonterminal(nt),
                                        WordNodeSymbol::Terminal(t) => Symbol::Terminal(t),
                                        _ => continue,
                                    }
                                ).or_default();

                                target = sub_states[index+1];
                                if source == node_start {
                                    entry.insert((target, Some([carried_rules, rules.clone()].concat())));
                                    carried_rules = Vec::new();
                                } else if carried_rules.len() > 0 {
                                    entry.insert((target, Some(carried_rules)));
                                    carried_rules = Vec::new();
                                } else {
                                    entry.insert((target, None));
                                }
                                source = sub_states[index+1];
                            }
                        }
                    }
                    states.extend(sub_states);
                }


            }
            accepting_states.insert(regex_to_state.get(&regex_to_state_key).unwrap().1);
            atomic_to_state.insert((Symbol::Nonterminal(nonterminal), terminal), (regex_to_state.get(&regex_to_state_key).unwrap().0, atomic_rules));
        }

        FiniteStateAutomaton{states, accepting_states, start, transitions, atomic_to_state}
    }

    //pub fn build_fsa(terminals: &HashSet<Terminal>, start_nt: Nonterminal, rules: &HashMap<Nonterminal, HashSet<Word>>) -> FiniteStateAutomaton {

    //    for ((nonterminal, terminal), node) in atomic_regex.regex {
    //        //println!("[{}]^({}): {}", nonterminal, terminal, node);
    //        if let RegexNode::Word(wordnode) = &node {
    //            if let (true, Some(rules)) = wordnode.is_e_node_get_rules() {
    //                atomic_to_state.insert((Symbol::Nonterminal(nonterminal), terminal), (1, Some(rules)));
    //                continue;
    //            }
    //        }
    //        let (new_states, opt_new_accepting_states, start_state, end_state, new_highest_state, new_transitions, atomic_rules) = FiniteStateAutomaton::atomic_regex_to_states(&node, nonterminal, None, highest_state,  &mut regex_to_state, false);
    //        states.extend(new_states);
    //        atomic_to_state.insert((Symbol::Nonterminal(nonterminal), terminal), (start_state, atomic_rules)); // TODO EDit
    //        accepting_states.insert(end_state);
    //        if let Some(new_accepting_states) = opt_new_accepting_states {
    //            accepting_states.extend(new_accepting_states);
    //        }
    //        highest_state = new_highest_state;
    //        transitions.extend(new_transitions);
    //    }

    //    FiniteStateAutomaton::new(states, accepting_states, start, transitions, atomic_to_state).unwrap()
    //}

    //pub fn atomic_regex_to_states(node: &RegexNode, nonterminal: Nonterminal, end_state: Option<State>, highest_state: State, regex_to_state: &mut HashMap<VecDeque<RegexNode>, (State, State)>, atomic: bool) -> (HashSet<State>, Option<HashSet<State>>, State, State, State, HashMap<State, HashMap<Symbol, HashSet<(State, Option<Rules>)>>>, Option<HashSet<Rules>>) {
    //    match node {
    //        RegexNode::Node(nodenode) => {
    //            //let mut curr_state: State = curr_state;
    //            let mut start_state: State = 0;
    //            let mut transitions: HashMap<State, HashMap<Symbol, HashSet<(State, Option<Rules>)>>> = HashMap::new();
    //            let mut atomic_rules: Option<HashSet<Rules>> = None;
    //            let mut states: HashSet<State> = HashSet::new();
    //            let mut accepting_states: HashSet<State> = HashSet::new();
    //            let mut highest_state: State = highest_state;
    //            let mut regex_to_state_key: VecDeque<RegexNode> = VecDeque::new();
    //            let mut nodenodequeue: VecDeque<RegexNode> = nodenode.nodes.clone().into();
    //            let mut end_state: Option<State> = end_state;
    //            let mut final_end_state: State = 0;

    //            while let Some(n) = nodenodequeue.pop_back() {
    //                //println!("{}", n);
    //                if let RegexNode::Word(wordnode) = &n {
    //                    //println!("Rules: {:?}", wordnode);
    //                    if let (true, Some(rules)) = wordnode.is_e_node_get_rules() {
    //                        //println!("Rules: {:?}", wordnode);
    //                        atomic_rules = Some(rules);
    //                        //println!("atomic {:?}", atomic_rules);
    //                        continue;
    //                    }
    //                }
    //                regex_to_state_key.push_front(n.clone());
    //                //println!("{:?}", regex_to_state_key);
    //                if let Some((source, dest)) = regex_to_state.get(&regex_to_state_key) {
    //                    start_state = *source;
    //                    end_state = Some(*source);
    //                    if final_end_state == 0 {
    //                        final_end_state = *dest;
    //                    }
    //                } else {
    //                    let (new_states, opt_new_accepting_states, new_start_state, new_end_state, new_highest_state, new_transitions, new_atomic_rules);
    //                    if nodenodequeue.is_empty() {
    //                        if let RegexNode::Word(wordnode) = &n {
    //                            let mut e_rules_to_remove: Rules = Vec::new();
    //                            if let Some(rule_set) = wordnode.get_rules() {
    //                                let mut base_rules: Rules = Vec::new();
    //                                for rules in rule_set {
    //                                    if rules.len() < base_rules.len() || base_rules.len() == 0 {
    //                                        base_rules = rules;
    //                                    }
    //                                }
    //                                for rule in &base_rules {
    //                                    if rule.1 == vec![Symbol::Epsilon] {
    //                                        e_rules_to_remove.push(rule.clone());
    //                                    }
    //                                }
    //                                atomic_rules = Some(HashSet::from([base_rules]));
    //                            }
    //                            let mut new_wordnode = wordnode.clone();
    //                            let mut new_words: BTreeSet<(Word, Rules)> = BTreeSet::new();
    //                            //println!("{}", new_wordnode);
    //                            for (old_word, old_rules) in &wordnode.words {
    //                                let mut new_rules: Rules = old_rules.clone();
    //                                for _ in 0..e_rules_to_remove.len() {
    //                                    new_rules.remove(0);
    //                                }
    //                                new_words.insert((old_word.clone(), new_rules));
    //                            }
    //                            new_wordnode.words = new_words;
    //                            //println!("{}", new_wordnode);
    //                            (new_states, opt_new_accepting_states, new_start_state, new_end_state, new_highest_state, new_transitions, new_atomic_rules) = FiniteStateAutomaton::atomic_regex_to_states(&RegexNode::Word(new_wordnode), nonterminal, end_state, highest_state, regex_to_state, true);
    //                        } else {
    //                            (new_states, opt_new_accepting_states, new_start_state, new_end_state, new_highest_state, new_transitions, new_atomic_rules) = FiniteStateAutomaton::atomic_regex_to_states(&n, nonterminal, end_state, highest_state, regex_to_state, true);
    //                        }
    //                    } else {
    //                        (new_states, opt_new_accepting_states, new_start_state, new_end_state, new_highest_state, new_transitions, new_atomic_rules) = FiniteStateAutomaton::atomic_regex_to_states(&n, nonterminal, end_state, highest_state, regex_to_state, false);
    //                    }
    //                    start_state = new_start_state;
    //                    states.extend(new_states);
    //                    if let Some(new_accepting_states) = opt_new_accepting_states {
    //                        accepting_states.extend(new_accepting_states);
    //                    }
    //                    end_state = Some(new_start_state);
    //                    highest_state = new_highest_state;
    //                    transitions.extend(new_transitions);
    //                    if atomic_rules.is_none() {
    //                        if let Some(rules) = new_atomic_rules {
    //                            atomic_rules = Some(rules);
    //                        }
    //                    }
    //                    if final_end_state == 0 {
    //                        final_end_state = new_end_state;
    //                    }
    //                    regex_to_state.insert(regex_to_state_key.clone(), (new_start_state, final_end_state));
    //                }
    //            }
    //            //println!("{:?}, {}, {}, {}, {:?}", states, start_state, final_end_state, highest_state, transitions);
    //            //println!("end_atomic_rules: {:?}", &atomic_rules);
    //            (states, Some(accepting_states), start_state, final_end_state, highest_state, transitions, atomic_rules)
    //        },
    //        RegexNode::Word(wordnode) => {
    //            if let Some(state) = end_state {
    //                if wordnode.kleene_star {
    //                    let (states, start_state, end_state, highest_state, transitions) = FiniteStateAutomaton::word_node_to_states(wordnode, nonterminal, state, state, highest_state + 1, atomic);
    //                    (states, None, start_state, end_state, highest_state, transitions, None)
    //                } else {
    //                    let (states, start_state, end_state, highest_state, transitions) = FiniteStateAutomaton::word_node_to_states(wordnode, nonterminal, highest_state + 1, state, highest_state + 1, atomic);
    //                    (states, None, start_state, end_state, highest_state, transitions, None)
    //                }
    //                //(states, start_state, end_state, highest_state, transitions)
    //            } else {
    //                let (states, start_state, end_state, highest_state, transitions) = FiniteStateAutomaton::word_node_to_states(wordnode, nonterminal, highest_state + 1, 0, highest_state + 1, atomic);
    //                (states, None, start_state, end_state, highest_state, transitions, None)
    //            }
    //        },
    //    }
    //    //unimplemented!()
    //}

    //fn word_node_to_states(node: &WordNode, nonterminal: Nonterminal, curr_state: State, end_state: State, highest_state: State, atomic: bool) -> (HashSet<State>, State, State, State, HashMap<State, HashMap<Symbol, HashSet<(State, Option<Rules>)>>>) {
    //    //println!("{:?}, {}, {}, {}", node, curr_state, end_state, highest_state);
    //    let start_state: State = curr_state;
    //    let mut end_state: State = end_state;
    //    let mut states: HashSet<State> = HashSet::from([start_state]);
    //    let mut transitions: HashMap<State, HashMap<Symbol, HashSet<(State, Option<Rules>)>>> = HashMap::new();

    //    let mut highest_state: State = highest_state;
    //    let mut curr_state: State;

    //    if node.kleene_star {
    //        end_state = start_state;
    //    }

    //    let mut base_rule_to_states: HashMap<Rules, HashSet<State>> = HashMap::new();

    //    for (word, rules) in node.words.iter() {
    //        //println!("Word: {:?}", word);
    //        //println!("Rules: {:?}", rules);
    //        curr_state = start_state;

    //        //let base_rule = rules.last().unwrap();
    //        let mut base_rules: Rules = Vec::new();
    //        let mut e_rules_to_apply: Rules = rules.clone();
    //        for rule in rules {
    //            if rule.1 != vec![Symbol::Epsilon] {
    //                base_rules.push(rule.clone());
    //            } else {
    //                e_rules_to_apply.push(rule.clone());
    //            }
    //        }
    //        let mut upcoming_base_rule_chars: VecDeque<Symbol> = VecDeque::new();
    //        for (_, chars) in &base_rules {
    //            let mut char_iter = chars.clone().into_iter();
    //            char_iter.next();
    //            upcoming_base_rule_chars.extend(char_iter);
    //        }

    //        //println!("Base rules: {:?}", base_rules);
    //        //println!("Start upcoming: {:?}", upcoming_base_rule_chars);
    //        let mut peekable_word = word.iter().peekable();

    //        if upcoming_base_rule_chars.get(0) == Some(&Symbol::Nonterminal(nonterminal)) && peekable_word.peek() != Some(&&Symbol::Nonterminal(nonterminal)) {
    //            upcoming_base_rule_chars.pop_front();
    //        }


    //        if let Some(base_rule_states) = base_rule_to_states.get(&base_rules) {
    //            let mut base_start: Option<State> = None;
    //            transitions.get(&curr_state).map(
    //                |symb_to_dest| 
    //                if let Some(symb) = peekable_word.peek() {
    //                    symb_to_dest.get(symb).map(
    //                        |dest_rules_set|

    //                        for (state, _) in dest_rules_set {
    //                            if base_rule_states.contains(state) {
    //                                if base_start.is_some() {
    //                                    panic!()
    //                                }
    //                                base_start = Some(*state);
    //                            }
    //                        }
    //                    );
    //                }

    //            );

    //            if let Some(state) = base_start {
    //                curr_state = state;
    //                peekable_word.next();
    //                upcoming_base_rule_chars.pop_front();
    //            } else {
    //                if let Some(symbol) = peekable_word.next() {
    //                    upcoming_base_rule_chars.pop_front();
    //                    let mut applying_rules: VecDeque<Rule> = VecDeque::new();
    //                    if !atomic {
    //                        applying_rules.extend(base_rules.clone());
    //                    } else {
    //                        upcoming_base_rule_chars.pop_front();
    //                    }
    //                    //println!("Applying rules: {:?}", applying_rules);
    //                    //println!("Upcoming base char: {}", upcoming_base_rule_chars[0]);
    //                    //println!("Symbol: {}", symbol);
    //                    while upcoming_base_rule_chars[0] != *symbol {
    //                        upcoming_base_rule_chars.pop_front();
    //                        applying_rules.push_front(e_rules_to_apply.pop().unwrap());
    //                    }
    //                    upcoming_base_rule_chars.pop_front();
    //                    //println!("Applying rules: {:?}", applying_rules);
    //                    if peekable_word.peek() != None {
    //                        highest_state += 1;
    //                        transitions.entry(curr_state).or_insert(HashMap::new()).entry(*symbol).or_insert(HashSet::new()).insert((highest_state, Some(applying_rules.into())));
    //                        states.insert(highest_state);
    //                        curr_state = highest_state;
    //                        base_rule_to_states.entry(base_rules.clone()).or_insert(HashSet::new()).insert(curr_state);
    //                    } else {
    //                        transitions.entry(curr_state).or_insert(HashMap::new()).entry(*symbol).or_insert(HashSet::new()).insert((end_state, Some(applying_rules.into())));
    //                    }
    //                }
    //            }
    //        } else {
    //            if let Some(symbol) = peekable_word.next() {
    //                let mut applying_rules: VecDeque<Rule> = VecDeque::new();
    //                if !atomic {
    //                    applying_rules.extend(base_rules.clone());
    //                } else {
    //                    upcoming_base_rule_chars.pop_front();
    //                }
    //                //println!("Applying rules: {:?}", applying_rules);
    //                //println!("Upcoming base char: {}", upcoming_base_rule_chars[0]);
    //                //println!("Symbol: {}", symbol);
    //                while upcoming_base_rule_chars.get(0).is_some() && upcoming_base_rule_chars.get(0) != Some(symbol) {
    //                    upcoming_base_rule_chars.pop_front();
    //                    applying_rules.push_front(e_rules_to_apply.pop().unwrap());
    //                }
    //                upcoming_base_rule_chars.pop_front();
    //                //println!("Applying rules: {:?}", applying_rules);
    //                if peekable_word.peek() != None {
    //                    highest_state += 1;
    //                    transitions.entry(curr_state).or_insert(HashMap::new()).entry(*symbol).or_insert(HashSet::new()).insert((highest_state, Some(applying_rules.into())));
    //                    states.insert(highest_state);
    //                    curr_state = highest_state;
    //                    base_rule_to_states.entry(base_rules.clone()).or_insert(HashSet::new()).insert(curr_state);
    //                } else {
    //                    transitions.entry(curr_state).or_insert(HashMap::new()).entry(*symbol).or_insert(HashSet::new()).insert((end_state, Some(applying_rules.into())));
    //                }
    //            }
    //        }

    //        'word: while let Some(symbol) = peekable_word.next() {
    //            if let Symbol::Epsilon = symbol {
    //                continue;
    //            }
    //            let mut applying_rules: VecDeque<Rule> = VecDeque::new();
    //            //println!("symb: {}", symbol);
    //            //println!("upcoming: {:?}", upcoming_base_rule_chars);
    //            while upcoming_base_rule_chars.get(0).is_some() && upcoming_base_rule_chars.get(0) != Some(symbol) {
    //                //println!("upcoming: {:?}, so skipping", upcoming_base_rule_chars.get(0));
    //                upcoming_base_rule_chars.pop_front();
    //                applying_rules.push_front(e_rules_to_apply.pop().unwrap());
    //            }
    //            upcoming_base_rule_chars.pop_front();
    //            if let Some(transition_list) = transitions.get(&curr_state) {
    //                for (transition_symbol, dest_list) in transition_list {
    //                    for (dest, _) in dest_list {
    //                        if transition_symbol == symbol {
    //                            upcoming_base_rule_chars.pop_front();
    //                            curr_state = *dest;
    //                            continue 'word;
    //                        }
    //                    }
    //                }
    //            }
    //            if peekable_word.peek() != None {
    //                upcoming_base_rule_chars.pop_front();
    //                highest_state += 1;
    //                if !applying_rules.is_empty() {
    //                    transitions.entry(curr_state).or_insert(HashMap::new()).entry(*symbol).or_insert(HashSet::new()).insert((highest_state, Some(applying_rules.into())));
    //                } else {
    //                    transitions.entry(curr_state).or_insert(HashMap::new()).entry(*symbol).or_insert(HashSet::new()).insert((highest_state, None));
    //                }
    //                states.insert(highest_state);
    //                curr_state = highest_state;
    //            } else {
    //                upcoming_base_rule_chars.pop_front();
    //                if end_state == 0 {
    //                    highest_state += 1;
    //                    end_state = highest_state;
    //                    states.insert(end_state);
    //                }
    //                if !applying_rules.is_empty() {
    //                    transitions.entry(curr_state).or_insert(HashMap::new()).entry(*symbol).or_insert(HashSet::new()).insert((end_state, Some(applying_rules.into())));
    //                } else {
    //                    transitions.entry(curr_state).or_insert(HashMap::new()).entry(*symbol).or_insert(HashSet::new()).insert((end_state, None));
    //                }
    //            }
    //            
    //        }
    //    }

    //    (states, start_state, end_state, highest_state, transitions)
    //}

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
            for (symbol, destinations) in transition_list {
                for (dest, opt_rules) in destinations {
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
        }
        write!(file, "}}")
    }

    pub fn simulate(&self, curr_state: &State, symbol: Symbol) -> Option<HashSet<(&State, &Option<Rules>, bool)>> {
        self.transitions.get(&curr_state)?
            .get(&symbol)
            .map_or(None, |destinations| {
                let mut res: HashSet<(&State, &Option<Rules>, bool)> = HashSet::new();
                for (dest, opt_rules) in destinations {
                    res.insert((dest, opt_rules, self.is_accepting(dest)));
                }
                Some(res)
            })
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