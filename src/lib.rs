use std::collections::HashSet;
use std::collections::HashMap;
use std::collections::VecDeque;
use std::fs::File;
use std::hash::Hash;
use std::io::Write;
use std::fmt;

type State = u32;
type Terminal = char;
type Nonterminal = char;
type Word = Vec<Symbol>;

#[derive(Eq, PartialEq, Hash, Debug, Clone, Copy)]
enum Symbol {
    Terminal(Terminal),
    Nonterminal(Nonterminal),
    Epsilon,
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Symbol::Terminal(t) => write!(f, "{}", t),
            Symbol::Nonterminal(nt) => write!(f, "{}", nt),
            Symbol::Epsilon => write!(f, "e"),
        }
    }
}

#[derive(Eq, PartialEq, Hash, Debug, Clone, Copy)]
enum RegexSymbol {
    Terminal(Terminal),
    Nonterminal(Nonterminal),
    AtomicLanguage(Nonterminal, Terminal),
    Epsilon,
}

#[derive(Debug)]
struct Regex {
    regex: HashMap<(Nonterminal, Terminal), RegexNode>,
}

impl fmt::Display for Regex {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for ((nonterminal, terminal), node) in &self.regex {
            write!(f, "[{}]({}): {}\n", nonterminal, terminal, node)?;
        }
        Ok(())
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
enum RegexNode {
    Word(WordNode),
    Node(NodeNode),
}

impl fmt::Display for RegexNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            RegexNode::Word(node) => write!(f, "{}", node),
            RegexNode::Node(node) => write!(f, "{}", node),
        }
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
struct NodeNode {
    nodes: Vec<RegexNode>,
}

impl fmt::Display for NodeNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(")?;
        for node in &self.nodes {
            write!(f, "{}", node)?;
        }
        write!(f, ")")
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
struct WordNode {
    words: HashSet<Word>,
    kleene_star: bool,
}

impl fmt::Display for WordNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "(")?;
        let mut words_iter = self.words.iter().peekable();
        while words_iter.peek().is_some() {
            for symbol in words_iter.next().unwrap() {
                write!(f, "{}", symbol)?;
            }
            if words_iter.peek().is_some() {
                write!(f, " + ")?;
            }
        }
        if self.kleene_star {
            write!(f, ")*")
        } else {
            write!(f, ")")
        }
    }
}

struct FiniteStateAutomaton {
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

struct Grammar {
    terminals: HashSet<Terminal>,
    nonterminals: HashSet<Nonterminal>,
    symbols: HashSet<Symbol>,
    start: Nonterminal,
    rules: HashMap<Nonterminal, HashSet<Word>>,
    finite_state_automaton: FiniteStateAutomaton,
}

enum StateErrors {
    StartNotInStates(State),
    AccNotInStates(State),
    SrcNotInStates(State),
    DestNotInStates(State),
    AtomicStateNotInStates(State),
}

impl FiniteStateAutomaton {
    pub fn new(states: HashSet<State>, accepting_states: HashSet<State>, start: State, transitions: HashMap<State, HashSet<(Symbol, State)>>, atomic_to_state: HashMap<(Symbol, Terminal), State>) -> Result<FiniteStateAutomaton, StateErrors> {
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

impl Grammar {
    pub fn new(terminals: HashSet<Terminal>, nonterminals: HashSet<Nonterminal>, start: Nonterminal, rules: HashMap<Nonterminal, HashSet<Word>>) -> Grammar {
        let mut symbols: HashSet<Symbol> = HashSet::new();
        for s in &nonterminals {
            symbols.insert(Symbol::Nonterminal(*s));
        }
        for t in &terminals {
            symbols.insert(Symbol::Terminal(*t));
        }
        let finite_state_automaton = Grammar::build_fsa(&terminals, &nonterminals, &symbols, Symbol::Nonterminal(start), &rules);
        Grammar{terminals, nonterminals, symbols, start, rules, finite_state_automaton}
    }

    fn build_fsa(terminals: &HashSet<Terminal>, nonterminals: &HashSet<Nonterminal>, symbols: &HashSet<Symbol>, start_nt: Symbol, rules: &HashMap<Nonterminal, HashSet<Word>>) -> FiniteStateAutomaton {
        let start: State = 0;
        let epsilon: State = 1;
        let mut states: HashSet<State> = HashSet::from([start, epsilon]); // All states in the atomic language, contains at least start state Sigma_epsilon (state 0) and state epsilon (state 1)
        let mut accepting_states: HashSet<State> = HashSet::from([epsilon]); // All states containing symbol epsilon, state 1 is epsilon itself
        let mut transitions: HashMap<State, HashSet<(Symbol, State)>> = HashMap::new();

        // Add transition from Sigma_epsilon to epsilon by start symbol
        transitions.insert(start, HashSet::from([(start_nt, epsilon)]));

        let mut curr_state: State = 2;
        let mut atomic_to_state: HashMap<(Symbol, Terminal), State> = HashMap::new();

        // add terminal derivations to atomic_to_state
        for terminal in terminals {
            atomic_to_state.insert((Symbol::Terminal(*terminal), *terminal), epsilon);
        }

        let atomic_regex: Regex = Regex::new(terminals, rules);

        for ((nonterminal, terminal), node) in atomic_regex.regex {
            if node == RegexNode::Word(WordNode{words: HashSet::from([vec![Symbol::Epsilon]]), kleene_star: false}) {
                atomic_to_state.insert((Symbol::Nonterminal(nonterminal), terminal), 1);
                continue;
            }
            let (new_states, start_state, end_state, highest_state, new_transitions) = Grammar::regex_node_to_states(&node, curr_state, curr_state);
            states.extend(new_states);
            atomic_to_state.insert((Symbol::Nonterminal(nonterminal), terminal), start_state);
            accepting_states.insert(end_state);
            curr_state = highest_state + 1;
            transitions.extend(new_transitions);
        }

        FiniteStateAutomaton{states, accepting_states, start, transitions, atomic_to_state}
    }

    fn regex_node_to_states(node: &RegexNode, curr_state: State, highest_state: State) -> (HashSet<State>, State, State, State, HashMap<State, HashSet<(Symbol, State)>>) {
        let start_state: State = curr_state;
        let mut end_state: State = 0;
        let mut states: HashSet<State> = HashSet::from([start_state]);
        let mut transitions: HashMap<State, HashSet<(Symbol, State)>> = HashMap::new();

        let mut highest_state: State = highest_state;
        match node {
            RegexNode::Node(node) => {
                let mut curr_state: State = curr_state;

                for n in node.nodes.iter() {
                    let (new_states, _, new_curr_state, new_highest_state, new_transitions) = Grammar::regex_node_to_states(n, curr_state, highest_state);
                    curr_state = new_curr_state;
                    states.extend(new_states);
                    transitions.extend(new_transitions);
                    highest_state = new_highest_state;
                    end_state = new_curr_state;
                }
            },
            RegexNode::Word(node) => {
                //node.words: HashSet<Word>
                let mut curr_state: State;
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
            },
        }

        (states, start_state, end_state, highest_state, transitions)
    }

}

impl Regex {
    fn new(terminals: &HashSet<Terminal>, rules: &HashMap<Nonterminal, HashSet<Word>>) -> Regex {
        let mut atomic_regex_rules: HashMap<(Nonterminal, Terminal), (HashSet<Vec<RegexSymbol>>, HashSet<Vec<RegexSymbol>>, HashSet<Vec<RegexSymbol>>)> = HashMap::new();
        let mut queue: VecDeque<(Nonterminal, Terminal)> = VecDeque::new();

        for (nonterminal, rule_list) in rules {
            for terminal in terminals {
                //println!("Nonterminal: {}, terminal: {}", nonterminal, terminal);
                let regex_rules = Regex::calculate_regex_rules(nonterminal, terminal, rules, rule_list);
                atomic_regex_rules.insert((*nonterminal, *terminal), regex_rules);
                queue.push_back((*nonterminal, *terminal));
            }
        }

        Regex {regex: Regex::build_regex_map(queue, atomic_regex_rules)}
    }

    fn calculate_regex_rules(nonterminal: &Nonterminal, terminal: &Terminal, rules: &HashMap<Nonterminal, HashSet<Word>>, rule_list: &HashSet<Vec<Symbol>>) -> (HashSet<Vec<RegexSymbol>>, HashSet<Vec<RegexSymbol>>, HashSet<Vec<RegexSymbol>>) {
        let mut direct: HashSet<Vec<RegexSymbol>> = HashSet::new();
        let mut recursive: HashSet<Vec<RegexSymbol>> = HashSet::new();
        let mut different_atomic: HashSet<Vec<RegexSymbol>> = HashSet::new();
        let mut nullable_atomic: bool = false;

        for rule in rule_list {
            //println!("Rule: {:?}", rule);
            if rule[0] == Symbol::Epsilon {
                nullable_atomic = true;
            }
            if let Some(regex_rule) = Regex::rule_to_regex_rule(rule, *terminal) {
                //println!("Regex rule: {:?}", regex_rule);
                match regex_rule[0] {
                    RegexSymbol::AtomicLanguage(nt, _) => {
                        if *nonterminal == nt {
                            recursive.insert(regex_rule);
                        } else {
                            different_atomic.insert(regex_rule);
                        }
                    },
                    _ => {
                        direct.insert(regex_rule);
                    },
                }
            }
        }
        let mut new_rules = Regex::null_rules(rules, &mut direct, nullable_atomic);
        new_rules.extend(Regex::null_rules(rules, &mut recursive, nullable_atomic));
        new_rules.extend(Regex::null_rules(rules, &mut different_atomic, nullable_atomic));

        for rule in new_rules {
            match rule[0] {
                RegexSymbol::AtomicLanguage(nt, _) => {
                    if *nonterminal == nt {
                        recursive.insert(rule);
                    } else {
                        different_atomic.insert(rule);
                    }
                },
                RegexSymbol::Epsilon => {direct.insert(rule);},
                RegexSymbol::Terminal(t) => {
                    if t == *terminal && nullable_atomic {
                        if rule.len() > 1 {
                            direct.insert(rule[1..].to_vec());
                        } else {
                            direct.insert(vec![RegexSymbol::Epsilon]);
                        }
                    }
                },
                _ => {},
            }
        }
        (direct, recursive, different_atomic)
    }

    fn build_regex_map(mut queue: VecDeque<(Nonterminal, Terminal)>, atomic_regex_rules: HashMap<(Nonterminal, Terminal), (HashSet<Vec<RegexSymbol>>, HashSet<Vec<RegexSymbol>>, HashSet<Vec<RegexSymbol>>)>) -> HashMap<(Nonterminal, Terminal), RegexNode> {
        let mut atomic_regex_rules_working_copy: HashMap<(Nonterminal, Terminal), (HashSet<Vec<RegexSymbol>>, HashSet<Vec<RegexSymbol>>, HashSet<Vec<RegexSymbol>>)> = atomic_regex_rules.clone();
        let mut res: HashMap<(Nonterminal, Terminal), RegexNode> = HashMap::new();

        while queue.len() > 0 {
            if let Some((nonterminal, terminal)) = queue.pop_front() {
                //println!("Current (nonterminal, terminal): {:?}", (nonterminal, terminal));
                if let Some((direct, recursive, different_atomic)) = atomic_regex_rules_working_copy.get_mut(&(nonterminal, terminal)) {
                    //println!("Current different: {:?}", different_atomic);
                    //let mut different_recursives: HashSet<Vec<RegexSymbol>> = HashSet::new();
                    for different_rule in different_atomic.clone() {
                        if let RegexSymbol::AtomicLanguage(nt, t) = different_rule[0] {
                            if let Some((ntdirect, ntrecursive, ntdifferent)) = atomic_regex_rules.get(&(nt, t)) {
                                for ntdirect_rule in ntdirect.clone() {
                                    direct.insert([&ntdirect_rule[..], &different_rule[1..]].concat());
                                }
                                for ntrecursive_rule in ntrecursive.clone() {
                                    recursive.insert([&[RegexSymbol::AtomicLanguage(nonterminal, t)], &ntrecursive_rule[1..], &different_rule[1..]].concat());
                                }
                                for ntdifferent_rule in ntdifferent.clone() {
                                    if let RegexSymbol::AtomicLanguage(nt2, _) = ntdifferent_rule[0] {
                                        if nt2 == nonterminal {
                                            recursive.insert([&ntdifferent_rule[..], &different_rule[1..]].concat());
                                        } else {
                                            different_atomic.insert([&ntdifferent_rule[..], &different_rule[1..]].concat());
                                        }
                                    }
                                }
                                different_atomic.remove(&different_rule);
                            }
                        }
                    }
                    
                    //println!("Current direct: {:?}", direct);
                    //println!("Current recursive: {:?}", recursive);
                    //println!("Current different: {:?}", different_atomic);
                    if different_atomic.len() == 0 && direct.len() > 0 {
                        res.insert((nonterminal, terminal), Regex::build_regex_node(&direct, &Vec::new(), &recursive));
                    } else {
                        if different_atomic.len() > 0 { //&& different_atomic.clone() == different_recursives {
                            let mut all_in_res: bool = true;
                            let mut regex_nodes: Vec<RegexNode> = Vec::new();
                            for different_rule in different_atomic.iter() {
                                if let RegexSymbol::AtomicLanguage(nt, t) = different_rule[0] {
                                    if let Some(regex) = res.get(&(nt, t)) {
                                        regex_nodes.push(regex.clone());
                                    } else if queue.contains(&(nt, t)) {
                                        all_in_res = false;
                                        queue.push_back((nonterminal, terminal));
                                    } else {
                                        all_in_res = false;
                                    }
                                }
                                //else {
                                //    all_in_res = false;
                                //}
                            }
                            if all_in_res {
                                res.insert((nonterminal, terminal), Regex::build_regex_node(&direct, &regex_nodes, &recursive));
                                continue;
                            }
                        }
                        //second_pass_queue.push_back((nonterminal, terminal));
                    }
                    //println!("End different: {:?}", different);
                }
            }
        }
        res
    }

    fn null_rules(rules: &HashMap<Nonterminal, HashSet<Word>>, rule_list: &HashSet<Vec<RegexSymbol>>, nullable_atomic: bool) -> HashSet<Vec<RegexSymbol>> {
        //println!("Rule_list: {:?}", rule_list);
        let mut new_rules: HashSet<Vec<RegexSymbol>> = HashSet::new();
        for rule in rule_list {
            //println!("Rule: {:?}", rule);
            let mut nullable_positions: Vec<usize> = Vec::new();
            for (pos, symbol) in rule.iter().enumerate() {
                //println!("Symbol: {:?}", symbol);
                match symbol {
                    RegexSymbol::Nonterminal(nt) => {
                        //println!("Nullable_nonterminal: {}", Regex::nullable_nonterminal(rules, &nt));
                        if Regex::nullable_nonterminal(rules, &nt) {
                            nullable_positions.push(pos);
                        }
                    },
                    RegexSymbol::AtomicLanguage(nt, _) => {
                        if nullable_atomic && Regex::nullable_nonterminal(rules, &nt) {
                            nullable_positions.push(pos);
                        }
                    }
                    _ => {},
                }
            }
            //println!("Nullable_positions: {:?}", nullable_positions);

            let nullable_combinations: Vec<Vec<usize>> = Regex::powerset(&nullable_positions);
            for comb in nullable_combinations {
                let mut rev = comb.clone();
                rev.sort();
                rev.reverse();
                let mut new_rule = rule.clone();
                for index in rev {
                    new_rule.remove(index);
                }
                //println!("New rule: {:?}", new_rule);
                if new_rule.len() > 0 {
                    new_rules.insert(new_rule);
                }
            }
        }
        new_rules
    }

    fn powerset<T>(s: &[T]) -> Vec<Vec<T>> where T: Clone {
        (0..2usize.pow(s.len() as u32)).map(|i| {
            s.iter().enumerate().filter(|&(t, _)| (i >> t) % 2 == 1)
                .map(|(_, element)| element.clone()).collect()
        }).collect()
    }

    fn nullable_nonterminal(rules: &HashMap<Nonterminal, HashSet<Word>>, nonterminal: &Nonterminal) -> bool {
        if let Some(rule_list) = rules.get(nonterminal) {
            for rule in rule_list {
                if *rule == vec![Symbol::Epsilon] {
                    return true;
                }
            }
        }
        false
    }

    fn build_regex_node(direct: &HashSet<Vec<RegexSymbol>>, different_recursive: &Vec<RegexNode>, recursive: &HashSet<Vec<RegexSymbol>>) -> RegexNode {
        let mut res_nodes: Vec<RegexNode> = Vec::new();
        if direct.len() > 0 {
            let mut direct_word_set: HashSet<Word> = HashSet::new();
            for direct_rule in direct {
                if let Some(word) = Regex::regex_word_to_word(&direct_rule) {
                    direct_word_set.insert(word);
                }
            }
            res_nodes.push(RegexNode::Word(WordNode{words: direct_word_set, kleene_star: false}));
        }
        if different_recursive.len() > 0 {
            if different_recursive.len() == 1 {
                res_nodes.push(different_recursive.clone().pop().unwrap());
            } else {
                let mut different_recursive_node_vec: Vec<RegexNode> = Vec::new();
                for different_recursive_node in different_recursive {
                    different_recursive_node_vec.push(different_recursive_node.clone());
                }
                res_nodes.push(RegexNode::Node(NodeNode{nodes: different_recursive_node_vec}));
            }
        }
        if recursive.len() > 0 {
            let mut recursive_word_set: HashSet<Word> = HashSet::new();
            for recursive_rule in recursive {
                if let Some(word) = Regex::regex_word_to_word(&recursive_rule[1..].to_vec()) {
                    if word.len() > 0 {
                        recursive_word_set.insert(word);
                    }
                }
            }
            res_nodes.push(RegexNode::Word(WordNode{words: recursive_word_set, kleene_star: true}));
        }
        if res_nodes.len() == 1 {
            res_nodes.pop().unwrap()
        } else {
            RegexNode::Node(NodeNode{nodes: res_nodes})
        }
    }

    fn rule_to_regex_rule(rule: &Word, terminal: Terminal) -> Option<Vec<RegexSymbol>> {
        match rule[0] {
            Symbol::Nonterminal(nt) => {
                let mut res: Vec<RegexSymbol> = Vec::new();
                res.push(RegexSymbol::AtomicLanguage(nt, terminal));
                for symbol in &rule[1..] {
                    res.push(Regex::symbol_to_regex_symbol(symbol));
                }
                Some(res)
            },
            Symbol::Terminal(t) => {
                if t == terminal {
                    if rule.len() > 1 {
                        let mut res: Vec<RegexSymbol> = Vec::new();
                        for symbol in &rule[1..] {
                            res.push(Regex::symbol_to_regex_symbol(symbol));
                        }
                        Some(res)
                    } else {
                        Some(vec![RegexSymbol::Epsilon])
                    }
                } else {
                    None
                }
            },
            _ => {None},
        }
    }

    fn symbol_to_regex_symbol(symbol: &Symbol) -> RegexSymbol {
        match symbol {
            Symbol::Nonterminal(nt) => RegexSymbol::Nonterminal(*nt),
            Symbol::Terminal(t) => RegexSymbol::Terminal(*t),
            Symbol::Epsilon => RegexSymbol::Epsilon
        }
    }

    fn regex_word_to_word(regex_word: &Vec<RegexSymbol>) -> Option<Word> {
        let mut res: Word = Vec::new();
        for regex_symbol in regex_word {
            if let Some(symbol) = Regex::regex_symbol_to_symbol(regex_symbol) {
                res.push(symbol);
            } else {
                return None
            }
        }
        Some(res)
    }

    fn regex_symbol_to_symbol(regex_symbol: &RegexSymbol) -> Option<Symbol> {
        match regex_symbol {
            RegexSymbol::Nonterminal(nt) => Some(Symbol::Nonterminal(*nt)),
            RegexSymbol::Terminal(t) => Some(Symbol::Terminal(*t)),
            RegexSymbol::Epsilon => Some(Symbol::Epsilon),
            RegexSymbol::AtomicLanguage(_, _) => None
        }
    }

}

#[cfg(test)]
mod tests{
    use super::*;

    fn basic_relational_parsing_example_grammar() -> Grammar {
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

    fn e_rule_relational_parsing_example_grammar() -> Grammar {
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

    fn three_rule_grammar() -> Grammar {
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

    fn difficult_bottom_up_grammar() -> Grammar {
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

    fn odd_number_of_a_grammar() -> Grammar {
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

    fn direct_left_recursive_grammar() -> Grammar {
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
    
    fn indirect_left_recursive_grammar() -> Grammar {
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

    fn even_more_indirect_left_recursive_grammar() -> Grammar {
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

    fn direct_right_recursive_grammar() -> Grammar {
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

    fn indirect_right_recursive_grammar() -> Grammar {
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

    #[test]
    fn rule_to_regex_rule_test() {
        assert_eq!(Regex::rule_to_regex_rule(&vec![Symbol::Terminal('a'), Symbol::Nonterminal('S'), Symbol::Terminal('b')], 'a'), Some(vec![RegexSymbol::Nonterminal('S'), RegexSymbol::Terminal('b')]));
        assert_eq!(Regex::rule_to_regex_rule(&vec![Symbol::Nonterminal('S'), Symbol::Terminal('a'), Symbol::Terminal('b')], 'a'), Some(vec![RegexSymbol::AtomicLanguage('S', 'a'), RegexSymbol::Terminal('a'), RegexSymbol::Terminal('b')]));
        assert_eq!(Regex::rule_to_regex_rule(&vec![Symbol::Terminal('a'), Symbol::Terminal('a'), Symbol::Terminal('a')], 'a'), Some(vec![RegexSymbol::Terminal('a'), RegexSymbol::Terminal('a')]));
        assert_eq!(Regex::rule_to_regex_rule(&vec![Symbol::Terminal('a')], 'a'), Some(vec![RegexSymbol::Epsilon]));
        assert_eq!(Regex::rule_to_regex_rule(&vec![Symbol::Terminal('b'), Symbol::Nonterminal('S'), Symbol::Terminal('b')], 'a'), None);
    }

    #[test]
    fn regex_word_to_word_test() {
        assert_eq!(Regex::regex_word_to_word(&vec![RegexSymbol::Terminal('a'), RegexSymbol::Nonterminal('b')]), Some(vec![Symbol::Terminal('a'), Symbol::Nonterminal('b')]));
        assert_eq!(Regex::regex_word_to_word(&vec![RegexSymbol::Terminal('a'), RegexSymbol::Nonterminal('b'), RegexSymbol::Epsilon]), Some(vec![Symbol::Terminal('a'), Symbol::Nonterminal('b'), Symbol::Epsilon]));
        assert_eq!(Regex::regex_word_to_word(&vec![RegexSymbol::AtomicLanguage('a', 'b')]), None);
        assert_eq!(Regex::regex_word_to_word(&vec![RegexSymbol::Terminal('a'), RegexSymbol::AtomicLanguage('a', 'b')]), None);
        assert_eq!(Regex::regex_word_to_word(&vec![RegexSymbol::Nonterminal('a'), RegexSymbol::AtomicLanguage('a', 'b')]), None);
    }

    #[test]
    fn rules_to_regex_rules_test() {
        let grammar = basic_relational_parsing_example_grammar();
        println!("{}", Regex::new(&grammar.terminals, &grammar.rules));

        let grammar = e_rule_relational_parsing_example_grammar();
        println!("{}", Regex::new(&grammar.terminals, &grammar.rules));

        let grammar = three_rule_grammar();
        println!("{}", Regex::new(&grammar.terminals, &grammar.rules));

        let grammar = difficult_bottom_up_grammar();
        println!("{}", Regex::new(&grammar.terminals, &grammar.rules));

        let grammar = odd_number_of_a_grammar();
        println!("{}", Regex::new(&grammar.terminals, &grammar.rules));

        let grammar = direct_left_recursive_grammar();
        println!("{}", Regex::new(&grammar.terminals, &grammar.rules));

        let grammar = indirect_left_recursive_grammar();
        println!("{}", Regex::new(&grammar.terminals, &grammar.rules));

        let grammar = even_more_indirect_left_recursive_grammar();
        println!("{}", Regex::new(&grammar.terminals, &grammar.rules));

        let grammar = direct_right_recursive_grammar();
        println!("{}", Regex::new(&grammar.terminals, &grammar.rules));

        let grammar = indirect_right_recursive_grammar();
        println!("{}", Regex::new(&grammar.terminals, &grammar.rules));
    }

    #[test]
    fn regex_node_to_states_test() {
        let grammar = e_rule_relational_parsing_example_grammar();
        let regex = Regex::new(&grammar.terminals, &grammar.rules);
        println!("{}", regex);
        println!("{:?}", Grammar::regex_node_to_states(regex.regex.get(&('S', 'b')).unwrap(), 1, 1));

        //let grammar = odd_number_of_a_grammar();
        //let regex = Regex::new(&grammar.terminals, &grammar.rules);
        //println!("{}", regex);
        //println!("{:?}", Grammar::regex_node_to_states(regex.regex.get(&('S', 'a')).unwrap(), 1));

    }

    #[test]
    fn to_dot() {
        let grammar = difficult_bottom_up_grammar();
        grammar.finite_state_automaton.to_dot("difficult_bottom_up").expect("error");

        let grammar = basic_relational_parsing_example_grammar();
        grammar.finite_state_automaton.to_dot("basic").expect("error");

        let grammar = even_more_indirect_left_recursive_grammar();
        grammar.finite_state_automaton.to_dot("very_indirect").expect("error");
    }

}