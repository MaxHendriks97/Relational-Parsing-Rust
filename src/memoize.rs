use std::collections::{HashSet, HashMap, BTreeSet};
use std::fmt;

use crate::language_list::CompletedParses;
use crate::word::*;
use crate::*;

pub type MemEdges = HashMap<(Edge, bool), RulesSet>;
pub type MemPart = HashMap<Edge, MemEdges>;
pub type MemParses = CompletedParses;

pub struct MemoBuilder {
    prep_deriv_memo: MemPart,
    deriv_memo: MemPart,
    deriv_accepting: HashMap<Edge, RulesSet>,
    prep_memo: MemPart,
    prep_edges: MemEdges,
    no_pops: usize,
}

impl MemoBuilder {
    pub fn new() -> MemoBuilder {
        MemoBuilder{prep_deriv_memo: HashMap::new(), deriv_memo: HashMap::new(), deriv_accepting: HashMap::new(), prep_memo: HashMap::new(), prep_edges: HashMap::new(), no_pops: 0}
    }

    pub fn extend_prepend_memo(&mut self, memedge: Edge, edge: Edge, accepting: bool, rules_set: RulesSet) {
        self.prep_memo.entry(memedge).or_default().entry((edge, accepting)).or_default().extend(rules_set);
    }

    pub fn insert_prepend_edges(&mut self, edge: Edge, accepting: bool, opt_rules: Option<Rules>) {
        if let Some(rules) = opt_rules {
            self.prep_edges.entry((edge, accepting)).or_default().insert(rules);
        } else {
            self.prep_edges.entry((edge, accepting)).or_default();
        }
    }

    pub fn add_deriv_memo(&mut self, deriv_memo: MemPart) {
        self.deriv_memo = deriv_memo;
    }

    pub fn get_prep_deriv_memo(&self) -> &MemPart {
        &self.prep_deriv_memo
    }

    pub fn insert_prep_deriv_memo(&mut self, memedge: Edge, edge: Edge, accepting: bool, rules: Rules) {
        self.prep_deriv_memo.entry(memedge).or_default().entry((edge, accepting)).or_default().insert(rules);
    }

    pub fn extend_prep_deriv_memo(&mut self, memedge: Edge, edge: Edge, accepting: bool, rules_set: HashSet<Rules>) {
        self.prep_deriv_memo.entry(memedge).or_default().entry((edge, accepting)).or_default().extend(rules_set);
    }

    pub fn concat_prep_deriv_memo(&mut self, mempart: MemPart) {
        for (memedge, edges) in mempart {
            for (edge, rules) in edges {
                self.prep_deriv_memo.entry(memedge).or_default().entry(edge).or_default().extend(rules);
            }
        }
    }

    pub fn insert_deriv_memo(&mut self, memedge: Edge, edge: Edge, accepting: bool, rules: Rules) {
        self.deriv_memo.entry(memedge).or_default().entry((edge, accepting)).or_default().insert(rules);
    }

    pub fn extend_deriv_memo(&mut self, memedge: Edge, edge: Edge, accepting: bool, rules_set: HashSet<Rules>) {
        self.deriv_memo.entry(memedge).or_default().entry((edge, accepting)).or_default().extend(rules_set);
    }

    pub fn insert_deriv_accepting(&mut self, memedge: Edge, rules: Rules) {
        self.deriv_accepting.entry(memedge).or_default().insert(rules);
    }

    pub fn extend_deriv_accepting(&mut self, memedge: Edge, rules_set: RulesSet) {
        self.deriv_accepting.entry(memedge).or_default().extend(rules_set);
    }

    pub fn increase_pop(&mut self) {
        self.no_pops += 1;
    }

    pub fn build_memo(mut self) -> Memo {
        let opt_memo: Option<MemPart>;
        let mut memo: MemPart = HashMap::new();
        let mut memo_accepting: HashMap<Edge, (Depth, RulesSet)> = HashMap::new();
        if !self.prep_deriv_memo.is_empty() {
            for (memedge, edges) in self.prep_memo {
                for (((source_state, dest_depth), accepting), rules) in edges {
                    memo.entry(memedge).or_default().entry(((source_state, dest_depth - self.no_pops), accepting)).or_default().extend(rules)
                }
            }
            if self.no_pops > 0 {
                self.no_pops -= 1;
                opt_memo = None;
            } else {
                opt_memo = Some(self.prep_deriv_memo);
            }

            for (memedge, edges) in self.deriv_memo {
                for (((source_state, dest_depth), accepting), rules) in edges {
                    memo.entry(memedge).or_default().entry(((source_state, dest_depth + 1 - self.no_pops), accepting)).or_default().extend(rules)
                }
            }
            for ((state, depth), rules_set) in self.deriv_accepting {
                memo_accepting.insert((state, depth), (depth + 1 - self.no_pops, rules_set));
            }
        } else {
            for (memedge, edges) in self.prep_memo {
                for (((source_state, dest_depth), accepting), rules) in edges {
                    memo.entry(memedge).or_default().entry(((source_state, dest_depth - self.no_pops), accepting)).or_default().extend(rules)
                }
            }
            opt_memo = None;

            for (memedge, edges) in self.deriv_memo {
                for (((source_state, dest_depth), accepting), rules) in edges {
                    memo.entry(memedge).or_default().entry(((source_state, dest_depth - self.no_pops), accepting)).or_default().extend(rules)
                }
            }
            for ((state, depth), rules_set) in self.deriv_accepting {
                memo_accepting.insert((state, depth), (depth - self.no_pops, rules_set));
            }
        }

        let mut extra_edges: MemEdges = HashMap::new();
        for (((source_state, dest_depth), accepting), rules_set) in self.prep_edges {
            extra_edges.insert(((source_state, dest_depth - self.no_pops), accepting), rules_set);
        }

        Memo {opt_memo, memo, memo_accepting, extra_edges, no_pops: self.no_pops}
    }
}

#[derive(Debug)]
pub struct Memo {
    opt_memo: Option<MemPart>,
    memo: MemPart,
    memo_accepting: HashMap<Edge, (Depth, RulesSet)>,
    extra_edges: MemEdges,
    no_pops: usize,
}

impl fmt::Display for Memo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "    opt_memo: {:?}\n", self.opt_memo)?;
        write!(f, "    memo: {:?}\n", self.memo)?;
        write!(f, "    memo_accepting: {:?}\n", self.memo_accepting)?;
        write!(f, "    extra_edges: {:?}\n", self.extra_edges)?;
        write!(f, "    no_pops: {:?}\n", self.no_pops)
    }
}

impl Memo {
    pub fn get_opt_memo(&self) -> &Option<MemPart> {
        &self.opt_memo
    }

    pub fn get_memo_edge(&self, edge: &Edge) -> Option<&MemEdges> {
        self.memo.get(edge)
    }

    pub fn get_accepting(&self, edge: &Edge) -> Option<&(Depth, RulesSet)> {
        self.memo_accepting.get(edge)
    }

    pub fn get_extra_edges(&self) -> &MemEdges {
        &self.extra_edges
    }

    pub fn get_pops(&self) -> usize {
        self.no_pops
    }
}

#[derive(Debug)]
pub struct Memoize {
    mem: HashMap<(BTreeSet<Edge>, Terminal), Memo>,
}

impl fmt::Display for Memoize {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for ((edge_set, terminal), memo) in &self.mem {
            for edge in edge_set {
                write!(f, "({}, {}), ", edge.0, edge.1)?;
            }
            write!(f, "{}:\n", terminal)?;
            println!("{}", memo);
        }
        Ok(())
    }
}

impl Memoize {
    pub fn new() -> Memoize {
        Memoize{mem: HashMap::new()}
    }

    pub fn get_memo(&self, edges: BTreeSet<Edge>, terminal: Terminal) -> Option<&Memo> {
        self.mem.get(&(edges, terminal))
    }

    pub fn memoize(&mut self, edges: BTreeSet<Edge>, terminal: Terminal, memo: Memo) {
        self.mem.insert((edges, terminal), memo);
    }


}