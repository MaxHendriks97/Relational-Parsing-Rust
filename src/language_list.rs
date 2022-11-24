use std::collections::{hash_set, hash_map};
use std::collections::{HashMap, HashSet, BTreeSet};
use std::fmt;

use crate::*;

pub type Depth = usize;
pub type Edge = (State, Depth);
pub type Edges = HashMap<Edge, RulesSet>;
pub type CompletedParses = RulesSet;

pub fn print_edge(edge: &Edge, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "({}, {})", edge.0, edge.1)
}

#[derive(Debug)]
pub struct Language {
    edges: Edges,
    completed_parses: CompletedParses,
    fin: bool,
}

impl fmt::Display for Language {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "final: {}\n", self.fin)?;
        if self.fin && !self.completed_parses.is_empty() {
            write!(f, "completed parses:\n")?;
            for rules in &self.completed_parses {
                write!(f, "        ")?;
                print_rules(&rules, f)?;
                write!(f, "\n")?;
            }
        }
        for ((state, depth), rules_set) in &self.edges {
            write!(f, "  ({}, {})", state, depth)?;
            if !rules_set.is_empty() {
                write!(f, " rules:\n")?;
                for rules in rules_set {
                    write!(f, "        ")?;
                    print_rules(rules, f)?;
                    write!(f, "\n")?;
                }
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

impl Language {
    pub fn new() -> Language {
        Language {edges: HashMap::new(), completed_parses: HashSet::new(), fin: false}
    }

    pub fn new_from(edges: Edges, completed_parses: CompletedParses, fin: bool) -> Language {
        Language {edges, completed_parses, fin}
    }

    pub fn start_lang() -> Language {
        Language {edges: HashMap::new(), completed_parses: HashSet::new(), fin: true}
    }

    pub fn get_edge_rules(&self, edge: &Edge) -> Option<&RulesSet> {
        self.edges.get(edge)
    }

    pub fn has_edges(&self) -> bool {
        !self.edges.is_empty()
    }

    pub fn edges_empty(&self) -> bool {
        self.edges.is_empty()
    }

    pub fn is_final(&self) -> bool {
        self.fin
    }

    pub fn not_final(&self) -> bool {
        !self.fin
    }

    pub fn set_final(&mut self) {
        self.fin = true;
    }

    pub fn take_edges(&mut self) -> hash_map::Drain<Edge, RulesSet> {
        self.edges.drain()
    }

    pub fn edges_ref(&self) -> &Edges {
        &self.edges
    }

    pub fn has_completed_parses(&self) -> bool {
        !self.completed_parses.is_empty()
    }

    pub fn take_completed_parses(&mut self) -> hash_set::Drain<Rules> {
        self.completed_parses.drain()
    }

    pub fn insert_edge(&mut self, edge: Edge, opt_rules: Option<Rules>) {
        if let Some(rules) = opt_rules {
            self.edges.entry(edge).or_default().insert(rules);
        } else {
            self.edges.entry(edge).or_default();
        }
    }

    pub fn extend_edge(&mut self, edge: Edge, rules_set: RulesSet) {
        self.edges.entry(edge).or_default().extend(rules_set);
    }

    pub fn insert_completed_parse(&mut self, completed_parse: Rules) {
        self.completed_parses.insert(completed_parse);
    }

    pub fn extend_completed_parses(&mut self, completed_parses: impl Iterator<Item = Rules>) {
        self.completed_parses.extend(completed_parses);
    }

    pub fn find_lowest_depth(&self) -> Depth {
        let mut res: Depth = usize::max_value();
        for ((_, depth), _) in &self.edges {
            if *depth < res {
                res = *depth;
            }
        }
        res
    }

    pub fn adjust_lowest_depth(&mut self, lowest_depth: Depth) {
        let old_edges: Edges = std::mem::take(&mut self.edges);
        for ((source_state, dest_depth), rules_set) in old_edges {
            self.edges.insert((source_state, dest_depth - lowest_depth + 1), rules_set);
        }
    }

    pub fn make_mem_edges(&self) -> BTreeSet<Edge> {
        let mut res: BTreeSet<Edge> = BTreeSet::new();

        for (state, depth) in self.edges.keys() {
            res.insert((*state, *depth));
        }

        res
    }
}

#[derive(Debug)]
pub struct LanguageList {
    languages: Vec<Language>,
}

impl fmt::Display for LanguageList {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for (index, language) in self.languages.iter().enumerate() {
            write!(f, "id: {} {}\n", index, language)?;
        }
        Ok(())
    }
}

impl LanguageList {
    pub fn new() -> LanguageList {
        let mut languages: Vec<Language> = Vec::new();
        languages.push(Language::start_lang());
        LanguageList {languages}
    }

    pub fn get(&self, depth: Depth) -> Option<&Language> {
        self.languages.get(self.languages.len().saturating_sub(depth))
    }

    pub fn insert_new_language(&mut self, lang: Language) {
        self.languages.push(lang);
    }

    pub fn pop_lang(&mut self) -> Option<Language> {
        self.languages.pop()
    }

}