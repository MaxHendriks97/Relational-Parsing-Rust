//! # Language list
//! 
//! The `Language_List` module defines the Language and LanguageList data structures necessary to keep track of all information relevant during parsing.
//! Additionally, it supplies many methods that aid in the parsing effort.
use std::collections::btree_map;
use std::collections::{BTreeSet, BTreeMap};
use std::fmt;
use std::mem;
use std::ops::{Add, Sub};

use crate::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Depth(pub usize);

impl fmt::Display for Depth {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Add<usize> for Depth {
    type Output = Depth;

    fn add(self, rhs: usize) -> Self::Output {
        Depth(self.0 + rhs)
    }
}

impl Add<Depth> for Depth {
    type Output = Depth;

    fn add(self, rhs: Depth) -> Self::Output {
        Depth(self.0 + rhs.0)
    }
}

impl Sub<usize> for Depth {
    type Output = Depth;

    fn sub(self, rhs: usize) -> Self::Output {
        Depth(self.0 - rhs)
    }
}

impl Sub<Depth> for Depth {
    type Output = Depth;

    fn sub(self, rhs: Depth) -> Self::Output {
        Depth(self.0 - rhs.0)
    }
}

impl Depth {
    pub fn new(depth: usize) -> Depth {
        Depth(depth)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Edge(State, Depth);

impl Edge {
    pub fn new(state: State, depth: Depth) -> Edge {
        Edge(state, depth)
    }

    pub fn state(&self) -> State {
        self.0
    }

    pub fn depth(&self) -> Depth {
        self.1
    }

    pub fn add_to_depth(&mut self, num: usize) {
        self.1 = self.1 + num;
    }

    pub fn sub_from_depth(&mut self, num: usize) {
        self.1 = self.1 - num;
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub struct Edges(BTreeMap<Edge, RulesSet>);

impl Edges {
    pub fn new() -> Edges {
        Edges(BTreeMap::new())
    }

    pub fn from(edges: BTreeMap<Edge, RulesSet>) -> Edges {
        Edges(edges)
    }

    pub fn insert_rules(&mut self, edge: Edge, rules: Rules) {
        self.0.entry(edge).or_insert_with(RulesSet::new).insert_rules(rules);
    }

    pub fn insert_rule(&mut self, edge: Edge, rule: Rule) {
        self.0.entry(edge).or_insert_with(RulesSet::new).insert_rule(rule);
    }

    pub fn insert_empty(&mut self, edge: Edge) {
        self.0.entry(edge).or_default();
    }

    pub fn extend_rules_set(&mut self, edge: Edge, rules_set: RulesSet) {
        self.0.entry(edge).or_insert_with(RulesSet::new).extend(rules_set);
    }

    pub fn extend(&mut self, edges: Edges) {
        for (edge, rules_set) in edges.into_iter() {
            self.0.entry(edge).or_insert_with(RulesSet::new).extend(rules_set);
        }
    }

    pub fn get(&self, edge: &Edge) -> Option<&RulesSet> {
        self.0.get(edge)
    }

    pub fn get_mut(&mut self, edge: &Edge) -> Option<&mut RulesSet> {
        self.0.get_mut(edge)
    }

    pub fn iter(&self) -> btree_map::Iter<Edge, RulesSet> {
        self.0.iter()
    }

    pub fn iter_mut(&mut self) -> btree_map::IterMut<Edge, RulesSet> {
        self.0.iter_mut()
    }

    pub fn into_iter(self) -> btree_map::IntoIter<Edge, RulesSet> {
        self.0.into_iter()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn take(&mut self) -> BTreeMap<Edge, RulesSet> {
        mem::take(&mut self.0)
    }

    pub fn keys(&self) -> btree_map::Keys<Edge, RulesSet> {
        self.0.keys()
    }
}

pub fn print_edge(edge: &Edge, f: &mut fmt::Formatter) -> fmt::Result {
    write!(f, "({}, {})", edge.0, edge.1)
}

#[derive(Debug)]
pub struct Language {
    edges: Edges,
    completed_parses: RulesSet,
    fin: bool,
}

impl fmt::Display for Language {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "final: {}\n", self.fin)?;
        if self.fin && !self.completed_parses.is_empty() {
            write!(f, "completed parses:\n")?;
            for rules in self.completed_parses.iter() {
                write!(f, "        {}\n", rules)?;
            }
        }
        for (edge, rules_set) in self.edges.iter() {
            write!(f, "  ({}, {})", edge.state(), edge.depth())?;
            if !rules_set.is_empty() {
                write!(f, " rules:\n")?;
                for rules in rules_set.iter() {
                    write!(f, "        {}\n", rules)?;
                }
            }
            write!(f, "\n")?;
        }
        Ok(())
    }
}

impl Language {
    pub fn new() -> Language {
        Language {edges: Edges::new(), completed_parses: RulesSet::new(), fin: false}
    }

    pub fn new_from(edges: Edges, completed_parses: RulesSet, fin: bool) -> Language {
        Language {edges, completed_parses, fin}
    }

    pub fn start_lang() -> Language {
        Language {edges: Edges::new(), completed_parses: RulesSet::new(), fin: true}
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

    pub fn take_edges(&mut self) -> Edges {
        mem::take(&mut self.edges)
    }

    pub fn edges_ref(&self) -> &Edges {
        &self.edges
    }

    pub fn has_completed_parses(&self) -> bool {
        !self.completed_parses.is_empty()
    }

    pub fn take_completed_parses(&mut self) -> RulesSet {
        mem::take(&mut self.completed_parses)
    }

    pub fn completed_parses_ref(&self) -> &RulesSet {
        &self.completed_parses
    }

    pub fn insert_edge(&mut self, edge: Edge, opt_rules: Option<Rules>) {
        if let Some(rules) = opt_rules {
            self.edges.insert_rules(edge, rules);
        } else {
            self.edges.insert_empty(edge);
        }
    }

    pub fn extend_edge(&mut self, edge: Edge, rules_set: RulesSet) {
        self.edges.extend_rules_set(edge, rules_set);
    }

    pub fn insert_completed_parse(&mut self, completed_parse: Rules) {
        self.completed_parses.insert_rules(completed_parse);
    }

    pub fn extend_completed_parses(&mut self, completed_parses: RulesSet) {
        self.completed_parses.extend(completed_parses);
    }

    pub fn extend(&mut self, other: Language) {
        self.edges.extend(other.edges);
        self.completed_parses.extend(other.completed_parses);
        self.fin = self.fin || other.fin;
    }

    pub fn find_lowest_depth(&self) -> Depth {
        let mut res: Depth = Depth::new(usize::max_value());
        for (edge, _) in self.edges.iter() {
            if edge.depth() < res {
                res = edge.depth();
            }
        }
        res
    }

    pub fn adjust_lowest_depth(&mut self, lowest_depth: Depth) {
        let old_edges: Edges = std::mem::take(&mut self.edges);
        for (edge, rules_set) in old_edges.into_iter() {
            self.edges.extend_rules_set(Edge::new(edge.state(), edge.depth() - lowest_depth + 1), rules_set);
        }
    }

    pub fn make_mem_edges(&self) -> BTreeSet<Edge> {
        let mut res: BTreeSet<Edge> = BTreeSet::new();

        for edge in self.edges.keys() {
            res.insert(*edge);
        }

        res
    }

    pub fn edge_count(&self) -> usize {
        self.edges.len()
    }

    pub fn parse_count(&self) -> usize {
        let mut res: usize = 0;
        for edge in self.edges.iter() {
            if edge.1.len() > res {
                res = edge.1.len();
            }
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
        self.languages.get(self.languages.len().saturating_sub(depth.0))
    }

    pub fn insert_new_language(&mut self, lang: Language) {
        self.languages.push(lang);
    }

    pub fn pop_lang(&mut self) -> Option<Language> {
        self.languages.pop()
    }

    pub fn top_level_edge_count(&self) -> Option<usize> {
        Some(self.languages.last()?.edge_count())
    }

    pub fn top_level_parse_count(&self) -> Option<usize> {
        Some(self.languages.last()?.parse_count())
    }

    pub fn language_count(&self) -> usize {
        self.languages.len()
    }
}
