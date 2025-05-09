//! Deterministic Finite Automata

use crate::syntax::Symbol;
use std::collections::BTreeMap;
pub use std::collections::{BTreeMap as Map, BTreeSet as Set};
use std::fmt;
use std::fmt::Debug;
use std::hash::Hash;
use std::io::Write;
pub mod parser;

/// DFA states.  Never create a state object yourself, always use
/// `State::fresh`.
#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct State(u32);
use std::sync::atomic::{AtomicU32, Ordering};
/// Next available state, do not use directly.
static NEXT_STATE: AtomicU32 = AtomicU32::new(0);

impl State {
    /// Generate a fresh state that hasn't been used before
    pub fn fresh() -> State {
        State(NEXT_STATE.fetch_add(1, Ordering::SeqCst))
    }

    /// reset State counter
    #[cfg(test)]
    pub fn reset() {
        NEXT_STATE.store(0, Ordering::SeqCst)
    }

    /// public State generation only for test cases
    #[cfg(test)]
    pub fn new(s: u32) -> State {
        State(s)
    }
}

// Pretty Print for DFA
impl fmt::Display for Dfa<Symbol> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "flowchart TD")?; // Mermaid header

        //  nodes: iterate through all the states, mark accepting states
        self.states.iter().try_for_each(|state| {
            let state_id = format!("q{}", state.0);
            let label = self.state_names.get(state).cloned().unwrap();
            // let mut layer = String::new();
            if self.accepting.contains(state) {
                write!(f, "  {}(((\"{}\")))", state_id, label) // double circle for accepting
            } else {
                write!(f, "  {}((\"{}\"))", state_id, label)
            }
        })?;

        // transitions
        self.trans.iter().try_for_each(|(from_state, trans_map)| {
            trans_map.iter().try_for_each(|(symbol, to_state)| {
                writeln!(f, "  q{} --{}--> q{}", from_state.0, symbol, to_state.0)
            })
        })?;

        Ok(())
    }
}

/// Potential errors during DFA construction
#[derive(Debug)]
pub enum DfaValidationError<Symbol> {
    /// There is a missing transition for given state
    MissingTransition(State),
    /// There is a missing transition for given state and symbol combination
    MissingTransitionForPair(State, Symbol),
    /// There is a symbol in the transition table which is not on the alphabet
    SymbolNotInAlphabet(Symbol),
    /// State occurs in transitions but not in the set of states
    StateNotInStateSet(State),
}

/// A deterministic finite automaton.
#[derive(Debug)]
pub struct Dfa<Symbol: Hash + Eq> {
    /// The set of all possible states
    pub states: Set<State>,
    /// The set of all possible symbols
    pub alphabet: Set<Symbol>,
    /// Curried transition function
    pub trans: Map<State, Map<Symbol, State>>,
    /// Start state
    pub start: State,
    /// Accepting states
    pub accepting: Set<State>,
    /// State names, optional
    pub state_names: Map<State, String>,
}

impl<Symbol: Hash + Eq + Ord> Dfa<Symbol> {
    /// Check if this DFA is valid
    pub fn validate(&self) -> Result<(), Vec<DfaValidationError<Symbol>>>
    where
        Symbol: Copy,
    {
        use DfaValidationError::*;
        let mut errors = vec![];

        for state in &self.states {
            if let Some(trans) = self.trans.get(state) {
                errors.extend(self.alphabet.iter().filter_map(|symbol| {
                    if !trans.contains_key(symbol) {
                        Some(MissingTransitionForPair(*state, *symbol))
                    } else {
                        None
                    }
                }));

                errors.extend(
                    trans
                        .keys()
                        .copied()
                        .filter(|k| !self.alphabet.contains(k))
                        .map(SymbolNotInAlphabet),
                );
            } else {
                errors.push(MissingTransition(*state));
            }
        }

        errors.extend(
            self.trans
                .keys()
                .copied()
                .chain(self.trans.values().flat_map(|m| m.values().copied()))
                .filter(|state| !self.states.contains(state))
                .map(StateNotInStateSet),
        );

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    /// Always build a DFA using this method to ensure that we never create an
    /// invalid DFA.
    pub fn try_new(
        alphabet: Set<Symbol>,
        trans: Map<State, Map<Symbol, State>>,
        init: State,
        accepting: Set<State>,
        state_names: Map<State, String>,
    ) -> Result<Self, Vec<DfaValidationError<Symbol>>>
    where
        Symbol: Copy,
    {
        // Collect all the states from everything else
        let states = accepting
            .iter()
            .copied()
            .chain(Some(init))
            .chain(trans.keys().copied())
            .chain(trans.values().flat_map(|m| m.values().copied()))
            .collect::<Set<State>>();

        let dfa = Dfa {
            states,
            alphabet,
            trans,
            start: init,
            accepting,
            state_names,
        };
        dfa.validate()?;
        Ok(dfa)
    }

    /// Run the DFA on given string.
    pub fn run<'a, I: Iterator<Item = &'a Symbol>>(&'a self, input: I) -> State {
        input.fold(self.start, |state, symbol| self.trans[&state][symbol])
    }

    /// Run the DFA on given string and check if it accepts the input.
    pub fn accepts<'a, I: Iterator<Item = &'a Symbol>>(&'a self, input: I) -> bool {
        self.accepting
            .contains(&input.fold(self.start, |state, symbol| self.trans[&state][symbol]))
    }

    /// Return the minimal DFA that recognizes the same language.
    pub fn minimize(&self) -> Self {
        todo!()
    }
}

impl<Symbol: Hash + Eq + Ord + Debug> Dfa<Symbol> {
    /// Run the DFA on given string and check if it accepts the input.  If the
    /// two DFA are equal, this function returns `None`.  Otherwise, it returns
    /// a counterexample (an input where the two DFA differ).
    pub fn compare(&self, other: &Self) -> Option<Vec<Symbol>>
    where
        Symbol: Copy,
    {
        // Build the cartesian product DFA
        assert!(self.alphabet == other.alphabet);

        let mut alphabet = self.alphabet.clone().into_iter().collect::<Vec<_>>();
        alphabet.sort();
        let start = (self.start, other.start);

        // start from the start state, and use a worklist algorithm to calculate
        // all reachable states.  For each reachable state pair, maintain a
        // string that leads to that state pair.

        let mut visited: BTreeMap<(State, State), Vec<Symbol>> = BTreeMap::from([(start, vec![])]);

        eprintln!("{:?}", self.accepting);
        eprintln!("{:?}", other.accepting);

        let mut changed = true;
        while changed {
            changed = false;
            for ((state1, state2), path) in visited.clone() {
                for symbol in alphabet.iter() {
                    let next = (self.trans[&state1][symbol], other.trans[&state2][symbol]);
                    let mut next_path = path.clone();
                    next_path.push(*symbol);

                    if self.accepting.contains(&next.0) != other.accepting.contains(&next.1) {
                        return Some(next_path);
                    }

                    if let std::collections::btree_map::Entry::Vacant(e) = visited.entry(next) {
                        e.insert(next_path.clone());
                        changed = true;
                    }
                }
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_display() {
        State::reset();
        let expected_output = "flowchart TD\n  q0(((\"w = false, z = false\")))  q1(((\"w = false, z = true\")))  q2((\"w = true, z = true\"))  q0 --a--> q2\n  q0 --d--> q1\n  q1 --a--> q2\n  q1 --d--> q1\n  q2 --a--> q2\n  q2 --d--> q1\n";
        let dfa: Dfa<Symbol> = Dfa::try_new(
            Set::from([Symbol('d'), Symbol('a')]),
            Map::from([
                (
                    State::new(1),
                    Map::from([(Symbol('d'), State::new(1)), (Symbol('a'), State::new(2))]),
                ),
                (
                    State::new(0),
                    Map::from([(Symbol('a'), State::new(2)), (Symbol('d'), State::new(1))]),
                ),
                (
                    State::new(2),
                    Map::from([(Symbol('d'), State::new(1)), (Symbol('a'), State::new(2))]),
                ),
            ]),
            State::new(0),
            Set::from([State::new(0), State::new(1)]),
            Map::from([
                (State::new(0), "w = false, z = false".to_string()),
                (State::new(1), "w = false, z = true".to_string()),
                (State::new(2), "w = true, z = true".to_string()),
            ]),
        )
        .unwrap();

        let output = format!("{}", dfa);
        assert_eq!(expected_output, output);
    }

    #[test]
    fn test_new_valid() {
        let dfa = Dfa::try_new(
            Set::from(['a', 'b']),
            Map::from([
                (State(0), Map::from([('a', State(1)), ('b', State(0))])),
                (State(1), Map::from([('a', State(1)), ('b', State(0))])),
            ]),
            State(0),
            Set::from([State(1)]),
            Map::new(),
        )
        .unwrap();

        assert_eq!(dfa.alphabet, Set::from(['a', 'b']));
        assert_eq!(dfa.start, State(0));
        assert_eq!(dfa.accepting, Set::from([State(1)]));
        assert_eq!(
            dfa.trans,
            Map::from([
                (State(0), Map::from([('a', State(1)), ('b', State(0))])),
                (State(1), Map::from([('a', State(1)), ('b', State(0))])),
            ])
        );
        assert_eq!(dfa.state_names, Map::new());
    }

    #[test]
    fn test_new_invalid() {
        let dfa = Dfa::try_new(
            Set::from(['a', 'b']),
            Map::from([
                (State(0), Map::from([('a', State(1))])),
                (State(1), Map::from([('a', State(1)), ('b', State(0))])),
            ]),
            State(0),
            Set::from([State(1)]),
            Map::new(),
        );

        assert!(dfa.is_err());
    }

    #[test]
    fn test_accepts() {
        let dfa = Dfa::try_new(
            Set::from(['a', 'b']),
            Map::from([
                (State(0), Map::from([('a', State(1)), ('b', State(0))])),
                (State(1), Map::from([('a', State(1)), ('b', State(0))])),
            ]),
            State(0),
            Set::from([State(1)]),
            Map::new(),
        )
        .unwrap();

        assert!(dfa.accepts("a".chars().collect::<Vec<_>>().iter()));
        assert!(!dfa.accepts("b".chars().collect::<Vec<_>>().iter()));
        assert!(dfa.accepts("aa".chars().collect::<Vec<_>>().iter()));
        assert!(!dfa.accepts("ab".chars().collect::<Vec<_>>().iter()));
        assert!(dfa.accepts("ba".chars().collect::<Vec<_>>().iter()));
        assert!(!dfa.accepts("bb".chars().collect::<Vec<_>>().iter()));
    }

    #[test]
    fn test_compare() {
        let dfa1 = Dfa::try_new(
            Set::from(['a', 'b']),
            Map::from([
                (State(0), Map::from([('a', State(1)), ('b', State(0))])),
                (State(1), Map::from([('a', State(1)), ('b', State(0))])),
            ]),
            State(0),
            Set::from([State(1)]),
            Map::new(),
        )
        .unwrap();

        let dfa2 = Dfa::try_new(
            Set::from(['a', 'b']),
            Map::from([
                (State(0), Map::from([('a', State(1)), ('b', State(0))])),
                (State(1), Map::from([('a', State(1)), ('b', State(0))])),
            ]),
            State(0),
            Set::from([State(1)]),
            Map::new(),
        )
        .unwrap();

        assert_eq!(dfa1.compare(&dfa2), None);

        let dfa3 = Dfa::try_new(
            Set::from(['a', 'b']),
            Map::from([
                (State(0), Map::from([('a', State(1)), ('b', State(0))])),
                (State(1), Map::from([('a', State(1)), ('b', State(0))])),
            ]),
            State(0),
            Set::from([State(0)]),
            Map::new(),
        )
        .unwrap();

        assert_eq!(dfa1.compare(&dfa3), Some(vec!['a']));

        let dfa4 = Dfa::try_new(
            Set::from(['a', 'b']),
            Map::from([
                (State(0), Map::from([('a', State(0)), ('b', State(1))])),
                (State(1), Map::from([('a', State(0)), ('b', State(1))])),
            ]),
            State(0),
            Set::from([State(1)]),
            Map::new(),
        )
        .unwrap();

        assert_eq!(dfa1.compare(&dfa4), Some(vec!['a']));
    }
}
