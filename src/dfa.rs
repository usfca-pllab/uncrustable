//! Deterministic Finite Automata

use std::collections::BTreeMap;
pub use std::collections::{HashMap as Map, HashSet as Set};
use std::fmt::Debug;
use std::hash::Hash;

pub mod parser;

/// DFA states.  Never create a state object yourself, always use
/// `State::fresh`.
#[derive(Clone, Copy, Debug, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct State(u32);

impl State {
    /// Generate a fresh state that hasn't been used before
    pub fn fresh() -> State {
        use std::sync::atomic::{AtomicU32, Ordering};
        /// Next available state, do not use directly.
        static NEXT_STATE: AtomicU32 = AtomicU32::new(0);
        State(NEXT_STATE.fetch_add(1, Ordering::SeqCst))
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

impl<Symbol: Hash + Eq> Dfa<Symbol> {
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
