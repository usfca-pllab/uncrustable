//! Deterministic Finite Automata

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
            .chain(Some(init).into_iter())
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
    pub fn run<'a, I: Iterator<Item=&'a Symbol>>(&'a self, input: I) -> State {
        input.fold(self.start, |state, symbol| self.trans[&state][symbol])
    }

    /// Run the DFA on given string and check if it accepts the input.
    pub fn accepts<'a, I: Iterator<Item=&'a Symbol>>(&'a self, input: I) -> bool {
        self.accepting.contains(&input.fold(self.start, |state, symbol| self.trans[&state][symbol]))
    }

    /// Run the DFA on given string and check if it accepts the input.  If the
    /// two DFA are equal, this function returns `None`.  Otherwise, it returns
    /// a counterexample (an input where the two DFA differ).
    pub fn compare(&self, other: Self) -> Option<Vec<Symbol>> where Symbol: Copy {
        todo!()
    }

    /// Return the minimal DFA that recognizes the same language.
    pub fn minimize(&self) -> Self {
        todo!()
    }
}
