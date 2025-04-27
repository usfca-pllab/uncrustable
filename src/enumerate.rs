//! State enumeration
use crate::syntax::*;
use thiserror::Error;

pub enum EnumError {}

pub fn enumerate(program: &Program) -> Result<(), EnumError> {
    Ok(()) //placeholder return , delete later
}

// create a workqueue with the initial state (start, which we get after running program.start)
// states = {} (visited states)
// transitions = Map<>

// run loop until worklist is empty

/*
1. Need to make a workqueue (vector)
while let Some(s) = w.pop {
    // check if s is in the states, insert if its not
    process(s) // where s is state
}
2. for processing:
if eval_expr(s, accept_condition)? == Bool(True):
    accept.insert(s)
    * check if the state has been visited by seeing if it's in the states
    for each symbol in the alphabet:
        a. eval_action(curr_state, alph_sym_curr)
        b. get the next state, if we haven't processed it, add it to the work queue
while let Some(s) = w.pop { // not sure if this is where it goes
    // check if s is in the states, insert if its not
    process(s) // where s is state
}
3. we're trying to figure out the behavior for the function...
    consider ALL possible inputs (i.e. try every sym in alphabet)
4. in the DFA data structure, the states are just numbers
    * state_names: State.fresh() wil return new numbers to name them -> "ids"
5. can have a map of the states and their ids
    i.e. 0: '[is_even: false]'
         1: '[is_even: true]'
*/
