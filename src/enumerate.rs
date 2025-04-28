//! State enumeration
use crate::dfa::State;
use crate::eval;
use crate::eval::RuntimeError;
use crate::eval::Value;
use crate::syntax::*;
use thiserror::Error;

// pub enum EnumError {}

/**
 * 
 */
pub fn enumerate(program: &Program, input: &str) -> Result<(), RuntimeError> {
    let mut env = eval::init_env(program);
    for stmt in &program.start {
        eval::eval_stmt(stmt, &mut env, &program)?;
    }

    // TODO: pull out the following into a helper function (eval_action)

    let mut workqueue: Vec<State>;

    for sym in &program.alphabet {
        if let Some(id) = &program.action.0 {
            env.insert(id.clone(), Value::Sym(*sym));
        };

        for stmt in &program.action.1 {
            eval::eval_stmt(stmt, &mut env, &program)?;
        }
        workqueue.push(env.clone());
        
    }

    // evaluate accept
    let accept = eval::eval_expr(&program.accept, &env, &program)?;

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
