//! State enumeration
use crate::dfa;
use crate::dfa::State;
use crate::eval;
use crate::eval::RuntimeError;
use crate::eval::Value;
use crate::syntax::*;
use env_logger::init;
use thiserror::Error;

// pub enum EnumError {}

type Env = Map<Id, Value>;

/**
 *
 */
pub fn enumerate(program: &Program, input: &str) -> Result<(), RuntimeError> {
    let mut state_lookup: Map<State, Env>;
    let mut env_lookup: Map<Env, State>;

    let mut env = eval::init_env(program);
    for stmt in &program.start {
        eval::eval_stmt(stmt, &mut env, &program)?;
    }

    //add inital state
    let init_s = dfa::State::fresh();
    let init_e = env.clone();
    // state_lookup.insert(init_s, init_e);

    // TODO: pull out the following into a helper function (eval_action) -- done

    let mut workqueue: Vec<State>;
    // workqueue.insert(0, init_s);
    let mut accepting: Set<State>;
    // check acceptance for init env

    for sym in &program.alphabet {
        if let Some(id) = &program.action.0 {
            env.insert(id.clone(), Value::Sym(*sym));
        };
        // clone env before we do this...

        eval::eval_action(program, &mut env);
        //see if new env
        let new = false;
        // for s in state_lookup.keys() {
        //     // if state_lookup.get(s) == env {
        //     //     new = true;
        //     // }
        //     continue;
        // }

        // workqueue.push(env.clone());

        // is this a final state that accepts?
        // evaluate accept
        // let accept = eval::eval_expr(&program.accept, &env, &program)?;
        // if accept {
        //     accepting.insert(state)
        // }
    }

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

/* in enumberation clone environment before doing the action to it
dont clone in eval action clone in enumberate

• can either find accepting states after discovering all the states
OR in the while some(s) loop

after would look like this : let accepting = states.iter()*/
