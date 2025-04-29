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
    //keep track of visited states and their environments
    let mut state_lookup: Map<State, Env> = Map::new();

    //get initial state
    let mut env = eval::init_env(program);
    for stmt in &program.start {
        eval::eval_stmt(stmt, &mut env, &program)?;
    }

    //add inital state
    let init_s = dfa::State::fresh();
    let init_e = env.clone();
    state_lookup.insert(init_s, init_e);

    // TODO: pull out the following into a helper function (eval_action) -- done

    let mut workqueue: Vec<State> = Vec::new();
    workqueue.insert(0, init_s);
    let mut accepting: Set<State> = Set::new();
    // check acceptance for init env

    while workqueue.is_empty() == false { 
        let s = workqueue.pop(); 
        for sym in &program.alphabet { //evaluate each part of alphabet for each state
            if let Some(id) = &program.action.0 {
                env.insert(id.clone(), Value::Sym(*sym));
                //TODO figure out how to collect transitions?? Is that here???
            };
        
            eval::eval_action(program, &mut env); //TODO should this env be a clone??

            //see if new env
            let mut new = false;
            for s in state_lookup.keys() {
                if state_lookup.get(s).unwrap() == &env {
                    new = true;
                }
            }
            let s_new = dfa::State::fresh();
            if new == false {
                state_lookup.insert(s, env.clone());
                workqueue.insert(workqueue.len(), s);
            }
        }
        let accept = eval::eval_expr(&program.accept, &env, &program)?;
        if accept == Value::Bool(true) {
            //assuming that all the accept statments of programs are bools
            // accepting.insert(s_new);
        }
    }

    //TODO is this the workqueue pop loop, or where would that be??
    // for sym in &program.alphabet {
    //     if let Some(id) = &program.action.0 {
    //         env.insert(id.clone(), Value::Sym(*sym));
    //         //TODO figure out how to collect transitions?? Is that here???
    //     };
        // clone env before we do this...

        

        //TODO add state names to the state name DFA map, how do we know state names tho?????

        // workqueue.push(env.clone());

        // is this a final state that accepts?
        // evaluate accept

    // }

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
