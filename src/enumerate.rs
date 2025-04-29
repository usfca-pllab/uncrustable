//! State enumeration
use crate::dfa;
use crate::dfa::State;
use crate::dfa::Dfa;
use crate::eval;
use crate::eval::RuntimeError;
use crate::eval::Value;
use crate::syntax::*;
use env_logger::init;
use pest::state;
use thiserror::Error;

// pub enum EnumError {}

type Env = Map<Id, Value>;

/**
 *
 */
pub fn enumerate(program: &Program, _input: &str) -> Result<(), RuntimeError> {
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

    let mut workqueue: Vec<State> = Vec::new();
    workqueue.insert(0, init_s);
    let mut accepting: Set<State> = Set::new();
    // check acceptance for init env

    while workqueue.is_empty() == false {
        let s = workqueue.pop().unwrap();
        let mut env_clone = state_lookup.get(&s).unwrap().clone();
        for sym in &program.alphabet {
            //evaluate each part of alphabet for each state
            if let Some(id) = &program.action.0 {
                env.insert(id.clone(), Value::Sym(*sym));
                // TODO figure out how to collect transitions?? Is that here???
            };
            
            eval::eval_action(program, &mut env_clone); // cloned env

            //see if new env
            let mut new = false;
            for x in state_lookup.keys() {
                if state_lookup.get(x).unwrap() == &env_clone {
                    new = true;
                }
            }
            let s_new = dfa::State::fresh();
            if new == false {
                state_lookup.insert(s_new, env_clone.clone());
                workqueue.insert(workqueue.len(), s);
            }
            let accept = eval::eval_expr(&program.accept, &env_clone, &program)?;
            println!("accept: {:?}", accept);
            if accept == Value::Bool(true) {
                //assuming that all the accept statments of programs are bools
                accepting.insert(s);
            }
        }
    }
    println!("program accept: {:?}", program.accept);
    println!("state lookups: {:?}", state_lookup);
    println!("accepting states {:?}", accepting);
    Ok(())

    // let dfa = Dfa::try_new(
    //     Set::from(['a', 'b']),
    //     Map::from([
    //         sta
    //         (State(1), Map::from([('a', State(1)), ('b', State(0))])),
    //     ]),
    //     State(0),
    //     Set::from([State(1)]),
    //     Map::new(),
    // )
    // .unwrap();
    // return Ok(accepting);

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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::parse;
    use crate::syntax::*;

    #[test]
    // Simple Local Assignment
    fn test_assign() {
        let input = r#"
		        alphabet: {'a'}
		        let x: int[4];
		        on input y {
					x = 3;   
		        }
		        accept if x == 3
		    "#;
        let program = parse(input).unwrap();
        println!("program: {:?}", program);

        let result = enumerate(&program, input).unwrap();
        println!("res: {:?}", result);
    }
}



