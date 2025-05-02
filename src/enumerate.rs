//! State enumeration
use crate::dfa;
use crate::dfa::Dfa;
use crate::dfa::State;
use crate::eval;
use crate::eval::RuntimeError;
use crate::eval::Value;
use crate::syntax::*;

// pub enum EnumError {}

type Env = Map<Id, Value>;

/**
 *
 */
pub fn enumerate(program: &Program, _input: &str) -> Result<(), RuntimeError> {
    // keep track of visited states and their environments
    let mut state_lookup: Map<State, Env> = Map::new();
    //
    let mut trans: Map<State, Map<Symbol, State>> = Map::new();

    let mut env_lookup: Map<Env, State> = Map::new(); //TODO Use BTree map instead of hash??

    //get initial state
    let mut init_e = eval::init_env(program);
    for stmt in &program.start {
        eval::eval_stmt(stmt, &mut init_e, &program)?;
    }

    //add inital state
    let init_s = dfa::State::fresh();
    // let init_e = env.clone();
    state_lookup.insert(init_s, init_e.clone());

    let mut workqueue: Vec<State> = Vec::new();
    workqueue.insert(0, init_s);
    let mut accepting: Set<State> = Set::new();
    // check acceptance for init env

    while workqueue.is_empty() == false {
        let s = workqueue.pop().unwrap();
        let mut env_clone = state_lookup.get(&s).unwrap().clone();
        for sym in &program.alphabet {
            //evaluate each part of alphabet for each state
            // let map = trans.entry(s) or default();
            // for sym t alphabet:
            // m.insert(sym, t)
            if let Some(id) = &program.action.0 {
                // apparently this is in eval_action
                env_clone.insert(id.clone(), Value::Sym(*sym));
                // TODO figure out how to collect transitions?? Is that here???
            };

            eval::eval_action(program, &mut env_clone); // cloned env

            //see if new env
            let mut new = false;
            // if state_lookup.contains_key(&env_clone) {
            //     new = false;
            // }

            //-----------------------------------------
            // filters the lookup environment based on matched to env_clone
            // then sees if the list is empty or not to tell if the env is
            // new and should be added
            let contains_env = env_lookup.keys().any(|env| env == &env_clone);

            if contains_env.to_string() != "{}" {
                new = true
            }

            //------------------------------------------

            // for x in state_lookup.keys() {
            //     if state_lookup.get(x).unwrap() == &env_clone {
            //         new = true;
            //     }
            // }
            // let s_new = dfa::State::fresh();
            // if new == false {
            //     state_lookup.insert(s_new, env_clone.clone());
            //     workqueue.insert(workqueue.len(), s);
            // }

            let s_new = dfa::State::fresh();
            if new == false {
                env_lookup.insert(env_clone.clone(), s_new);
                state_lookup.insert(s_new, env_clone.clone());
                workqueue.insert(workqueue.len(), s);
            }

            let accept = eval::eval_expr(&program.accept.clone(), &env_clone.clone(), &program)?;

            if accept == Value::Bool(true) {
                //assuming that all the accept statments of programs are bools
                accepting.insert(s);
            }
        }
    }
    // TODO: Make state names

    println!("program accept: {:?}", program.accept);
    println!("state lookups: {:?}", state_lookup);
    println!("accepting states {:?}", accepting);

    // let dfa = Dfa::try_new(
    //     Set::from(program.alphabet),
    //      //     pub trans: Map<State, Map<Symbol, State>>,
    //     state_lookup, // trans.iter().map(target)
    //     init_s,
    //     Set::from(accepting),
    //     state_lookup, // pub state_names: Map<State, String>,
    // )
    // .unwrap();
    // return Ok(dfa);
    Ok(())

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
		        accept if x == 2
		    "#;
        let program = parse(input).unwrap();
        println!("program: {:?}", program);

        let result = enumerate(&program, input).unwrap();
        println!("res: {:?}", result);
        println!("--------------");
    }

    #[test]
    fn test_end() {
        let input = r#"
                alphabet: {'0','1'}
                let ends_with_zero: bool;
                on input x {
                    ends_with_zero = x == '0';
                }
                accept if ends_with_zero
		    "#;
        let program = parse(input).unwrap();
        println!("program: {:?}", program);

        let result = enumerate(&program, "1").unwrap();
        println!("res: {:?}", result);
        println!("--------------");
    }
}
