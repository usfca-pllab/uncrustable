//! State enumeration
use crate::dfa;
use crate::dfa::Dfa;
use crate::dfa::State;
use crate::eval;
use crate::eval::RuntimeError;
use crate::eval::Value;
use crate::syntax::*;
use std::collections::BTreeMap;

type Env = BTreeMap<Id, Value>;

fn eval_action(program: &Program, env: &mut Env) {
    // evaluate action
    for stmt in &program.action.1 {
        eval::eval_stmt(stmt, env, &program);
    }
}
/**
 *
 */
pub fn enumerate(program: &Program, _input: &str) -> Result<(), RuntimeError> {
    // keep track of visited states and their environments
    // let mut state_lookup: Map<State, Env> = Map::new();
    let mut state_lookup: Map<State, Env> = Map::new();
    // the transitions between states
    let mut trans: Map<State, Map<Symbol, State>> = Map::new();
    //state names
    let mut names: Map<State, String> = Map::new();

    let mut env_lookup: BTreeMap<Env, State> = BTreeMap::new();

    //get initial state
    let mut init_e = eval::init_env(program);
    for stmt in &program.start {
        eval::eval_stmt(stmt, &mut init_e, &program)?;
    }

    //add inital state
    let init_s = dfa::State::fresh();
    state_lookup.insert(init_s, init_e.clone());
    env_lookup.insert(init_e.clone(), init_s);

    let mut workqueue: Vec<State> = Vec::new();
    workqueue.insert(0, init_s);
    // println!("workqueue: {:?}", workqueue);
    let mut accepting: Set<State> = Set::new();
    // check acceptance for init env

    let mut first = true;
    while workqueue.is_empty() == false {
        println!("workqueue: {:?}", workqueue);
        let s = workqueue.pop().unwrap();
        let mut s_edges: Map<Symbol, State> = Map::new();
        let mut env_clone = state_lookup.get(&s).unwrap().clone();
        for sym in &program.alphabet {
            if let Some(id) = &program.action.0 {
                // apparently this is in eval_action
                env_clone.insert(id.clone(), Value::Sym(*sym));
            };

            eval_action(program, &mut env_clone); // cloned env
            if let Some(id) = &program.action.0 {
                env_clone.remove(&program.action.0.unwrap());
            }
            println!("curent state: {:?}", s);
            println!("current env: {:?}", env_clone);
            // do an if let to remove like above ^
            //see if new env
            let mut new = true;
            if env_lookup.contains_key(&env_clone) {
                println!("env_lookup:");
                new = false;
            }

            if new == true {
                let s_new = dfa::State::fresh();
                env_lookup.insert(env_clone.clone(), s_new);
                state_lookup.insert(s_new, env_clone.clone());
                workqueue.insert(workqueue.len(), s_new);
                s_edges.insert(*sym, s_new);
            } else {
                s_edges.insert(*sym, s);
            }
            // delete the input var in the enum
        }
        trans.insert(s, s_edges);
    }
    for st in state_lookup.keys().clone() {
        let accept = eval::eval_expr(
            &program.accept.clone(),
            &state_lookup.get(&st).unwrap().clone(),
            &program,
        )?;
        if accept == Value::Bool(true) {
            // assuming that all the accept statments of programs are bools
            accepting.insert(*st);
        }
        // let mut name = "";
        // let state_env = state_lookup.get(st).unwrap();
        // let f = state_env.first_key_value().unwrap();
        // name = &(f.0.to_string() + " = " + f.1.fmt());
        
    }
    // TODO: Make state names

    println!("program accept: {:?}", program.accept);
    println!("state lookups: {:?}", state_lookup);
    println!("accepting states {:?}", accepting);
    println!("transitions {:?}", trans);

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
}

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
                accept if ends_with_zero == true
		    "#;
        let program = parse(input).unwrap();
        println!("program: {:?}", program);

        let result = enumerate(&program, "1").unwrap();
        println!("res: {:?}", result);
        println!("--------------");
    }

    #[test]
    fn test_even() {
        let input = r#"
                alphabet: {'0','1','2','3'}
                let is_even: bool;
                on input x {
                    is_even = x % 2 == 0;
                }
                accept if is_even == true
		    "#;
        let program = parse(input).unwrap();
        println!("program: {:?}", program);

        let result = enumerate(&program, "1").unwrap();
        println!("res: {:?}", result);
        println!("--------------");
    }

    #[test]
    fn test_div3() {
        let input = r#"
                alphabet: { '0', '1' }
                fn char_to_bit(c: sym) -> int[2] = match c {
                '0' -> 0 as int[2]
                '1' -> 1 as int[2]
                }
                let rem: int[3];
                on input bit {
                    rem = 2 as int[3] * rem + char_to_bit(bit) as int[3];
                }
                accept if rem == 0 as int[3]
		    "#;
        let program = parse(input).unwrap();
        println!("program: {:?}", program);

        let result = enumerate(&program, "1").unwrap();
        println!("res: {:?}", result);
        println!("--------------");
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
