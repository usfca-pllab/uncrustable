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

/**
 *
 */
pub fn enumerate(program: &Program, _input: &str) -> Result<Dfa<Symbol>, RuntimeError> {
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
    let mut accepting: Set<State> = Set::new();
    // check acceptance for init env

    while workqueue.is_empty() == false {
        println!("workqueue: {:?}", workqueue);
        let s = workqueue.pop().unwrap();
        let mut s_edges: Map<Symbol, State> = Map::new();
        let mut env_clone = state_lookup.get(&s).unwrap().clone();
        for sym in &program.alphabet {
            // TODO: move to eval_action
            if let Some(id) = &program.action.0 {
                // apparently this is in eval_action
                env_clone.insert(id.clone(), Value::Sym(*sym));
            };

            eval::eval_action(program, &mut env_clone, sym); // cloned env
            if let Some(id) = &program.action.0 {
                env_clone.remove(&program.action.0.unwrap());
            }

            let mut new = true;
            if env_lookup.contains_key(&env_clone) {
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

        let state_env = state_lookup.get(st).unwrap();
        let f = state_env.first_key_value().unwrap();
        names.insert(*st, format!("{} = {}", f.0, f.1).to_string());
    }

    let dfa = Dfa::try_new(
        Set::from(program.alphabet.clone()),
        trans,
        init_s,
        Set::from(accepting),
        names,
    )
    .unwrap();
    return Ok(dfa);
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
