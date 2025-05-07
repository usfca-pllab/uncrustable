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

/// Populates the DFA struct
pub fn enumerate(program: &Program, _input: &str) -> Result<Dfa<Symbol>, RuntimeError> {
    let mut state_lookup: Map<State, Env> = Map::new();
    let mut trans: Map<State, Map<Symbol, State>> = Map::new();
    let mut names: Map<State, String> = Map::new();
    let mut env_lookup: BTreeMap<Env, State> = BTreeMap::new();

    let mut init_e = eval::init_env(program);
    for stmt in &program.start {
        eval::eval_stmt(stmt, &mut init_e, &program)?;
    }

    let init_s = dfa::State::fresh();
    state_lookup.insert(init_s, init_e.clone());
    env_lookup.insert(init_e.clone(), init_s);

    let mut worklist: Vec<State> = Vec::new();
    worklist.insert(0, init_s);
    let mut accepting: Set<State> = Set::new();

    while worklist.is_empty() == false {
        let s = worklist.pop().unwrap();
        let mut s_edges: Map<Symbol, State> = Map::new();
        let curr_env = state_lookup.get(&s).unwrap().clone();
        for sym in &program.alphabet {
            let mut env_clone = curr_env.clone();
            eval::eval_action(program, &mut env_clone, sym)?;

            if let Some(_) = &program.action.0 {
                env_clone.remove(&program.action.0.unwrap());
            }

            let mut new = true;
            if env_lookup.contains_key(&env_clone) {
                new = false;
            }

            let t = env_lookup
                .entry(env_clone.clone())
                .or_insert_with(|| dfa::State::fresh());

            s_edges.insert(*sym, t.clone());
            if new == true {
                state_lookup.insert(*t, env_clone.clone());
                worklist.insert(worklist.len(), t.clone());
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
        println!("res: {:#?}", result);
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
        println!("res: {:#?}", result);
        println!("--------------");
    }

    #[test]
    fn test_div3() {
        let input = r#"
                alphabet: { '0', '1' }
                fn char_to_bit(c: sym) -> int[0..3] = match c {
                    '0' -> 0
                    '1' -> 1
                }
                let rem: int[0..3];
                on input bit {
                    rem = (2 as int[0..3] * rem as int[0..3]) + (char_to_bit(bit) as int[0..3]);
                }
                accept if rem == 0
		    "#;
        let program = parse(input).unwrap();
        println!("program: {:?}", program);

        let result = enumerate(&program, "01").unwrap();
        println!("res: {:#?}", result);
        println!("res: {}", result);
        println!("--------------");
    }

    #[test]
    fn test_odd() {
        let input = r#"
                alphabet: { '0', '1' }
                let length_parity: int[2];
                on input {
                    length_parity = length_parity + 1;
                }
                accept if length_parity == 1
		    "#;
        let program = parse(input).unwrap();
        println!("program: {:?}", program);

        let result = enumerate(&program, "01").unwrap();
        println!("res: {}", result);
        println!("--------------");
    }

    #[test]
    // Boolean BinOp - And / Or
    fn test_binop_4() {
        let input = r#"
	        alphabet: {'d', 'a'}
	        let z: bool;
	        let w: bool;
	        on input y {
                z = true;
                w = match y {
                    'd' -> true && false
                    y if true -> true || false
                };
	        }
	        accept if w == false
	    "#;
        let program = parse(input).unwrap();
        let dfa = enumerate(&program, input).unwrap();
        println!("res: {}", dfa);
    }
}
