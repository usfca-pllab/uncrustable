//! State enumeration
use crate::dfa;
use crate::dfa::Dfa;
use crate::dfa::State;
use crate::eval;
use crate::eval::RuntimeError;
use crate::eval::Value;
use crate::syntax::*;
use std::collections::BTreeMap;
use std::collections::BTreeSet;

type Env = BTreeMap<Id, Value>;

/// process a program and create Dfa
pub fn enumerate(program: &Program) -> Result<Dfa<Symbol>, RuntimeError> {
    let mut state_lookup: Map<State, Env> = Map::new();
    let mut trans: BTreeMap<State, BTreeMap<Symbol, State>> = BTreeMap::new();
    let mut names: BTreeMap<State, String> = BTreeMap::new();
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
        let mut s_edges = BTreeMap::new();
        let curr_env = state_lookup.get(&s).unwrap().clone();
        for sym in &program.alphabet {
            let mut env_clone = curr_env.clone();
            eval::eval_action(program, &mut env_clone, sym)?;

            if let Some(_) = &program.action.0 {
                env_clone.remove(&program.action.0.unwrap());
            }

            let mut new = false;

            let t = *env_lookup.entry(env_clone.clone()).or_insert_with(|| {
                new = true;
                dfa::State::fresh()
            });

            s_edges.insert(*sym, t);
            if new {
                state_lookup.insert(t, env_clone.clone());
                worklist.insert(worklist.len(), t);
            }
        }
        trans.insert(s, s_edges);
    }
    for st in state_lookup.keys().clone() {
        let state_env = state_lookup.get(st).unwrap();
        let accept = eval::eval_expr(&program.accept.clone(), &state_env.clone(), &program)?;
        if accept == Value::Bool(true) {
            // assuming that all the accept statments of programs are bools
            accepting.insert(*st);
        }
        names.insert(
            *st,
            state_env
                .iter()
                .map(|(x, v)| format!("{x} = {v}"))
                .collect::<Vec<_>>()
                .join(", "),
        );
    }
    // let alph = BTreeSet::from_iter(program.alphabet);

    let dfa = Dfa::try_new(
        BTreeSet::from_iter(program.alphabet.clone()),
        trans,
        init_s,
        BTreeSet::from_iter(accepting),
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
                    x = 3 as int[0..3];   
                }
                accept if x == 2 as int[0..2]
            "#;
        let program = parse(input).unwrap();

        let result = enumerate(&program).unwrap();
        let dfa: Dfa<Symbol> = Dfa::try_new(
            BTreeSet::from([Symbol('a')]),
            BTreeMap::from([
                (
                    State::new(0),
                    BTreeMap::from([(Symbol('a'), State::new(1))]),
                ),
                (
                    State::new(1),
                    BTreeMap::from([(Symbol('a'), State::new(1))]),
                ),
            ]),
            State::new(0),
            BTreeSet::from([State::new(0), State::new(1)]),
            BTreeMap::from([
                (State::new(1), "x = 3 as [3..4]".to_string()),
                (State::new(0), "x = 0 as [0..4]".to_string()),
            ]),
        )
        .unwrap();

        assert_eq!(result.compare(&dfa), None);
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

        let result = enumerate(&program).unwrap();

        let dfa: Dfa<Symbol> = Dfa::try_new(
            BTreeSet::from([Symbol('0'), Symbol('1')]),
            BTreeMap::from([
                (
                    State::new(1),
                    BTreeMap::from([(Symbol('1'), State::new(0)), (Symbol('0'), State::new(1))]),
                ),
                (
                    State::new(0),
                    BTreeMap::from([(Symbol('0'), State::new(1)), (Symbol('1'), State::new(0))]),
                ),
            ]),
            State::new(0),
            BTreeSet::from([State::new(1)]),
            BTreeMap::from([
                (State::new(1), "ends_with_zero = true".to_string()),
                (State::new(0), "ends_with_zero = false".to_string()),
            ]),
        )
        .unwrap();

        assert_eq!(result.compare(&dfa), None);
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

        let result = enumerate(&program).unwrap();

        let dfa: Dfa<Symbol> = Dfa::try_new(
            BTreeSet::from([Symbol('0'), Symbol('1')]),
            BTreeMap::from([
                (
                    State::new(1),
                    BTreeMap::from([(Symbol('1'), State::new(0)), (Symbol('0'), State::new(2))]),
                ),
                (
                    State::new(0),
                    BTreeMap::from([(Symbol('0'), State::new(0)), (Symbol('1'), State::new(1))]),
                ),
                (
                    State::new(2),
                    BTreeMap::from([(Symbol('0'), State::new(1)), (Symbol('1'), State::new(2))]),
                ),
            ]),
            State::new(0),
            BTreeSet::from([State::new(0)]),
            BTreeMap::from([
                (State::new(0), "rem = 0 as [0..3]".to_string()),
                (State::new(1), "rem = 1 as [0..3]".to_string()),
                (State::new(2), "rem = 2 as [0..3]".to_string()),
            ]),
        )
        .unwrap();

        assert_eq!(result.compare(&dfa), None);
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
        let result = enumerate(&program).unwrap();

        let dfa: Dfa<Symbol> = Dfa::try_new(
            BTreeSet::from([Symbol('d'), Symbol('a')]),
            BTreeMap::from([
                (
                    State::new(1),
                    BTreeMap::from([(Symbol('d'), State::new(1)), (Symbol('a'), State::new(2))]),
                ),
                (
                    State::new(0),
                    BTreeMap::from([(Symbol('a'), State::new(2)), (Symbol('d'), State::new(1))]),
                ),
                (
                    State::new(2),
                    BTreeMap::from([(Symbol('d'), State::new(1)), (Symbol('a'), State::new(2))]),
                ),
            ]),
            State::new(0),
            BTreeSet::from([State::new(0), State::new(1)]),
            BTreeMap::from([
                (State::new(0), "w = false, z = false".to_string()),
                (State::new(1), "w = false, z = true".to_string()),
                (State::new(2), "w = true, z = true".to_string()),
            ]),
        )
        .unwrap();

        assert_eq!(result.compare(&dfa), None);
    }
}
