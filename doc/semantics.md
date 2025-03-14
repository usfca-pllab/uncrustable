# Semantics of uncrustable programs

## Semantic domains

Values in an uncrustable program range over:

```
i ∈ Int[n..m]  -- Integers within a specified range
b ∈ Bool = {true, false}

v ∈ Value ::= i | σ | b
```

## Program state

The state of the program consists of the values of the locals (the environment):

```
env ∈ Env = Var ⇀ Value
```

### State for functions

Informally, each function has access it its parameters and nothing else.

## Semantic relations

We will define the following semantic relations:

```
[[ e ]] : Env ⇀ Value
[[ s ]] : Env ⇀ Env
```

- These relations are partial due to the possibility of pattern match failures
  and bounds checking failures.  Otherwise, well-typed programs should not get
  stuck.
- In both these relations, the function definitions and the alphabet are
  available as global constants.

Here are the definitions of both:

```
# [[ e ]]

[[ x ]] env = env(x)
[[ σ ]] env = σ
[[ n ]] env = n
[[ true ]] env = true
[[ false ]] env = false
[[ e as τ overf ]] env = cast([[ e ]], τ, overf)
[[ e₁ ⊕ e₂ ]] env = ([[ e₁ ]] env) [[⊕]] ([[ e₂ ]] env)
[[ ⊞ e ]] env = [[⊞]] ([[ e ]] env)
[[ f(e₁, …, eₙ) ]] env = [[ eFun ]] envFun
  where
    "(x₁: τ₁, …, xₙ: τₙ) -> τᵣ = eFun" = F(f)
    envFun = [x₁ ↦ [[e₁]] env, …, xₙ ↦ [[eₙ]] env]   -- this is defining a function/map
[[ match e { (pattern eGuard eOut) :: cases } ]] env =
  let v = [[ e ]] env in
    if v == pattern && [[ eGuard ]] env then [[ eOut ]] env
                                      else [[ match e { cases } ]] env

# [[ s ]]
[[ x = e ]] env = env[x ↦ v]
  where v = [[ e ]] env
[[ if e sTrue* else sFalse* ]] env = case [[ e ]] env of
  true -> [[ sTrue* ]] env
  false -> [[ sFalse* ]] env

# [[ s* ]] -- this just runs statements in sequence

[[ s₁ :: sᵣₑₛₜ ]] env = [[ sᵣₑₛₜ ]] ([[ s₁ ]] env)
[[ ]] env = env
```

Note that a match with 0 cases automatically fails (this would happen on a
recursive call).  Also, some rules fail if the program is not well-typed (e.g.,
the guard of the if evaluates to something other than a boolean).

Here are the definitions of the helpers:

- `[[⊕]]` executes given binary operation.  It also applies the operation
  w.r.t. the correct modulus if that information is available.  Otherwise, it
  infers the modulus from the left-hand side argument (for some operations, both
  arguments must have the same modulus).
- `[[⊞]]` executes given binary operation.  It executes numeric operations based
  on the range of its input.  If the range cannot fit the result, the evaluation
  gets stuck.
- `τ` in `cast(v, τ, overf)` has to be an integer type, `v` should also be an
  integer.  Otherwise, casting would get stuck.
- `cast(v, τ, fail)` converts `v` to type `τ` and fails if the result cannot fit in `τ`.
- `cast(v, τ, wraparound)` converts `v` to type `τ` and wraps out-of-bounds values around.
- `cast(v, τ, saturate)` converts `v` to type `τ` and returns the minimum/the
  maximum value for out-of-bounds values.

All `cast` functions are defined on only numeric values and numeric types.

### Casting wraparound

Here is how casting should work in wraparound mode:

```
cast(n, int[lower..upper], wraparound) = (n - lower) % (upper - lower) + lower
```

All arithmetic should be done with unbounded integers.  In practice, doing
everything with `i64` in Rust should suffice because we cannot handle large
bounds when compiling anyway (the resulting automaton would have too many
states).

## Execution of the program

Informally,
1. The program starts with all locals initialized to the minimum number in the
   range, the first symbol, or false depending on the type.
2. The initialization statements are run.
3. The actions are run for each input symbol.
4. The acceptance condition is checked.


We assume the alphabet (the set `Symbol`) is available as a global constant.

Overall, the evaluation function for a program does the following:

```
[[ p ]] : Symbol* ⇀ Bool
[[ F × LocalTypes × sᵢₙᵢₜ × action × eₐ ]](σ*) =
  let env₀ = init(LocalTypes) in
  let envFinal = fold [[action]] ([[sᵢₙᵢₜ]] envₒ) σ* in
  [[ eₐ ]] envFinal
```

Here,
- the helper `init` takes the types of each variable and initializes them to the
  smallest value in their respective types.
- the helper `[[action]]` has the following meaning:
  - `[[on input x s*]] = λ(env,σ). [[s*]] env[x ↦ σ]`
  - `[[on input s*]] = λ(env,σ). [[s*]] env`
- `fold f a [b₁, …, bₙ]` stands for `f(bₙ, … f(b₂, f(b₁, a)))`.
- if any partial function fails, execution gets stuck.
- if the final evaluation does not result in a boolean, execution gets stuck.
