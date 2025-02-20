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
ρ ∈ Env = Var ⇀ Value
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

[[ x ]] ρ = ρ(x)
[[ σ ]] ρ = σ
[[ n ]] ρ = n
[[ true ]] ρ = true
[[ false ]] ρ = false
[[ e as τ overf ]] ρ = cast([[ e ]], τ, overf)
[[ e₁ ⊕ e₂ ]] ρ = ([[ e₁ ]] ρ) [[⊕]] ([[ e₂ ]] ρ)
[[ ⊞ e ]] ρ = [[⊞]] ([[ e ]] ρ)
[[ f(e₁, …, eₙ) ]] ρ = [[ eFun ]] ρFun
  where
    "(x₁: τ₁, …, xₙ: τₙ) -> τᵣ = eFun" = F(f)
    ρFun = [x₁ ↦ [[e₁]] ρ, …, xₙ ↦ [[eₙ]] ρ]   -- this is defining a function/map
[[ match e { (pattern eGuard eOut) :: cases } ]] ρ =
  let v = [[ e ]] ρ in
    if v == pattern && [[ eGuard ]] ρ then [[ eOut ]] ρ
                                      else [[ match e { cases } ]] ρ
    
# [[ s* ]] -- this just runs statements in sequence

[[ s₁ :: sᵣₑₛₜ ]] ρ = [[ sᵣₑₛₜ ]] ([[ s₁ ]] ρ)
[[ ]] ρ = ρ
```

Note that a match with 0 cases automatically fails (this would happen on a recursive call).

Here are the definitions of the helpers:

- `[[⊕]]` executes given binary operation.  It also applies the operation
  w.r.t. the correct modulus if that information is available.  Otherwise, it
  infers the modulus from the left-hand side argument (for some operations, both
  arguments must have the same modulus).
- `[[⊞]]` executes given binary operation.  It executes numeric operations based
  on the range of its input.  If the range cannot fit the result, the evaluation
  gets stuck.
- `cast(v, τ, fail)` converts `v` to type `τ` and fails if the result cannot fit in `τ`.
- `cast(v, τ, wraparound)` converts `v` to type `τ` and wraps out-of-bounds values around.
- `cast(v, τ, saturate)` converts `v` to type `τ` and returns the minimum/the
  maximum value for out-of-bounds values.
  
All `cast` functions are defined on only numeric values and numeric types.

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
  let ρ₀ = init(LocalTypes) in
  let ρFinal = fold [[action]] ([[sᵢₙᵢₜ]] ρₒ) σ* in
  [[ eₐ ]] ρFinal
```

Here,
- the helper `init` takes the types of each variable and initializes them to the
  smallest value in their respective types.
- the helper `[[action]]` has the following meaning:
  - `[[on input x s*]] = λ(ρ,σ). [[s*]] ρ[x ↦ σ]`
  - `[[on input s*]] = λ(ρ,σ). [[s*]] ρ`
- `fold f a [b₁, …, bₙ]` stands for `f(bₙ, … f(b₂, f(b₁, a)))`.
- if any partial function fails, execution gets stuck.
- if the final evaluation does not result in a boolean, execution gets stuck.
