# The type system

The types are defined in `syntax.md`.  The typing environment is:

```
Γ ∈ TypeEnv = Var → Type
```

Below are the typing judgments and rules:

## Typing of expressions

Judgment form: `Γ ⊢ e : τ`.

```
Γ(x) = t
---------- [T-Var]
Γ ⊢ x : τ

lower ≤ n < upper
------------------------- [T-Num]
Γ ⊢ n : int[lower..upper]

------------ [T-Bool]
Γ ⊢ b : bool

----------- [T-Sym]
Γ ⊢ σ : sym


Γ ⊢ e₁ : int[lower..upper]
Γ ⊢ e₂ : int[lower..upper]
τ is the output type of ⊕ (numeric, bool, etc.)
---------------------------------------------- [T-Binop]
Γ ⊢ e₁ ⊕ e₂ : τ

Γ ⊢ e : int[lower..upper]
τ is the output type of ⊞ (numeric, bool, etc.)
---------------------------------------------- [T-Unop]
Γ ⊢ ⊞ e : τ

Γ ⊢ e₁ : τ₁
...
Γ ⊢ eₙ : τₙ
F(f) = (x₁: τ₁, ..., xₙ: τₙ) -> τ
---------------------------------------------- [T-Call]
Γ ⊢ f(e₁, ..., eₙ) : τ

Γ ⊢ e : int[lower'..upper']
---------------------------------------------------- [T-Cast]
Γ ⊢ e as int[lower..upper] overf : int[lower..upper]

Γ ⊢ e : τScrutinee
Γ, x₁ : τScrutinee ⊢ eGuard₁ ⊢ bool   if pattern₁ = x₁
Γ ⊢ eGuard₁ ⊢ bool                    otherwise
Γ, x₁ : τScrutinee ⊢ eResult₁ ⊢ τ     if pattern₁ = x₁
Γ ⊢ eResult₁ ⊢ τ                      otherwise
...  # repeat for other patterns in the match
------------------------------------------------------- [T-Match]
Γ ⊢ match e { (pattern₁ eGuard₁ -> eResult₁) ... } : τ

```

**Important note on the bounds for integers:** Integer constants do not have
bounds.  The type system needs to infer the bounds somehow.  In the
implementation, you can have a type for unknown bounds for constants, and coerce
them immediately when they are used in an assignment/function call/binary
operation.

### Typing of statements and blocks

Judgment forms:
- `Γ ⊢ s ok`
- `Γ ⊢ s* ok`

#### Statements

```
Γ ⊢ x : τ
Γ ⊢ e : τ
------------ [T-Assign]
Γ ⊢ x = e ok
```

#### Blocks

```
Γ ⊢ s₁ ok
...
Γ ⊢ sₙ ok
----------------
Γ ⊢ s₁ ... sₙ ok
```

### Typing of functions

Judgment form: `⊢ (x : τ)* -> τ = e ok`, same as `⊢ fun ok`.

```
(x₁ : τ₁, …, xₙ : τₙ) ⊢ e : τ
----------------------------------- [T-Fun]
⊢ (x₁ : τ₁, …, xₙ : τₙ) -> τ = e ok
```

### Typing of programs

Judgment form: `⊢ F × Γ × s* × action × e ok`

```
∀ f → fun ∈ F. (⊢ fun ok)
Γ ⊢ s* ok
Γ ⊢ action : ok
Γ ⊢ e : bool
--------------------------------------- [T-Prog]
⊢ F × Γ × s* × action × e ok
```

Note: `Γ` for the main program is coming from the declarations in the program.

### Typing of actions

```
Γ ⊢ s* ok
--------------------------------------- [T-ActionNoVar]
Γ ⊢ on input s* ok

Γ, x : sym ⊢ s* ok
--------------------------------------- [T-ActionVar]
Γ ⊢ on input x s* ok
```
