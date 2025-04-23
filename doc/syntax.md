# Syntax for uncrustable programs

**This document is work-in-progress:**
- the type system will evolve throughout the project with user-defined types.
- the way arithmetic works is experimental and subject to change.

# Concrete syntax

Terminals are `id`, `num`, `symbol` and everything between backticks (e.g. `` `if` ``).

We use the shorthand `LIST(α)` for a comma-separated list of `α`s with a final
optional comma (similar to Rust-style comma-separated lists).  Formally, it is
substituted as

```
LIST(α) = α (`,` α)* `,`?
```

Here is the grammar:

```
program ::= `alphabet:` `{` LIST(symbol) `}` fun_decl* local_decl* block action accept

fun_decl ::= `fn` id `(` LIST(decl)? `)` `->` type `=` expr

local_decl ::= `let` LIST(decl) `;`

decl ::= id `:` type

block ::= `{` stmt+ `}`

action ::= `on` `input` id? block

accept ::= `accept` `if` expr

type ::= `bool`
       | `int` `[` nonneg `]`            // integers mod N
       | `int` `[` num `..` num `]`      // integers with specific range
       | `sym`                           // symbols

stmt ::= id `=` expr `;`
       | `if` expr block (`else` block)?

expr ::= id                              // variables
       | char                            // symbols
       | num                             // numbers
       | `true`                          // booleans
       | `false`                         //
       | expr binop expr (`mod` nonneg)? // binary operations with optional modulus specification
       | unop expr                       // unary operations
       | expr `as` type overflow?        // casting with overflow specification
       | `match` expr `{` case+ `}`      // pattern matching
       | id `(` LIST(expr)? `)`          // function calls

case ::= pattern (`if` expr)? `->` expr

pattern ::= char | num | `true` | `false` | id

binop ::= `+` | `-` | `*` | `/` | `%`
        | `<<` | `>>`
        | `<=` | `>=` | `<` | `>` | `!=` | `==`
        | `&&` | `||`
unop ::= `!` | `-`

overflow ::= `wraparound` | `saturate` | `fail`
```

## Valid symbols and identifiers

- Valid identifiers are everything that matches the regex below, except keywords.
  ```
  id ::= [a-zA-Z][a-zA-Z0-9_]*
  ```

- Valid symbols are any single character other than `:{}()->_'`.  There are two
  versions in the language:

  ```
  // this is a "naked" symbol without surrounding quotes, used in the alphabet
  // declaration.
  symbol ::= [^:{}()->_']
  // this is a quoted symbol, used everywhere else
  char ::= '[^:{}()->_']'
  ```

- Valid number literals are `num ::= -?[0-9]+`.  All numbers must fit in a
  64-bit signed integer.

- `nonneg ::= [0-9]+` describes non-negative integers.

## Comments

Both C++-style comment syntax (line comments and block comments) are valid.

## Operator associativity precedence

- All operators are left-associative.
- There is no short-circuiting (although this is semantics).
- Precedence is TBD.

## Example program

```
// Accept numbers divisible by 3
alphabet: {0, 1}

fn to_trit(digit: symbol) -> int[3] = match digit {
  '0' -> 0,
  '1' -> 1,
}

let remainder: int[3];

remainder = 0;

on input b {
  remainder = 2 * remainder + to_trit(b)
}

accept if remainder == 0
```

# Abstract syntax

See `syntax.rs` for the detailed representation in Rust.

Metanotation:

- `a ∈ A` is used for describing metavariables in syntactic categories.
- `A → B` are total functions from `A` to `B`.
- `A ⇀ B` are total functions from `A` to `B`.
- `a*` are a sequence of `a`s.
- `a?` is an optional `a`.
- `P(A)` is the powerset of `A`.

Here is the abstract syntax without the details:

```
σ ∈ Symbol   -- this is the alphabet
x ∈ Var
f ∈ FnName
n, m ∈ ℤ
⊕ ∈ Binop  -- this also includes the mod N part
⊞ ∈ Unop

p ∈ Program = FunDef × (x → τ) × s* × action × e
F ∈ FunDef = f → fun

action ∈ Action ::= on input x? s*

fun ∈ Function ::= (x : τ)* -> τ = e

τ ∈ Type ::= bool | int[n..n] | sym

s ∈ Stmt ::= x = e
           | if e s* else s*

e ∈ Expr ::= x | σ | n | true | false
           | e ⊕ e
           | ⊞ e
           | e as τ overf
           | match e { case+ }
           | f(e*)

case ∈ Case ::= pattern e -> e

pattern ∈ Pattern ::= σ | n | true | false | x

overf ∈ Overflow ::= wraparound | saturate | fail
```

Some notes:
- All pattern matching cases have a guard.  If the concrete syntax does not have
  a guard for a case, then the guard is `true`.
- All `if` statements have an `else` block (an empty block is inserted
  automatically).
- The concrete syntax for natural number types `int[n]` is normalized to
  `int[0..n]` in the abstract syntax.
- The default casting behavior (when the overflow part is omitted in the
  concrete syntax) is wraparound.
