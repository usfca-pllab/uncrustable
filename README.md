# Uncrustable

Uncrustable is an imperative programming language that can describe/compile to
deterministic finite automata.

## Documentation

See the `docs/` directory for the language specification and other
documentation.  The relevant parts are:

1. `syntax.md` describes the concrete syntax and the abstract syntax.
2. `types.md` describes the type system.
3. `semantics.md` describes how to evaluate an uncrustable program.
4. `translation.md` describes the way the compiler translates an uncrustable
   program to a DFA description.
5. `dfa.md` describes the concrete syntax for describing DFA.

## Project structure

The compiler and the evaluator are structured as a library which is used by the
binaries.  All binaries are in `src/bin`.

Relevant modules are:

- `dfa`: DFA description, parsing, equivalence checking and execution.
- `eval`: Direct evaluator for uncrustable programs.
- `enumerate`: State enumeration for model checking and compiling.
- `codegen`: Conversion to DFA.
- `parse`: Parsing.
- `syntax`: AST definitions.
- `typecheck`: Type checking.

## License

See `COPYRIGHT` and `LICENSE`.
