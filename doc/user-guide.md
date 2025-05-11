# Uncrustables User Guide

## Command Line Interface

### Components

Typechecker -  checks that the inputted program is type-safe or returns a TypeError
```
--typecheck
```
Evaluator -  runs the start action and evaluates all of the actions within the action 
block of the program. Returns true if the program accepts the input and false if it 
rejects the input.
```
--evaluate
```
Enumerator - visits all possible states, evaluating all possible outcomes for each 
and populating the DFA struct, keeping track of the states, their names, all possible 
transitions between the states, and the accepting states for the DFA.
```
--dfa
```

### Running CLI With Program

Example:
```
cargo run --bin main -- example-programs/ends-with-zero.un --typecheck --evaluate --dfa
```
The above example passes in the ends-with-zero.un program file, typechecks, evaluates,
and pretty-prints the DFA in the terminal.

Not all flags need to be present, for example, if you wanted to just typecheck the 
program you could just enter the --typecheck flag.

## Understanding Output

Output from the previous CLI example:
```
Typecheck successful
program accepts the input
flowchart TD
  q0(("ends_with_zero = false"))
  q1((("ends_with_zero = true")))
  q0 --0--> q1
  q0 --1--> q0
  q1 --0--> q1
  q1 --1--> q0
```
Program typechecks and is sucessful, evaluates and the input is accepted, and prints DFA.

The two states with their names are printed first, to help with understanding what the 
states represent.
Following, the transitions the the states are printed:
&nbsp;q0 --0--> q1 : translates to when '0' is run on the program, the state changes from
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;q0(("ends_with_zero = false")) to q1((("ends_with_zero = true")))






