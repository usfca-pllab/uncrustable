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



