# Uncrustables User Guide

## uncrustable Syntax

### Supported Operations

#### Binary Operations

#### Unary Operations

#### Matching

#### Function calls

### Example 1: divisble by 3

```
alphabet: { '0', '1' }
fn char_to_bit(c: sym) -> int[0..3] = match c {
    '0' -> 0
    '1' -> 1
}
let rem: int[0..3];
on input bit {
    rem = (2 as int[0..3] * rem as int[0..3]) + (char_to_bit(bit) as int[0..3]);
}
accept if rem == 0 as int[0..3]
```
A unique feature of the uncrustable language is including casting. All numbers must be cast to be within a specific range, which means the language behaves differently than expected. All numeric operations (including matching on numbers, binary operations and returning numbers from functions) cast, even if no range was explicitly given. If no range was provided, ranges are default to be ```[num...num + 1]```. All ranges are exclusive.

Ranges can be written like the following: ```int[3]``` (which will default to ```int[0..3]```) or ```int[0..3]```.

If a casting type (```saturate```, ```wraparound```, or ```fail```) is not provided the default type is ```wraparound```. The casting types are as follows:

```saturate```: If the resulting number to be casted is below the given range, return the lowest value in the range. If the resulting number to be casted is above the given range, return the highest value in the range. If the resulting number to be casted is within the given range, return that value.

```wraparound```: Perform the following arithmetic operation on all values when casting regardless of being within the range
```cast(n, int[lower..upper], wraparound) = (n - lower) % (upper - lower) + lower```

```fail```: If the number is out of the range, fail the program entirely with ```OutOfRange``` error.

All well-written programs will include casting for each numerical value. Be sure to include a casting type if there is a desired program behavior.

### Example 2: ends with zero

```
alphabet: {'0','1'}
let ends_with_zero: bool;
on input x {
    ends_with_zero = x == '0';
}
accept if ends_with_zero == true
```
This is an example of a valid program that checks if the current bit input is ```0```. All programs only read one input character at a time. So, if you have ```10``` as your input, it will process one bit at a time and still produce ```true```.

All variables in an uncrustable program have scope within where it was initialized. The ```input``` bit is not accesible outside of the ```on``` call, and cannot be initialized outside of the call. The ```input``` variable should not shadow any other variable in the program.

When initializing a variable, be sure to include its type (```bool```, ```int[range]```, ```char```).

### Example 3: matching

```
alphabet: {'a'}
let x: int[3];
on input y {
        x = 3 + 4 as int[3] saturate;
        if x < 3 {
            y = 'a';
        } else {
            x = match y {
                    'a' -> 1
                    x if true -> 2
            };
        }
}
accept if x == 1
```
In the above program


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

q0 --0--> q1 : translates to when '0' is run on the program, the state changes from q0(("ends_with_zero = false")) to q1((("ends_with_zero = true")))






