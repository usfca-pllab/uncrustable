// use clap derive for argument parsing
//
// read a file name as the only argument
use clap::Parser;
use uncrustable::parse::parse;
use uncrustable::typecheck;

#[derive(Parser, Debug)]
struct Args {
    /// The input program to read
    input: String,
}

fn main() {
    let args = Args::parse();
    let input = std::fs::read_to_string(&args.input).expect("Could not read file");
    let program = parse(&input).unwrap_or_else(|err| panic!("Syntax error: {err}"));

    println!("Parsed program {program:#?}");

    // type check
    if args[1] == "typecheck" {
        typecheck::typecheck_program(&program);
    }
    // evaluate
    // print verdict

    // more commands (later):
    // - check for equality
    // - compile to DFA
}
