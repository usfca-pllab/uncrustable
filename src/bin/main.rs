// use clap derive for argument parsing
//
// read a file name as the only argument
use clap::Parser;
use log::{error, info, warn};
use uncrustable::enumerate;
use uncrustable::eval;
use uncrustable::parse::parse;
use uncrustable::typecheck;

#[derive(Parser, Debug)]
struct Args {
    /// The input program to read
    input: String,

    // The typecheck flag, can be used as --typecheck
    #[arg(short, long)]
    typecheck: bool,

    // The evaluate flag, can be used as --evaluate
    #[arg(short, long)]
    evaluate: bool,

    // The evaluate flag, can be used as --dfa
    #[arg(short, long)]
    dfa: bool,
}

// Runs on command line example: cargo run --bin main -- example-programs/ends-with-zero.un --typecheck --evaluate --dfa

fn main() {
    env_logger::init();
    let args = Args::parse();
    let input = std::fs::read_to_string(&args.input).expect("Could not read file");
    let program = parse(&input).unwrap_or_else(|err| panic!("Syntax error: {err}"));

    // type check
    if args.typecheck {
        match typecheck::typecheck_program(&program) {
            Ok(_) => {
                println!("Typecheck successful");
            }
            Err(error) => {
                println!("TypeCheck Error: {error}");
            }
        }
    }

    // evaluate
    // print verdict
    if args.evaluate {
        match eval::evaluate(&program, &input) {
            Ok(_) => {
                if true {
                    println!("program accepts the input");
                } else {
                    println!("program rejects the input");
                }
            }
            Err(error) => {
                println!("Evaluation Error: {error}");
            }
        }
    }

    // compile to DFA
    if args.dfa {
        match enumerate::enumerate(&program) {
            Ok(dfa) => {
                println!("{}", dfa);
            }
            Err(error) => {
                println!("Enumeration Error: {error}");
            }
        }
    }
}
