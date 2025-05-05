// use clap derive for argument parsing
//
// read a file name as the only argument
use clap::Parser;
use log::{error, info, warn};
use uncrustable::eval;
use uncrustable::parse::parse;
use uncrustable::typecheck;
#[derive(Parser, Debug)]

struct Args {
    /// The input program to read
    input: String,

    // The typecheck flag, can be used as --typecheck or --t
    #[arg(short, long)]
    typecheck: bool,

    // The evaluate flag, can be used as --evaluate or --e
    #[arg(short, long)]
    evaluate: bool,
}

fn main() {
    env_logger::init();
    let args = Args::parse();
    let input = std::fs::read_to_string(&args.input).expect("Could not read file");
    let program = parse(&input).unwrap_or_else(|err| panic!("Syntax error: {err}"));
    // println!("Parsed program {:?}", program);

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

    print!("here");

    // evaluate
    // Uncomment and debug when we merge the two branches
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

    // print verdict

    // more commands (later):
    // - check for equality
    // - compile to DFA
}
