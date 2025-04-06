// use clap derive for argument parsing
//
// read a file name as the only argument
use clap::{Arg, Parser};
use log::{error, info, warn, LevelFilter};
use std::io::Write;
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

    println!("Parsed program {program:#?}");

    // type check
    if args.typecheck {
        match typecheck::typecheck_program(&program) {
            Ok(_) => {
                println!("Typecheck successful");
                info!("Typecheck successful");
            }
            Err(error) => {
                println!("TypeCheck Error: {error}");
                warn!("Error with typecheck: {}", error);
            }
        }
    }

    // evaluate
    // Uncomment and debug when we merge the two branches
    // if args.evaluate {
    //     match eval::eval(&program, &input) {
    //         Ok(_) => {
    //             println!("Evaluation successful");
    //             info!("Evaluation successful");
    //         }
    //         Err(error) => {
    //             println!("Evaluation Error: {error}");
    //             warn!("Error with Evaluation: {}", error);
    //         }
    //     }
    // }

    // print verdict

    // more commands (later):
    // - check for equality
    // - compile to DFA
}
