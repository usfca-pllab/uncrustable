// use clap derive for argument parsing
//
// read a file name as the only argument
use clap::{arg, Command, Parser};
use uncrustable::eval;
use uncrustable::parse::parse;
use uncrustable::typecheck;

#[derive(Parser, Debug)]
#[cargo]
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
    let matches = Command::new("DFA")
        .arg(arg!(--typecheck     "Typecheck the program you are inputting"))
        .arg(arg!(--evaluate      "Evaluate the program you are inputting"))
        .get_matches();
    
    
    if matches.get_one::<String>("typecheck") != None {
        let type_check_result = typecheck::typecheck_program(&program);
        if type_check_result.is_err() {
            let error = type_check_result.unwrap_err();
            println!("Error: {error}");
        }
    }

    // evaluate
    // print verdict

    // more commands (later):
    // - check for equality
    // - compile to DFA
}
