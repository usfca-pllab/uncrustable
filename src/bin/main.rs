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
    // evaluate
    // Uncomment and debug when we merge the two branches
    // if args.evaluate {
    //     let eval_result = eval::eval(&program, &input);
    //     if eval_result.is_err() {
    //         let error = eval_result.unwrap_err();
    //         println!("Error: {error}");
    //         log::warn!("Error with evaluation");
    //     } else {
    //         log::info!("Evaluation successful");
    //     }
    // }

    // print verdict

    // more commands (later):
    // - check for equality
    // - compile to DFA
}
