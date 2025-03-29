// use clap derive for argument parsing
//
// read a file name as the only argument
use chrono::Local;
use clap::{Arg, Parser};
use env_logger::Builder;
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
    Builder::new()
        .format(|buf, record| {
            writeln!(
                buf,
                "{} [{}] - {}",
                Local::now().format("%Y-%m-%d %H:%M:%S"),
                record.level(),
                record.args()
            )
        })
        .filter(None, LevelFilter::Info)
        .init();
    let args = Args::parse();
    let input = std::fs::read_to_string(&args.input).expect("Could not read file");
    let program = parse(&input).unwrap_or_else(|err| panic!("Syntax error: {err}"));

    println!("Parsed program {program:#?}");

    // type check
    if args.typecheck {
        let type_check_result = typecheck::typecheck_program(&program);
        if type_check_result.is_err() {
            let error = type_check_result.unwrap_err();
            println!("Error: {error}");
            log::warn!("Error with typecheck");
        } else {
            log::info!("Typecheck successful");
        }
    }

    // evaluate
    // Uncomment and debug when we merge the two branches
    // if args.evaluate {
    //     let eval_result = eval::eval(&program, &input);
    //     if eval_result.is_err() {
    //         let error = eval_result.unwrap_err();
    //         println!("Error: {error}");
    //     }
    // }

    // print verdict

    // more commands (later):
    // - check for equality
    // - compile to DFA
}
