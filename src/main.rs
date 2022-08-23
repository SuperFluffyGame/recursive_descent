use std::io::prelude::*;
use std::path::PathBuf;
use std::process;
use clap::Parser;

use recursive_descent::interpreter::run;
use recursive_descent::lexer::Lexer;
use recursive_descent::parser::program as parse;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    #[clap(index = 1, value_parser)]
    file: PathBuf,

    /// Stack Size in MB.
    #[clap(short, long, value_parser, default_value_t = 1.0)]
    stack_size: f64,
}

fn main() {
    let args = Args::parse();

    let mut file = std::fs::File::open(
        args.file,
    )
    .unwrap();

    let mut input: String = String::new();
    file.read_to_string(&mut input).unwrap();

    // let builder = std::thread::Builder::new()
    //     .name("parser".into())
    //     .stack_size((1024.0 * 1024.0 * args.stack_size) as usize);

    // let handle = builder
    //     .spawn(move || {
    let a = std::time::Instant::now();
    let mut lexer = Lexer::new(input);

    let tree;
    let result = parse(&mut lexer);
    match result {
        Ok(_tree) => tree = _tree,
        Err(e) => {
            println!("PARSER ERROR: {}", e);
            process::exit(-1)
        }
    }

    let b = std::time::Instant::now();

    println!("TREE: {:?}", tree);
    println!("PARSE: {:?}", b - a);

    let a = std::time::Instant::now();
    run(tree).unwrap();
    let b = std::time::Instant::now();
    println!("INTERPRET: {:?}", b - a);
    // })
    // .unwrap();

    // handle.join().unwrap();
}
