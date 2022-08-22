use std::io::prelude::*;

use recursive_descent::interpreter::run;
use recursive_descent::lexer::Lexer;
use recursive_descent::parser::program as parse;

fn main() {
    let mut file = std::fs::File::open(
        r"C:\Users\super\OneDrive\Documents\coding\RUST\recursive_descent\test.txt",
    )
    .unwrap();
    let mut input: String = String::new();
    file.read_to_string(&mut input).unwrap();

    let a = std::time::Instant::now();
    let mut lexer = Lexer::new(input);
    let o = parse(&mut lexer).unwrap();
    let b = std::time::Instant::now();

    println!("TREE: {:?}", o);
    println!("PARSE: {:?}", b - a);

    let a = std::time::Instant::now();
    run(o).unwrap();
    let b = std::time::Instant::now();
    println!("INTERPRET: {:?}", b - a);
}
