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

    let mut lexer = Lexer::new(input);

    // for _ in 1..10 {
    //     println!("{:?}", lexer.next_token);
    //     lexer.scan();
    // }

    let a = std::time::Instant::now();
    let o = parse(&mut lexer).unwrap();
    let b = std::time::Instant::now();

    println!("{:?}; TIME: {:?}", o, b - a);

    let a = std::time::Instant::now();
    run(&o);
    let b = std::time::Instant::now();
    println!("TIME: {:?}", b - a);
}
