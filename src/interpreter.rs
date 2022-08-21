use crate::parser::{Expr, Statement};
use std::collections::HashMap;

type Memory = HashMap<String, Value>;

pub fn run(statements: &Vec<Statement>) {
    let mut memory: Memory = HashMap::new();

    for statement in statements.iter() {
        if let Statement::Let(id, value) = statement {
            memory.insert(id.clone(), eval_expr(&memory, value));
        } else if let Statement::Expr(e) = statement {
            eval_expr(&memory, e);
        }
    }
}

#[derive(Clone, Debug)]
enum Value {
    String(String),
    Number(f64),
    Array(Vec<Value>),
    None,
}

fn run_function(memory: &Memory, name: &String, args: &Vec<Expr>) -> Value {
    if name == "print" {
        let mut vals = Vec::new();
        for a in args {
            vals.push(eval_expr(memory, a));
        }
        print(&vals);
        return Value::None;
    } else {
        panic!("Function doesnt exists")
    }
}

fn eval_expr(memory: &Memory, expr: &Expr) -> Value {
    match expr {
        Expr::Number(n) => Value::Number(*n),
        Expr::String(s) => Value::String(s.clone()),
        Expr::Ident(i) => memory.get(i).expect("variable doesnt exist").clone(),
        Expr::FunctionCall(name, args) => run_function(&memory, name, args),
        Expr::Add(a, b) => {
            let a = eval_expr(memory, a);
            let b = eval_expr(memory, b);
            add(&a, &b)
        }
        _ => todo!(),
    }
}

fn print(args: &Vec<Value>) {
    let mut to_print = String::new();

    for arg in args {
        match arg {
            Value::None => to_print += "None",
            Value::String(s) => to_print += s,
            Value::Number(n) => to_print += &n.to_string(),
            Value::Array(v) => print(v),
        }
    }

    println!("{}", to_print);
}

fn add(a: &Value, b: &Value) -> Value {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Value::Number(a + b),
        (Value::String(a), Value::String(b)) => Value::String(a.clone() + b),
        _ => panic!("Cannot Add"),
    }
}
