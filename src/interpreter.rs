use crate::parser::Expr;
use std::collections::HashMap;
type Memory = HashMap<String, Value>;

#[derive(Debug)]
pub enum InterpreterError {
    NotAFunction(String),
    VariableDoesntExist(String),
    InvalidLeftHandSide(Expr),
}

pub fn run(exprs: &Vec<Expr>) -> Result<(), InterpreterError> {
    let mut memory: Memory = HashMap::new();

    for expr in exprs.iter() {
        eval_expr(&mut memory, expr)?;
    }

    Ok(())
}

#[derive(Clone, Debug)]
enum Value {
    String(String),
    Number(f64),
    Array(Vec<Value>),
    None,
    Function(Option<String>, Vec<String>, Expr),
}
impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut o = String::new();
        match self {
            Value::None => {
                o += "None";
            }
            Value::String(s) => {
                o += "\"";
                o += s;
                o += "\"";
            }
            Value::Number(n) => {
                o += &n.to_string();
            }
            Value::Array(values) => {
                o += "[";
                for (i, value) in values.iter().enumerate() {
                    o += &format!("{}", value);
                    if i == values.len() - 1 {
                        break;
                    }
                    o += ", ";
                }
                o += "]";
            }
            Value::Function(args, _block) => {
                o += "fn (";
                for (i, arg) in args.iter().enumerate() {
                    o += arg;
                    if i == args.len() - 1 {
                        break;
                    }
                    o += ", ";
                }
                o += ") { [CODE] }"
            }
        }
        write!(f, "{}", o)
    }
}

fn run_function(
    memory: &mut Memory,
    value_to_call: Value,
    args: &Vec<Expr>,
) -> Result<Value, InterpreterError> {
    todo!();

    // if name == "print" {
    //     let mut vals = Vec::new();
    //     for a in args {
    //         vals.push(eval_expr(memory, a)?);
    //     }
    //     print(&vals)?;
    //     return Ok(Value::None);
    // } else {
    //     panic!("Function doesnt exists")
    // }
}

fn eval_expr(memory: &mut Memory, expr: &Expr) -> Result<Value, InterpreterError> {
    match expr {
        Expr::Let(i, v) => {
            let val = eval_expr(memory, v)?;
            memory.insert(i.clone(), val.clone());

            Ok(val)
        }
        Expr::Number(n) => Ok(Value::Number(*n)),
        Expr::String(s) => Ok(Value::String(s.clone())),
        Expr::Ident(i) => {
            let possible_value = memory.get(i);
            if let Some(v) = possible_value {
                return Ok(v.clone());
            } else {
                return Err(InterpreterError::VariableDoesntExist(i.clone()));
            }
        }
        Expr::FunctionCall(name, args) => run_function(memory, eval_expr(memory, expr)?, args),
        Expr::Add(a, b) => {
            let a = eval_expr(memory, a)?;
            let b = eval_expr(memory, b)?;
            add(&a, &b)
        }
        Expr::Sub(a, b) => {
            let a = eval_expr(memory, a)?;
            let b = eval_expr(memory, b)?;
            sub(&a, &b)
        }
        Expr::Exp(a, b) => {
            let a = eval_expr(memory, a)?;
            let b = eval_expr(memory, b)?;
            exp(&a, &b)
        }
        Expr::Array(exprs) => {
            let mut o = Vec::new();
            for e in exprs {
                o.push(eval_expr(memory, e)?);
            }
            Ok(Value::Array(o))
        }
        Expr::Assign(id, expr) => {
            let val = eval_expr(memory, expr)?;
            let id = *id.clone();
            if let Expr::Ident(i) = id {
                memory.insert(i.clone(), val.clone());
            } else {
                return Err(InterpreterError::InvalidLeftHandSide(id.clone()));
            }

            return Ok(val);
        }
        // Expr::FunctionDeclaration(name, args, block) => Value::F
        _ => todo!(),
    }
}

fn print(args: &Vec<Value>) -> Result<Value, InterpreterError> {
    for arg in args {
        println!("{}", arg);
    }

    Ok(Value::None)
}

fn add(a: &Value, b: &Value) -> Result<Value, InterpreterError> {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a + b)),
        (Value::String(a), Value::String(b)) => Ok(Value::String(a.clone() + b)),
        _ => panic!("Cannot Add"),
    }
}
fn sub(a: &Value, b: &Value) -> Result<Value, InterpreterError> {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a - b)),
        _ => panic!("Cannot Add"),
    }
}
fn exp(a: &Value, b: &Value) -> Result<Value, InterpreterError> {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a.powf(*b))),
        _ => panic!("Cannot Add"),
    }
}
