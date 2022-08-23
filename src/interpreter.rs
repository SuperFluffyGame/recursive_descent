use crate::parser::Expr;
use std::collections::HashMap;
type Memory = HashMap<String, Value>;

#[derive(Debug)]
pub enum InterpreterError {
    NotAFunction(String),
    VariableDoesntExist(String),
    InvalidLeftHandSide(Expr),
    FunctionArgNotIdentifier,
}

pub fn run(exprs: Vec<Expr>) -> Result<(), InterpreterError> {
    let mut memory: Memory = HashMap::new();

    memory.insert(
        "print".to_string(),
        Value::CoreFunction(Some("print".to_string()), Box::new(&print)),
    );

    eval_block(&mut memory, Expr::Block(exprs))?;

    Ok(())
}

#[derive(Clone)]
enum Value {
    String(String),
    Number(f64),
    Array(Vec<Value>),
    None,
    Function(Option<String>, Vec<FunctionArg>, Expr),
    CoreFunction(Option<String>, Box<&'static dyn Fn(&Vec<Value>) -> Value>),
}
#[derive(Clone)]
struct FunctionArg {
    name: String,
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
            Value::Function(name, args, _block) => {
                o += "fn ";
                if let Some(n) = name {
                    o += n;
                }
                o += " (";

                for (i, arg) in args.iter().enumerate() {
                    o += &arg.name;
                    if i == args.len() - 1 {
                        break;
                    }
                    o += ", ";
                }
                o += ") { [CODE] }"
            }
            Value::CoreFunction(name, _f) => {
                o += "core fn ";
                if let Some(n) = name {
                    o += n;
                }

                o += "() { [CODE] }"
            }
        }
        write!(f, "{}", o)
    }
}

fn run_function(
    memory: &mut Memory,
    value_to_call: Value,
    args: &Vec<Value>,
) -> Result<Value, InterpreterError> {
    if let Value::CoreFunction(_name, f) = value_to_call {
        Ok(f(args))
    } else if let Value::Function(_name, func_args, e) = value_to_call {
        let mut memory_with_args = memory.clone();
        for (i, func_arg) in func_args.into_iter().enumerate() {
            let value = args.get(i);

            if let Some(v) = value {
                memory_with_args.insert(func_arg.name, v.clone());
            } else {
                memory_with_args.insert(func_arg.name, Value::None);
            }
        }

        eval_block(&mut memory_with_args, e)
    } else {
        Err(InterpreterError::NotAFunction(format!("{}", value_to_call)))
    }
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
        Expr::FunctionCall(to_call, args_expr) => {
            let value_to_call = eval_expr(memory, to_call)?;

            let mut args = Vec::new();

            for arg_expr in args_expr {
                let arg = eval_expr(memory, arg_expr)?;
                args.push(arg);
            }

            run_function(memory, value_to_call, &args)
        }
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
        Expr::Mul(a, b) => {
            let a = eval_expr(memory, a)?;
            let b = eval_expr(memory, b)?;
            mul(&a, &b)
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
        Expr::FunctionDeclaration(name, arg_exprs, block) => {
            let mut args = Vec::new();
            for arg in arg_exprs {
                if let Expr::Ident(i) = arg {
                    args.push(FunctionArg { name: i.clone() })
                } else {
                    return Err(InterpreterError::FunctionArgNotIdentifier);
                }
            }

            Ok(Value::Function(name.clone(), args, *block.clone()))
        }
        Expr::Index(a, b) => {
            let eval_a = eval_expr(memory, a)?;
            let eval_b = eval_expr(memory, b)?;
            index(&eval_a, &eval_b)
        }
        Expr::Block(_) => eval_block(memory, expr.clone()),
        _ => {
            panic!("NOT YET IMPLEMENTED: {:?}", expr);
        }
    }
}

fn print(values: &Vec<Value>) -> Value {
    let mut to_print = String::new();
    for (i, val) in values.iter().enumerate() {
        to_print += &format!("{}", val);
        if i < values.len() {
            to_print += " ";
        }
    }
    println!("{}", to_print);
    Value::None
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
        _ => panic!("Cannot Sub"),
    }
}
fn exp(a: &Value, b: &Value) -> Result<Value, InterpreterError> {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a.powf(*b))),
        _ => panic!("Cannot Exp"),
    }
}
fn mul(a: &Value, b: &Value) -> Result<Value, InterpreterError> {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(a * b)),
        _ => panic!("Cannot Mul"),
    }
}

fn index(a: &Value, b: &Value) -> Result<Value, InterpreterError> {
    match (a, b) {
        (Value::Array(v), Value::Number(n)) => {
            let possible = v.get(*n as usize);
            match possible {
                Some(v) => return Ok(v.clone()),
                None => return Ok(Value::None),
            }
        }
        _ => panic!("Cannot Index"),
    }
}

fn eval_block(memory: &mut Memory, e: Expr) -> Result<Value, InterpreterError> {
    if let Expr::Block(v) = e {
        for expr in v {
            if let Expr::Return(e) = expr {
                return eval_expr(memory, &e);
            } else if let Expr::FunctionDeclaration(ref name, ..) = expr {
                if let Some(n) = name {
                    let f = eval_expr(memory, &expr)?;
                    memory.insert(n.clone(), f);
                }
            } else {
                eval_expr(memory, &expr)?;
            }
        }
    }

    Ok(Value::None)
}

// fn merge_map<'a, K: std::hash::Hash + Eq, V>(
//     map1: &'a HashMap<K, V>,
//     map2: &'a HashMap<K, V>,
// ) -> HashMap<&'a K, &'a V> {
//     map1.iter().chain(map2.iter()).collect()
// }
