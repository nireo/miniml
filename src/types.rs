use crate::parser::BinOp;
use crate::parser::Expr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Int,
    Bool,
    Func(Box<Type>, Box<Type>),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "int"),
            Type::Bool => write!(f, "bool"),
            Type::Func(param_type, return_type) => {
                write!(f, "({} -> {})", param_type, return_type)
            }
        }
    }
}

pub type Env = std::collections::HashMap<String, Value>;

#[derive(Debug, Clone)]
pub enum Value {
    Int(i64),
    Bool(bool),
    Closure {
        params: Vec<String>,
        body: Box<Expr>,
        env: Env, // environment captured when the lambda was defined
    },
}

pub type TypeEnv = std::collections::HashMap<String, Type>;

pub fn type_of(expr: &Expr, env: &TypeEnv) -> Result<Type, String> {
    match expr {
        Expr::Int(_) => Ok(Type::Int),
        Expr::Bool(_) => Ok(Type::Bool),

        Expr::Var(name) => env
            .get(name)
            .cloned()
            .ok_or_else(|| format!("Unbound variable: {name}")),

        Expr::Lambda { params, body } => {
            // TODO: handle multiple parameters
            if params.len() != 1 {
                return Err("Only single-argument lambdas supported in this demo".into());
            }

            let param_name = &params[0];

            let param_ty = Type::Int;

            let mut extended = env.clone();
            extended.insert(param_name.clone(), param_ty.clone());

            let body_ty = type_of(body, &extended)?;
            Ok(Type::Func(Box::new(param_ty), Box::new(body_ty)))
        }

        Expr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            // since this is an expression based languages if statements must always produce a
            // value. therefore the else is not optional.
            let cond_type = type_of(cond, env)?;
            if cond_type != Type::Bool {
                return Err("condition to if not a boolean".into());
            }

            let then_branch = type_of(then_branch, env)?;
            let else_branch = type_of(else_branch, env)?;

            if else_branch != then_branch {
                return Err("if statement branches return different values".into());
            }

            Ok(then_branch)
        }

        Expr::App(func, arg) => {
            let func_ty = type_of(func, env)?;
            let arg_ty = type_of(arg, env)?;

            match func_ty {
                Type::Func(param_ty, ret_ty) => {
                    if *param_ty == arg_ty {
                        Ok(*ret_ty)
                    } else {
                        Err(format!(
                            "type mismatch in application: expected {:?}, found {:?}",
                            param_ty, arg_ty
                        ))
                    }
                }
                other => Err(format!(
                    "trying to apply a non-function: has type {:?}",
                    other
                )),
            }
        }

        Expr::Let { name, value, body } => {
            let value_ty = type_of(value, env)?;
            let mut extended = env.clone();
            extended.insert(name.clone(), value_ty);
            type_of(body, &extended)
        }

        Expr::BinOp {
            op: BinOp::Add,
            left,
            right,
        } => {
            let left_ty = type_of(left, env)?;
            let right_ty = type_of(right, env)?;
            if left_ty == Type::Int && right_ty == Type::Int {
                Ok(Type::Int)
            } else {
                Err(format!(
                    "both sides of + must be int, got {:?} and {:?}",
                    left_ty, right_ty
                ))
            }
        }
    }
}

pub fn eval(expr: &Expr, env: &mut Env) -> Result<Value, String> {
    match expr {
        Expr::Int(n) => Ok(Value::Int(*n)),
        Expr::Bool(b) => Ok(Value::Bool(*b)),

        Expr::If {
            cond,
            then_branch,
            else_branch,
        } => {
            let cond_val = eval(cond, env)?;
            match cond_val {
                Value::Bool(true) => eval(then_branch, env),
                Value::Bool(false) => eval(else_branch, env),
                other => Err(format!("trying to apply a non-closure: {:?}", other)),
            }
        }

        Expr::Var(name) => env
            .get(name)
            .cloned()
            .ok_or_else(|| format!("unbound variable at runtime: {name}")),

        Expr::Lambda { params, body } => Ok(Value::Closure {
            params: params.clone(),
            body: body.clone(),
            env: env.clone(),
        }),

        Expr::App(func_expr, arg_expr) => {
            let func_val = eval(func_expr, env)?;
            let arg_val = eval(arg_expr, env)?;

            match func_val {
                Value::Closure {
                    params,
                    body,
                    mut env,
                } => {
                    if params.len() != 1 {
                        return Err("only single-arg functions supported".into());
                    }
                    let param_name = &params[0];

                    env.insert(param_name.clone(), arg_val);
                    eval(&body, &mut env)
                }
                other => Err(format!("trying to apply a non-closure: {:?}", other)),
            }
        }

        Expr::Let { name, value, body } => {
            let v = eval(value, env)?;
            env.insert(name.clone(), v);
            eval(body, env)
        }

        Expr::BinOp {
            op: BinOp::Add,
            left,
            right,
        } => {
            let lv = eval(left, env)?;
            let rv = eval(right, env)?;

            match (lv, rv) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                (l, r) => Err(format!("Type error at runtime in +: {:?}, {:?}", l, r)),
            }
        }
    }
}
