use std::io;

mod parser;
mod types;

fn main() {
    println!("welcome to miniml repl");

    let stdin = io::stdin();
    loop {
        let mut input = String::new();
        print!("> ");
        stdin.read_line(&mut input).unwrap();

        let trimmed = input.trim();
        if trimmed.is_empty() {
            continue;
        }
        if trimmed == "exit" || trimmed == "quit" {
            break;
        }

        match parser::parse(&input) {
            Ok(expr) => {
                match types::type_of(&expr, &types::TypeEnv::new()) {
                    Ok(ty) => println!("type: {}", ty),
                    Err(e) => println!("type error: {}", e),
                }

                match types::eval(&expr, &mut types::Env::new()) {
                    Ok(val) => println!("value: {:?}", val),
                    Err(e) => println!("evaluation error: {}", e),
                }
            }
            Err(e) => println!("failed to parse error: {}", e),
        }
    }
}
