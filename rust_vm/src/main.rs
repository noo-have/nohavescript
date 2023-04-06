mod analyzer;
mod bytecode;
mod error;
mod macros;
mod parser;
mod tokenizer;
mod translate;
// mod type_check;
mod vm;
fn main() {
    debug!("{}", std::mem::size_of::<NsType>());
    let mut c = std::env::args();
    if c.len() == 1 {
        repl().unwrap();
    } else {
        let path = c.nth(1).unwrap();
        let result = std::fs::read_to_string(path).unwrap();
        let mut parser = Parser::new(&result);
        let reg = Regex::new("-(.*)").unwrap();
        // 第一个参数为""或当前路径
        for arg in c {
            let arg = reg
                .captures(&arg)
                .unwrap()
                .unwrap()
                .get(1)
                .unwrap()
                .as_str();
            debug!("{:?}", arg);
            match arg {
                "help" => {}
                "run" => {
                    // let vm = vm::VM::new();
                    parser.set_gen_bytecode(true);
                    parser.start();
                    // vm::run(bytecode)
                }
                "translate" => {
                    parser.set_translate(true);
                    parser.start();
                }
                "bytecode" => {
                    parser.set_gen_bytecode(true);
                    parser.start();
                    // write_file(bytecode,path)
                }
                _ => panic!(),
            }
        }
    }
}
use std::io::Write;

use fancy_regex::Regex;

use crate::{analyzer::NsType, parser::Parser};

fn repl() -> Result<(), String> {
    let mut parser = Parser::new("");
    loop {
        let line = read_line()?;
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        match respond(line, &mut parser) {
            Ok(quit) => {
                if quit {
                    break;
                }
            }
            Err(err) => {
                write!(std::io::stdout(), "{err}").map_err(|e| e.to_string())?;
                std::io::stdout().flush().map_err(|e| e.to_string())?;
            }
        }
    }
    Ok(())
}

fn respond(line: &str, parser: &mut Parser) -> Result<bool, String> {
    // let args = shlex::split(line).ok_or("error: Invalid quoting")?;
    parser.set_source_code(line);
    parser.start();
    Ok(false)
}

fn read_line() -> Result<String, String> {
    write!(std::io::stdout(), "$ ").map_err(|e| e.to_string())?;
    std::io::stdout().flush().map_err(|e| e.to_string())?;
    let mut buffer = String::new();
    std::io::stdin()
        .read_line(&mut buffer)
        .map_err(|e| e.to_string())?;
    Ok(buffer)
}

#[cfg(test)]
mod test_all {
    mod parser_test {
        use crate::debug;

        #[test]
        fn parser_元组() {
            let mut p = crate::parser::Parser::new("(42+3,\"1\",2)");
            p.start();
        }
        #[test]
        fn parser_二元表达式() {
            let mut p = crate::parser::Parser::new("1+2");
            p.start();
        }
        #[test]
        fn parser_块语句() {
            let mut p = crate::parser::Parser::new("{1+2;23}");
            p.start();
        }
        #[test]
        fn parser_单元() {
            let mut p = crate::parser::Parser::new("0+()+(1*3)");
            p.start();
            debug!("{:#?}", p.ast);
        }
    }
    mod analyzer_test {
        use crate::{analyzer::Analyzer, parser::Parser};

        #[test]
        fn analyzer() -> Result<(), String> {
            let mut analyzer = Analyzer::new();
            let mut parser = Parser::new("1");
            parser.start();
            for stat in parser.ast {
                analyzer.analysis_stat(&stat)?;
            }
            Ok(())
        }
    }
    mod vm_test {
        use crate::{
            debug,
            vm::{ByteCode, NsValue, VM},
        };
        use std::time;
        #[test]
        fn vm() {
            let mut vm = VM::new();
            let bytecode_list = [
                // ByteCode::Key("s".to_string()),
                // ByteCode::Arr,
                // ByteCode::Idx,
                // ByteCode::Obj,
                ByteCode::Cst(NsValue::Number(1.0)),
                ByteCode::GlobalDef("s".to_string()),
                // ByteCode::Cst(NsValue::Number(1.0)),
                // ByteCode::Add,
                // ByteCode::Str("as".to_string()),
                ByteCode::Cst(NsValue::Number(1.0)),
                ByteCode::SetVariant("s".to_string()),
                // ByteCode::Ne,
                // ByteCode::If(6, 11),
                // ByteCode::Def("s"),
                ByteCode::Go(2),
            ];
            let p = time::Instant::now();
            vm.run(&bytecode_list);
            debug!("vm花了{:?}", time::Instant::now() - p);
            let p = time::Instant::now();
            for _ in 0..10000000 {}
            debug!("rust花了{:?}", time::Instant::now() - p);
        }
    }
}
