mod analyzer;
mod bytecode;
mod error;
mod macros;
mod parser;
mod tokenizer;
mod translate;
mod unionfind;
mod vm;
fn main() {
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
            let arg = reg.captures(&arg).unwrap().get(1).unwrap().as_str();
            match arg {
                "help" => {}
                "run" => {
                    let mut a = analyzer::Analyzer::new();
                    // let vm = vm::VM::new();
                    // parser.set_gen_bytecode(true);
                    let now = std::time::Instant::now();
                    parser.start();
                    parser
                        .ast
                        .iter()
                        .try_for_each(|stat| {
                            if let Ok(stat) = stat {
                                a.analysis_stat(stat)
                            } else {
                                todo!()
                            }
                        })
                        .unwrap();
                    println!("解析用时{:?}", std::time::Instant::now() - now);
                    // vm::run(bytecode)
                }
                "translate" => {
                    // parser.set_translate(true);
                    parser.start();
                }
                "bytecode" => {

                    // parser.start().unwrap();
                    // write_file(bytecode,path)
                }
                _ => panic!(),
            }
        }
    }
}
use std::io::Write;

use regex::Regex;

use crate::parser::Parser;

fn repl() -> Result<(), String> {
    let mut analyzer = analyzer::Analyzer::new();
    let mut parser = Parser::new("");
    loop {
        let line = read_line()?;
        let line = line.trim();
        if line.is_empty() {
            continue;
        }
        match respond(line, &mut parser, &mut analyzer) {
            Ok(quit) => {
                if quit {
                    break;
                }
            }
            Err(err) => {
                writeln!(std::io::stdout(), "{err:?}").map_err(|e| e.to_string())?;
                std::io::stdout().flush().map_err(|e| e.to_string())?;
            }
        }
    }
    Ok(())
}
fn respond<'a, 'b, 'c>(
    line: &'a str,
    parser: &'b mut Parser,
    analyzer: &'c mut analyzer::Analyzer<'_>,
) -> Result<bool, error::ParseError> {
    parser.ast.clear();
    parser.set_source_code(line);
    let now = std::time::Instant::now();
    parser.start();
    // let mut c = vec![Ok(Stat::表达式语句 {
    //     expression: BoxExpr(parser::Expr::单元 {}, (0, 0)),
    // })];
    // std::mem::swap(&mut c, &mut parser.ast);
    // let y = c.into_iter().try_for_each(|stat| {
    //     analyzer.analysis_stat({
    //         if let Ok(stat) = stat {
    //             // stat
    //             analyzer.analysis_stat(&stat)?;
    //             fn x(a: &Stat) {}
    //             // x(stat);
    //             todo!()
    //         } else {
    //             todo!()
    //         }
    //     })
    // })?;
    println!("解析用时{:?}", std::time::Instant::now() - now);
    Ok(false)
}

fn read_line() -> Result<String, String> {
    write!(std::io::stdout(), "$>").map_err(|e| e.to_string())?;
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
        #[test]
        fn parser_元组() {
            let mut p = crate::parser::Parser::new("(42+3,\"1\",2)");
            p.start();
        }
        #[test]
        fn parse_二元表达式() {
            let mut p = crate::parser::Parser::new("1+2");
            p.start();
        }
        #[test]
        fn parse_块语句() {
            let mut p = crate::parser::Parser::new("{}{1+2;23}");
            p.start();
        }
        #[test]
        fn parse_单元() {
            let mut p = crate::parser::Parser::new("(((2,)))+()  ");
            p.start();
        }
        #[test]
        fn parse_let_stat() {
            let mut p = crate::parser::Parser::new("let as::w = 2");
            p.start();
        }
        #[test]
        fn parse_type_literal() {
            let mut p = crate::parser::Parser::new("let l:P<w,(l,p),s -> d -> ()> = 2");
            p.start();
        }
        #[test]
        fn parse_type_def() {
            let mut p = crate::parser::Parser::new("type k = {d:i32}");
            p.start();
        }
        #[test]
        fn parse_panic_mode() {
            let mut p = crate::parser::Parser::new("type c= let d ;let c = type d = () let p = 0");
            p.start();
            println!("{:#?}", p.ast);
        }
        #[test]
        fn parse_fn_expr() {
            let mut p = crate::parser::Parser::new(
                "\\d => 1 \\() => 2 \\(d) => 3 \\(d,f) => 4 \\(d:P) => 0",
            );
            p.start();
        }
        #[test]
        fn parse_call_expr() {
            let mut p = crate::parser::Parser::new(r"dss(1,4,(1,2),\e => {let x = 23;})");
            p.start();
        }
        #[test]
        fn parse_obj_expr() {
            let mut p = crate::parser::Parser::new(r"{a:23,d} {let p = 2;}");
            p.start();
        }
        #[test]
        fn parse_all_test() {
            let mut p = crate::parser::Parser::new(
                r"
            let a =23
            let {x:a} = ()
            let c = \(s,d) => {a:1,b:2};",
            );
            p.start();
            crate::debug!("{:#?}", p.ast);
        }
    }
    mod analyzer_test {
        use crate::{analyzer::Analyzer, error, parser::Parser};

        #[should_panic]
        #[test]
        // 未定义类型a
        fn analyzer_type1() {
            let mut analyzer = Analyzer::new();
            let mut parser = Parser::new("type d<T> = a");
            parser.start();
            parser.ast.iter().for_each(|stat| {
                if let Ok(stat) = stat {
                    analyzer.analysis_stat(stat).unwrap();
                }
            })
        }
        #[should_panic]
        #[test]
        // 参数数量太多
        fn analyzer_type2() {
            let mut analyzer = Analyzer::new();
            let mut parser = Parser::new("type a<T> = (T,num) let p:a<num,num> =2");
            parser.start();
            parser.ast.iter().for_each(|stat| {
                if let Ok(stat) = stat {
                    analyzer.analysis_stat(stat).unwrap();
                }
            })
        }
        #[should_panic]
        #[test]
        // 不给参数的泛型
        fn analyzer_type3() {
            let mut analyzer = Analyzer::new();
            let mut parser = Parser::new("type a<'T> = () let p:a =2");
            parser.start();
            parser.ast.iter().for_each(|stat| {
                if let Ok(stat) = stat {
                    analyzer.analysis_stat(stat).unwrap();
                }
            })
        }
        #[test]
        fn analyzer_type4() -> Result<(), error::ParseError> {
            // let mut pa = Parser::new("s(1)(3)");
            // pa.start();
            // println!("{:#?}", pa.ast);
            let mut analyzer = Analyzer::new();
            let mut parser = Parser::new(
                r#"
                type A<'a,'b> = 'a -> 'b;
                type B<'a>  = ('a,Num)
                let p:Num -> _ -> _ = \(a,b:'T) =>{
                    let c:'T = "aw";
                    ()
                };
                let a:A<_,_> = \c => \o => 2;
                let b:B<_> = ((),1)
                // error
                let ccc:Bool = a(true);
                let c = \c => 1
                let _a = c(2)
                let _a = c(())
                let cx = \(a,b) => ()
                // error
                let _a = cx(1)(())()
                "#,
            );
            let p = match 23 {
                (23 | 12) => 1,
                12 => 3,
                _ => 1,
            };
            parser.start();
            // println!("{:?}", parser.ast);
            parser
                .ast
                .into_iter()
                .collect::<Result<Vec<_>, _>>()?
                .iter()
                .try_for_each(|stat| analyzer.analysis_stat(stat))?;
            Ok(())
        }
        #[test]
        fn analyzer_type5() {
            let mut a = Analyzer::new();
            use crate::analyzer::{BasicType, Type};
            let t0 = Type::TVar(0, 0);
            let t1 = Type::TVar(1, 0);
            let t2 = Type::TVar(2, 0);
            let q0 = Type::QVar(0);
            let q1 = Type::QVar(1);
            let str = Type::Basic(BasicType::Str);
            let unit = Type::Basic(BasicType::Unit);
            let num = Type::Basic(BasicType::Num);
            let bool = Type::Basic(BasicType::Bool);
            let bool_num = Type::Fn(Box::new(bool.clone()), Box::new(num.clone()));
            let bool_bool_num = Type::Fn(
                Box::new(bool.clone()),
                Box::new(Type::Fn(Box::new(bool), Box::new(num))),
            );
            a.type_env.unify(&t0, &bool_num).unwrap();
            a.type_env.unify(&t1, &bool_bool_num).unwrap();
            a.type_env.unify(&t2, &bool_bool_num).unwrap();
            a.type_env.unify(&t0, &t2).unwrap();
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
                // ByteCode::Go(2),
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
