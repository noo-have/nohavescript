mod analysis;
mod bytecode;
mod error;
mod macros;
mod parser;
mod tokenizer;
mod translate;
mod type_check;
mod vm;

fn main() {}

#[cfg(test)]
mod test_all {
    mod parser_test {
        #[test]
        fn parser() {
            let mut p = crate::parser::Parser::new("(4+3,\"1\",2)");
            p.start();
        }
    }
    mod vm_test {
        use crate::vm::{ByteCode, NsValue, VM};
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
            println!("vm花了{:?}", time::Instant::now() - p);
            let p = time::Instant::now();
            for _ in 0..10000000 {}
            println!("rust花了{:?}", time::Instant::now() - p);
        }
    }
}
