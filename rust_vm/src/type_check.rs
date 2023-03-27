//! 类型检查器

use std::collections::HashMap;

use crate::parser::Expr;
pub enum NsType {
    Bool,
    String,
    Number,
    Tuple(Vec<NsType>),
    Function(Vec<NsType>),
    Array(Box<NsType>),
    Object(HashMap<String, NsType>),
    Generic,
}

pub fn parse_ns_type(expr: &Expr) -> NsType {
    match expr {
        Expr::字面量 { value, .. } => {
            if value == "true" || value == "false" {
                NsType::Bool
            } else if value.contains('\"') {
                NsType::String
            } else {
                NsType::Number
            }
        }
        Expr::元组 { elements } => {
            let mut _type = vec![];
            for elem in elements {
                _type.push(parse_ns_type(elem))
            }
            NsType::Tuple(_type)
        }
        Expr::标识符 { value, .. } => todo!(),
        Expr::二元表达式 {
            operator,
            left,
            right,
        } => match operator.value.as_str() {
            "+" | "-" | "*" | "/" => {
                let left_type = parse_ns_type(left);
                let right_type = parse_ns_type(right);
                todo!()
            }
            _ => todo!(),
        },
        Expr::赋值表达式 {
            operator,
            left,
            right,
        } => todo!(),
    }
}
