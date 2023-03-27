//! 语义分析器
use std::collections::HashMap;
pub struct Analyzer {}
impl Analyzer {
    pub fn analysis(stat: &Stat) {
        match stat {
            Stat::表达式语句 { expression } => {}
        }
    }
}
use crate::{parser::Stat, type_check::NsType};

pub struct Scope {
    /// 变量表
    variable_table: HashMap<String, (NsType, Vec<(i32, i32)>)>,
    /// 父作用域
    parent_scope: Option<Box<Scope>>,
}
impl Scope {
    fn new() -> Scope {
        Scope {
            variable_table: HashMap::default(),
            parent_scope: None,
        }
    }
}
