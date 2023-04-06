//! 语义分析器
use crate::parser::{Expr, Stat, TypeLiteral};
use std::{cell::RefCell, collections::HashMap, rc::Rc};
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum NsType {
    Unit,
    Bool,
    String,
    Number,
    Tuple(Rc<Vec<NsType>>),
    Function(Rc<Vec<NsType>>),
    Array(Rc<NsType>),
    Object(Rc<HashMap<String, NsType>>),
    Generic,
}
impl Clone for NsType {
    /// 廉价的复制,放心使用
    fn clone(&self) -> Self {
        match self {
            NsType::Unit => NsType::Unit,
            NsType::Bool => NsType::Bool,
            NsType::String => NsType::String,
            NsType::Number => NsType::Number,
            NsType::Tuple(v) => NsType::Tuple(Rc::clone(v)),
            NsType::Function(v) => NsType::Function(Rc::clone(v)),
            NsType::Array(v) => NsType::Array(Rc::clone(v)),
            NsType::Object(v) => NsType::Object(Rc::clone(v)),
            NsType::Generic => NsType::Generic,
        }
    }
}
// /// 实现 TypeLiteral 到 NsType的转换
// impl From<TypeLiteral> for NsType {
//     fn from(value: TypeLiteral) -> Self {
//         match value {
//             TypeLiteral::p { name, type_arg } => {
//                 todo!()
//             }
//             TypeLiteral::B(literal_arr) => {
//                 if literal_arr.is_empty() {
//                     NsType::Unit
//                 } else {
//                     NsType::Tuple(Rc::new(
//                         literal_arr
//                             .into_iter()
//                             .map(|literal| literal.into())
//                             .collect(),
//                     ))
//                 }
//             }
//         }
//     }
// }
/// 语法分析器
pub struct Analyzer {
    /// 全局作用域
    global_scope: Rc<RefCell<Scope>>,
    /// 当前作用域
    now_scope: Rc<RefCell<Scope>>,
}
impl Analyzer {
    pub fn new() -> Self {
        let global_scope = Scope::new(None);
        let now_scope = global_scope.clone();
        Self {
            global_scope,
            now_scope,
        }
    }
    /// 在全局作用域下插入一些std变量
    pub fn init(self) {
        let mut scope = self.global_scope.borrow_mut();
        macro_rules! o {
            ($($name:expr,$_type:ident)+) => {

                   $( scope.type_table.insert($name,(NsType::$_type));)*

            };
        }
        o! {
          "number".to_string(), Number
          "unit".to_string(), Unit
          "bool".to_string(), Bool
        };
    }

    /// 以当前作用域创建一个子作用域,然后将它设置为当前作用域
    pub fn create_and_set_now_scope(&mut self) {
        self.now_scope = Scope::new(Some(Rc::clone(&self.now_scope)));
    }
    /// 检查当前作用域是否存在没推断出来的类型
    pub fn check_now_scope_inferType(&self) -> bool {
        self.now_scope
            .borrow()
            .variable_table
            .values()
            .any(|(_type, ..)| !matches!(_type, NsType::Generic))
    }
    /// 清理当前作用域,然后将当前作用域的父作用域设置成当前作用域
    ///
    /// 此函数会首先调用check_now_scope_inferType
    pub fn delete_now_scope(&mut self) -> Option<()> {
        if self.check_now_scope_inferType() {}
        let p = self.now_scope.borrow_mut().parent_scope.clone()?;
        self.now_scope = p;
        Some(())
    }
    // 块表达式内部的stat让analysis_stat处理
    // analysis处理expr时再让analysis_expr处理
    // 就能做到,块语句总是在analysis_stat和analysis_expr中流转
    // 确保类型检查检查的总是非块表达式
    /// 分析表达式
    pub fn analysis_expr(&mut self, expr: &Expr) -> Result<NsType, String> {
        match expr {
            Expr::字面量 { value, .. } => {
                if value == "true" || value == "false" {
                    Ok(NsType::Bool)
                } else if value.contains('\"') {
                    Ok(NsType::String)
                } else {
                    Ok(NsType::Number)
                }
            }
            Expr::元组 { elements, .. } => {
                let mut _type = vec![];
                for elem in elements {
                    _type.push(self.analysis_expr(elem)?)
                }
                Ok(NsType::Tuple(Rc::new(_type)))
            }
            Expr::标识符 { value, .. } => self.now_scope.borrow().get_variable(value),
            Expr::二元表达式 {
                operator,
                left,
                right,
                ..
            } => match operator.as_str() {
                "+" | "-" | "*" | "/" => {
                    let left_type = self.analysis_expr(left)?;
                    let right_type = self.analysis_expr(right)?;
                    todo!()
                }
                _ => unimplemented!(),
            },
            Expr::赋值表达式 {
                operator,
                left,
                right,
                ..
            } => todo!(),
            Expr::块表达式 { body, .. } => {
                self.create_and_set_now_scope();
                for stat in body {
                    self.analysis_stat(stat)?
                }
                // 记录作用域的返回类型
                let ret_type = self.now_scope.borrow().ret_type.clone();

                // 清理
                self.delete_now_scope().unwrap();

                // 该块表达式没有语义错误
                // 并且解析完成,因此self.now_scope_ret_type是准确的,clone即可
                Ok(ret_type)
            }
            Expr::对象表达式 { property } => todo!(),
            Expr::单元 { .. } => Ok(NsType::Unit),
        }
    }
    /// 分析语句
    pub fn analysis_stat(&mut self, stat: &Stat) -> Result<(), String> {
        match stat {
            Stat::表达式语句 { expression } => {
                self.analysis_expr(expression)?;
                Ok(())
            }
            Stat::变量声明语句 {
                is_const,
                pattern,
                type_annotation,
                right,
            } => {
                if *is_const {
                    let right_type = self.analysis_expr(right)?;
                    // 因为是常量声明
                    // 放心unwrap即可
                    // self.now_scope.def_variable(variable_name,type_annotation.unwrap(),)?;
                    todo!()
                } else {
                    if let Some(type_liter) = type_annotation {
                        match type_liter {
                            TypeLiteral::Generic { name, type_arg } => todo!(),
                            TypeLiteral::Tuple(_) => todo!(),
                            TypeLiteral::Infer => todo!(),
                            TypeLiteral::Normal(_) => todo!(),
                        }
                    }
                    todo!()
                }
            }
            Stat::函数声明语句 {
                is_async,
                args,
                ret_type_annotation,
                name,
                block_expr,
            } => {
                self.now_scope = Scope::new(Some(Rc::clone(&self.now_scope)));
                crate::debug!("{:#?}", self.now_scope);
                self.analysis_expr(block_expr)?;
                todo!()
            }
            Stat::返回值语句 { expression } => {
                if self.now_scope.borrow().parent_scope.is_none() {
                    Err("顶层作用域不允许返回值语句".to_string())
                } else {
                    self.now_scope.borrow_mut().ret_type = self.analysis_expr(expression)?;
                    Ok(())
                }
            }
        }
    }
}

#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Scope {
    /// 类型表
    pub type_table: HashMap<String, NsType>,
    /// 变量表
    pub variable_table: HashMap<String, (NsType, Vec<(i32, i32)>)>,
    /// 父作用域
    pub parent_scope: Option<Rc<RefCell<Scope>>>,
    ///
    pub ret_type: NsType,
}
impl Scope {
    /// 从某个作用域里创建一个子作用域
    pub fn new(parent_scope: Option<Rc<RefCell<Scope>>>) -> Rc<RefCell<Scope>> {
        Rc::new(RefCell::new(Scope {
            type_table: HashMap::default(),
            variable_table: HashMap::default(),
            parent_scope,
            ret_type: NsType::Unit,
        }))
    }
    /// 局部变量定义
    pub fn def_variable(
        &mut self,
        variable_name: &str,
        variable_type: &NsType,
        line: i32,
        column: i32,
    ) -> Result<(), String> {
        if self.variable_table.get(variable_name).is_none() {
            self.variable_table.insert(
                variable_name.to_string(),
                (variable_type.clone(), vec![(line, column)]),
            );
            Ok(())
        } else {
            Err("变量 ".to_string() + variable_name + "已经定义")
        }
    }
    /// 从作用域里读取变量
    pub fn get_variable(&self, variable_name: &str) -> Result<NsType, String> {
        if let Some((ns_type, _)) = self.variable_table.get(variable_name) {
            Ok(ns_type.clone())
        } else if let Some(c) = &self.parent_scope {
            c.borrow().get_variable(variable_name)
        } else {
            Err("变量 ".to_string() + variable_name + "未定义")
        }
    }
}
