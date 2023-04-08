//! 语义分析器
use crate::parser::{Expr, Stat, TypeLiteral};
use std::{cell::RefCell, collections::HashMap, rc::Rc};
/// Ns的类型
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum NsType {
    /// 单元类型
    Unit,
    /// 布尔值
    Bool,
    /// 字符串
    String,
    /// 数字
    Number,
    /// 元组
    Tuple(Rc<Vec<NsType>>),
    /// 函数
    Function(Rc<Vec<NsType>>),
    /// 数组
    Array(Rc<NsType>),
    /// 对象
    Object(Rc<HashMap<String, NsType>>),
    /// 泛型和它的泛型参数数量,只是一个简单包装,方便模式匹配
    Generic(Rc<NsType>, u8),
    /// 真正的未知类型,比如泛型定义时的参数,以及 `_`
    ///
    /// 内部包装的值是一个描述自身位置的 u8 正数,方便在解析泛型时替换
    Unknown(u8),
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
            NsType::Generic(v1, v2) => NsType::Generic(Rc::clone(v1), *v2),
            NsType::Unknown(v) => NsType::Unknown(*v),
        }
    }
}
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
        let mut _self = Self {
            global_scope,
            now_scope,
        };
        _self.init();
        _self
    }
    /// 在全局作用域下插入一些变量 or 类型
    pub fn init(&mut self) {
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
            // .any(|(_type, ..)| !matches!(_type, NsType::Generic))
            .any(|_| todo!())
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
                    let result = if let Some(type_liter) = type_annotation {
                        match type_liter {
                            TypeLiteral::Generic { name, type_args } => {
                                self.assign_generic(name, type_args)?
                            }
                            TypeLiteral::Tuple(_) => todo!(),
                            TypeLiteral::Infer => NsType::Unknown(0),
                            // 最简单的情况，直接去作用域里找
                            TypeLiteral::Normal(id) => self.now_scope.borrow().get_ns_type(id)?,
                            TypeLiteral::ObjLiteral(_) => todo!(),
                        }
                    } else {
                        NsType::Unknown(0)
                    };
                    println!("result:::{result:?}");
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
            // type A<T> = (T,number)
            Stat::类型声明语句(left_type_literal, right_type_literal) => {
                // 对于左侧的字面量来说,只有 泛型 和 普通标识符 两种形式
                match left_type_literal {
                    TypeLiteral::Generic { name, type_args } => {
                        // 同理,对于泛型参数字面量来说也只能使用 普通标识符
                        if !type_args
                            .iter()
                            .all(|_type| matches!(_type, TypeLiteral::Normal(_)))
                        {
                            return Err("泛型参数必须是标识符".to_string());
                        } else {
                            // 先为A类型占位
                            // 也许之后递归也会更方便?
                            self.now_scope.borrow_mut().def_ns_type(
                                name,
                                NsType::Generic(
                                    Rc::new(NsType::Unknown(type_args.len() as u8)),
                                    type_args.len() as u8,
                                ),
                            )?;
                            // 泛型参数的占位
                            // 对于 type A<T,U> = (T,U) 来说,将会生成
                            // T -> (Unknown(1))
                            // U -> (Unknown(2))
                            let mut count = 0;
                            for type_arg in type_args.iter() {
                                count += 1;
                                match type_arg {
                                    TypeLiteral::Normal(type_arg_name) => {
                                        self.now_scope
                                            .borrow_mut()
                                            .def_ns_type(type_arg_name, NsType::Unknown(count))?;
                                    }
                                    _ => todo!(),
                                };
                            }
                            // 现在可以解析右侧类型了
                            // 中间变量 p 的使用纯属是为了避免运行时的借用错误
                            let p = self.analysis_type_literal(right_type_literal)?;
                            // A类型正式被定义
                            self.now_scope.borrow_mut().def_ns_type(
                                name,
                                NsType::Generic(Rc::new(p), type_args.len() as u8),
                            )?;
                            // 清理泛型参数的占位
                            for type_arg in type_args.iter() {
                                match type_arg {
                                    TypeLiteral::Normal(type_arg_name) => {
                                        self.now_scope.borrow_mut().delete_ns_type(type_arg_name);
                                    }
                                    _ => todo!(),
                                };
                            }
                        }
                    }
                    // 简单的情况
                    TypeLiteral::Normal(name) => {
                        // 解析右侧,定义即可
                        let p = self.analysis_type_literal(right_type_literal)?;
                        self.now_scope.borrow_mut().def_ns_type(name, p)?;
                    }
                    _ => return Err("错误的语法".to_string()),
                }
                Ok(())
            }
        }
    }
    /// 合并泛型参数,生成一个新的Ns类型
    fn assign_generic(
        &self,
        type_name: &str,
        type_literal_args: &[TypeLiteral],
    ) -> Result<NsType, String> {
        let gen_type = self.now_scope.borrow().get_ns_type(type_name)?;
        if let NsType::Generic(_type /* 被Generic包装的原类型 */, len) = gen_type {
            if type_literal_args.len() != len.into() {
                Err("类型 ".to_string() + type_name + " 的泛型参数数量不对")
            } else {
                // 遍历参数,将 Vec<TypeLiteral> 转换为 Vec<NsType>
                let type_args = type_literal_args
                    .iter()
                    .map(|type_literal| self.analysis_type_literal(type_literal))
                    .collect::<Result<Vec<_>, _>>()?;
                // 因为上文已经判断过参数数量了,因此下文 unsafe 的索引操作总是安全的
                match *_type {
                    // 对应 type A<T> = T
                    // 因此直接找到 type_args 的相应位置替换即可
                    NsType::Unknown(index) => {
                        Ok(unsafe { type_args.get_unchecked((index) as usize).clone() })
                    }
                    // 对应 type A<T,U> = (T,U)
                    NsType::Tuple(ref tuple_type) => {
                        let mut result = vec![];
                        for _type in tuple_type.iter() {
                            // 同上
                            // 替换并记录
                            if let NsType::Unknown(index) = _type {
                                result.push(unsafe {
                                    type_args.get_unchecked((*index - 1) as usize).clone()
                                });
                            } else if let NsType::Generic(..) = _type {
                                // 如果是泛型
                                // 用辅助函数递归解析之
                                result.push(
                                    self.assign_generic_type(_type.clone(), type_literal_args)?,
                                );
                            } else {
                                // 既不是泛型也不是泛型参数
                                // 说明是一个已知类型
                                result.push(_type.clone())
                            }
                        }
                        Ok(NsType::Tuple(Rc::new(result)))
                    }
                    NsType::Function(_) => todo!(),
                    NsType::Array(_) => todo!(),
                    NsType::Object(_) => todo!(),
                    // 对应 type A<T> = ...;type B<T> = A<T> 这种情况
                    NsType::Generic(..) => {
                        Ok(self.assign_generic_type(_type.as_ref().clone(), type_literal_args)?)
                    }
                    // 以下对应 type A<T> = 基本类型
                    NsType::Bool => Ok(NsType::Bool),
                    NsType::Number => Ok(NsType::Number),
                    NsType::String => Ok(NsType::String),
                    NsType::Unit => Ok(NsType::Unit),
                }
            }
        } else {
            Err("类型 ".to_string() + type_name + "不是泛型类型")
        }
    }
    /// # 不要直接使用
    ///
    /// 用于辅助 `assign_generic` 函数
    fn assign_generic_type(
        &self,
        gen_type: NsType,
        type_literal_args: &[TypeLiteral],
    ) -> Result<NsType, String> {
        if let NsType::Generic(_type /* 原类型 */, _) = gen_type {
            let type_args = type_literal_args
                .iter()
                .map(|type_literal| self.analysis_type_literal(type_literal))
                .collect::<Result<Vec<_>, _>>()?;
            match *_type {
                NsType::Unknown(index) => {
                    Ok(unsafe { type_args.get_unchecked((index) as usize).clone() })
                }
                NsType::Tuple(ref tuple_type) => {
                    let mut result = vec![];
                    for _t2 in tuple_type.iter() {
                        if let NsType::Unknown(index) = _t2 {
                            result.push(unsafe {
                                type_args.get_unchecked((*index - 1) as usize).clone()
                            });
                        } else if let NsType::Generic(_, _) = _t2 {
                            result.push(self.assign_generic_type(_t2.clone(), type_literal_args)?);
                        } else {
                            result.push(_t2.clone())
                        }
                    }
                    Ok(NsType::Tuple(Rc::new(result)))
                }
                NsType::Function(_) => todo!(),
                NsType::Array(_) => todo!(),
                NsType::Object(_) => todo!(),
                NsType::Generic(_, _) => {
                    Ok(self.assign_generic_type(_type.as_ref().clone(), type_literal_args)?)
                }
                NsType::Bool => Ok(NsType::Bool),
                NsType::Number => Ok(NsType::Number),
                NsType::String => Ok(NsType::String),
                NsType::Unit => Ok(NsType::Unit),
            }
        } else {
            Err("不是泛型类型".to_string())
        }
    }
    /// 将类型字面量转换为NsType
    fn analysis_type_literal(&self, type_literal: &TypeLiteral) -> Result<NsType, String> {
        match type_literal {
            // T<U>
            TypeLiteral::Generic { name, type_args } => {
                // 一个问题是 type A<T> = ... type B<T> = A<T>
                // 对于 B<T> 的右侧 A<T> 来说,这里的解析会生成一个已经合并过的泛型
                // 即 Tuple(Unknown(1)) 这会导致上文的类型无法正确的定义,因为不被 Generic 包裹的 NsType 不会被推断
                // 因此在这里包装一次
                let result = self.assign_generic(name, type_args)?;
                Ok(NsType::Generic(Rc::new(result), type_args.len() as u8))
            }
            // (T,U)
            // 直接递归推断
            TypeLiteral::Tuple(_types) => Ok(NsType::Tuple(Rc::new(
                _types
                    .iter()
                    .map(|_type| self.analysis_type_literal(_type))
                    .collect::<Result<Vec<_>, _>>()?,
            ))),
            // {a:T,b:U}
            TypeLiteral::ObjLiteral(table) => {
                let mut obj = HashMap::new();
                for (f, v) in table.iter() {
                    obj.insert(f.to_string(), self.analysis_type_literal(v)?);
                }
                Ok(NsType::Object(Rc::new(obj)))
            }
            // T
            // 单个标识符应该是别名
            TypeLiteral::Normal(name) => Ok(self.now_scope.borrow().get_ns_type(name)?),
            // `_`
            TypeLiteral::Infer => Ok(NsType::Unknown(0)),
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
    /// 局部类型定义
    ///
    /// 目前可以重复定义
    pub fn def_ns_type(&mut self, ns_type_name: &str, ns_type: NsType) -> Result<(), String> {
        // if self.type_table.get(ns_type_name).is_none() {
        //     self.type_table.insert(ns_type_name.to_string(), ns_type);
        //     Ok(())
        // } else {
        //     Err("类型 ".to_string() + ns_type_name + "已经定义")
        // }
        self.type_table.insert(ns_type_name.to_string(), ns_type);
        Ok(())
    }
    /// 从作用域里读取类型
    pub fn get_ns_type(&self, ns_type_name: &str) -> Result<NsType, String> {
        if let Some(ns_type) = self.type_table.get(ns_type_name) {
            Ok(ns_type.clone())
        } else if let Some(c) = &self.parent_scope {
            c.borrow().get_ns_type(ns_type_name)
        } else {
            Err("类型 ".to_string() + ns_type_name + "未定义")
        }
    }
    /// 从作用域里删除一个类型
    pub fn delete_ns_type(&mut self, ns_type_name: &str) -> Option<()> {
        self.type_table.remove(ns_type_name)?;
        Some(())
    }
}
