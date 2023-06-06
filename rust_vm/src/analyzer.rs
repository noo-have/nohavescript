#![allow(unused_variables)]
use std::{borrow::Cow, collections::HashMap, mem::take};

use crate::{
    error::{analysis_error, ParseError},
    parser::{BoxExpr, Expr, Pattern, Stat, TypeLiteral},
    unionfind::UnionFind,
};
// impl From<Vec<Type>> for Type {
//     fn from(mut value: Vec<Type>) -> Self {
//         let mut t = value.pop().expect("长度为0的数组无法转换");
//         while let Some(x) = value.pop() {
//             t = Type::Fn(Box::new(x), Box::new(t));
//         }
//         t
//     }
// }
impl std::convert::TryFrom<Vec<Type>> for Type {
    type Error = ParseError;
    fn try_from(mut value: Vec<Type>) -> Result<Self, Self::Error> {
        let mut t = value.pop().ok_or(analysis_error("message", 0, 0))?;
        while let Some(x) = value.pop() {
            t = Type::Fn(Box::new(x), Box::new(t));
        }
        Ok(t)
    }
}
/// 基元类型
#[derive(Clone, PartialEq, Hash, Debug, Eq)]
pub enum BasicType {
    /// 数字
    Num,
    /// 单元
    Unit,
    /// 布尔值
    Bool,
    /// 字符串
    Str,
}
/// NS的类型
#[derive(Clone, Hash, PartialEq, Eq)]
#[cfg_attr(not(debug_assertions), derive(Debug))]
pub enum Type {
    /// 多态
    // Polymorphic(usize, Box<Type>),
    /// 基元类型
    Basic(BasicType),
    /// 类型变量
    TVar(usize, usize),
    /// 元组
    Tuple(Vec<Type>),
    /// 函数
    Fn(Box<Type>, Box<Type>),
    /// 为了表示多态类型的辅助类型
    ///
    /// 例如:`let a = (a,b) => a`
    ///
    /// a的类型是 QVar(0) -> QVar(1) -> QVar(0)
    ///
    /// 这里的QVar只表示位置,方便之后多态类型实例化时替换
    QVar(usize),
    // /// 数组
    // Array(Rc<Type>),
    // /// 对象
    // // Object(Rc<HashMap<&'static str, NsType>>),
    // /// 泛型和它的泛型参数数量,只是一个简单包装,方便模式匹配
    // Generic(Rc<Type>, u8),
    // /// 真正的未知类型,比如泛型定义时的参数,以及 `_`
    // ///
    // /// 内部包装的值是一个描述自身位置的 u8 正数,方便在解析泛型时替换
    // Unknown(u8),
}
impl Default for Type {
    fn default() -> Self {
        Self::Basic(BasicType::Unit)
    }
}
impl Type {
    fn qvar_count(&self) -> usize {
        match self {
            Type::Basic(_) => 0,
            Type::TVar(..) => 0,
            Type::Tuple(ts) => {
                let mut r = 0;
                for t in ts.iter() {
                    let count = t.qvar_count();
                    r = if count > r { count } else { r }
                }
                r
            }
            Type::Fn(t1, t2) => {
                let (count1, count2) = (t1.qvar_count(), t2.qvar_count());
                if count1 > count2 {
                    count1
                } else {
                    count2
                }
            }
            Type::QVar(i) => *i + 1,
        }
    }
    //     fn is_polymorphic(&self) -> bool {
    //         self.qvar_count() != 0
    //     }
}
#[cfg(debug_assertions)]
impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            // Type::Polymorphic(len, t) => {
            //     f.write_str("∀")?;
            //     for t in 0..*len {
            //         f.write_fmt(format_args!(
            //             "{}{}",
            //             ((t % 26) + 97) as u8 as char,
            //             t.div_euclid(26)
            //         ))?;
            //         if t != *len - 1 {
            //             f.write_str(",")?;
            //         }
            //     }
            //     f.write_str(".")?;
            //     t.as_ref().fmt(f)
            // }
            Type::Basic(BasicType::Unit) => f.write_str("()"),
            Type::Basic(BasicType::Bool) => f.write_str("Bool"),
            Type::Basic(BasicType::Str) => f.write_str("Str"),
            Type::Basic(BasicType::Num) => f.write_str("Num"),
            Type::TVar(i, level) => {
                f.write_fmt(format_args!("tv{}/{}", i, level))

                // f.write_fmt(format_args!(
                // "{}{}",
                // ((idx % 26) + 97) as u8 as char,
                // idx.div_euclid(26)
                // ))
            }
            Type::Tuple(ts) => {
                let mut i = 0;
                f.write_str("(")?;
                for t in ts {
                    i += 1;
                    t.fmt(f)?;
                    if i != ts.len() {
                        f.write_str(", ")?;
                    }
                }
                f.write_str(")")?;
                Ok(())
            }
            Type::Fn(t1, t2) => {
                t1.fmt(f)?;
                f.write_str(" -> ")?;
                t2.fmt(f)?;
                Ok(())
            }
            Type::QVar(i) => f.write_fmt(format_args!("a{}", i)),
        }
    }
}
pub struct Analyzer<'a> {
    /// 作用域
    pub scope: Scope,
    /// 类型环境(类型的作用域)
    pub type_env: TypeEnv<'a>,
}
impl<'a> Analyzer<'a> {
    pub fn new() -> Self {
        Self {
            scope: Scope::new(None),
            type_env: TypeEnv::new(None, 1).init(),
        }
    }
    /// 实现与作用域的`create_scope`函数完全相同
    ///
    /// 添加了对`level`的处理
    pub fn create_type_env(&mut self) {
        let level = self.type_env.level;
        self.type_env = TypeEnv::new(Some(Box::new(take(&mut self.type_env))), level + 1);
    }
    /// 实现与作用域的`delete_scope`函数完全相同
    ///
    /// ## 请确保当前环境有父环境
    pub unsafe fn delete_type_env(&mut self) {
        self.type_env = take(self.type_env.parent.as_mut().unwrap_unchecked());
    }
    /// 设置当前作用域为新作用域的父级
    ///
    /// 即,新增一个子作用域
    pub fn create_scope(&mut self) {
        // self.checker.create_env();
        self.scope = Scope::new(Some(Box::new(take(&mut self.scope))));
    }
    /// 设置当前作用域的父级为新作用域
    ///
    /// 即,删除子作用域
    ///
    /// ## 请确保当前作用域有父作用域
    pub unsafe fn delete_scope(&mut self) {
        // self.checker.delete_env();
        self.scope = take(self.scope.parent_scope.as_mut().unwrap_unchecked());
    }

    /// 类型声明语句的右侧类型字面量解析
    ///
    /// type TypeName = **TypeLiteral**
    pub fn parse_typeliteral(&self, type_literal: &TypeLiteral) -> Result<Type, ParseError> {
        match type_literal {
            TypeLiteral::Generic {
                name,
                type_args,
                loc,
            } => todo!(),
            TypeLiteral::Normal(name, (line, column)) => self.type_env.get_type(name).ok_or(
                analysis_error(&format!("类型 {} 未定义", name), *line, *column),
            ),
            TypeLiteral::Tuple(arr, _) => Ok(Type::Tuple(
                arr.iter()
                    .map(|t| self.parse_typeliteral(t))
                    .collect::<Result<Vec<_>, _>>()?,
            )),
            TypeLiteral::Infer((line, column)) => {
                todo!()
            }
            TypeLiteral::ObjLiteral(..) => todo!(),
            TypeLiteral::Fn(arr, _) => Ok(arr
                .iter()
                .map(|tl| self.parse_typeliteral(tl))
                .collect::<Result<Vec<Type>, ParseError>>()?
                .try_into()?),

            TypeLiteral::Variable(name, (line, column)) => {
                /*
                  type x<'a> = 'a
                  let p: Option<'b> = None
                */
                if let Some(_type) = self.type_env.get_type(name) {
                    Ok(_type)
                } else {
                    Err(analysis_error(
                        &format!("类型变量 '{} 未定义", name),
                        *line,
                        *column,
                    ))
                }
            }
        }
    }
    /// 类型声明语句的左侧类型字面量解析
    ///
    /// type **TypeName** = xx
    ///
    /// 类型声明时,对左侧的类型字面量有约束
    ///
    /// 例如,type _ =xx,type (T) = xx,type 'd = xx诸如此类的语法是不合法的
    ///
    /// 只有`TypeLiteral::Generic` 和 `TypeLiteral::Normal`形式是可以的
    fn parse_typeliteral_def(type_literal: &TypeLiteral) -> Result<(&str, Vec<&str>), ParseError> {
        match type_literal {
            TypeLiteral::Generic {
                name, type_args, ..
            } => Ok((
                name,
                type_args
                    .iter()
                    .map(|x| match x {
                        TypeLiteral::Variable(name, _) => Ok(*name),
                        TypeLiteral::Fn(_, (line, column))
                        | TypeLiteral::Generic {
                            name: _,
                            type_args: _,
                            loc: (line, column),
                        }
                        | TypeLiteral::Infer((line, column))
                        | TypeLiteral::Normal(_, (line, column))
                        | TypeLiteral::ObjLiteral(_, (line, column))
                        | TypeLiteral::Tuple(_, (line, column)) => Err(analysis_error(
                            "泛型参数只能使用 `'a` 这种形式",
                            *line,
                            *column,
                        )),
                    })
                    .collect::<Result<Vec<&str>, ParseError>>()?,
            )),
            TypeLiteral::Normal(name, _) => Ok((name, vec![])),
            TypeLiteral::Fn(_, (line, column))
            | TypeLiteral::Variable(_, (line, column))
            | TypeLiteral::Infer((line, column))
            | TypeLiteral::ObjLiteral(_, (line, column))
            | TypeLiteral::Tuple(_, (line, column)) => Err(analysis_error(
                "类型声明语句的左侧,只能使用 `T`或`T<'a>` 这种形式",
                *line,
                *column,
            )),
        }
    }
    /// 函数表达式中,参数的类型标注解析
    ///
    /// \(a:**TypeAnnotation**) => xx
    fn parse_typeliteral_type_annotation_parma(
        &mut self,
        type_literal: &TypeLiteral,
    ) -> Result<Type, ParseError> {
        match type_literal {
            TypeLiteral::Generic {
                name,
                type_args,
                loc,
            } => todo!(),
            TypeLiteral::Normal(name, (line, column)) => {
                if let Some(_type) = self.type_env.get_type(name) {
                    Ok(_type)
                } else {
                    Err(analysis_error(
                        &format!("类型 {} 不存在", name),
                        *line,
                        *column,
                    ))
                }
            }
            TypeLiteral::Variable(name, _) => {
                // 如果参数类型标注的是类型变量,那么总是合法的:
                // 1. 如果类型变量存在,可以;
                // 2. 如果类型变量不存在,允许在类型环境中新增它
                Ok(self.type_env.get_type(name).unwrap_or_else(|| {
                    let p = self.type_env.new_t_var();
                    self.type_env
                        .type_variable
                        .insert(Cow::from(*name), p.clone());
                    p
                }))
            }
            TypeLiteral::Tuple(ts, _) => {
                if ts.is_empty() {
                    Ok(Type::Basic(BasicType::Unit))
                } else {
                    Ok(Type::Tuple(
                        ts.iter()
                            .map(|t| self.parse_typeliteral_type_annotation_parma(t))
                            .collect::<Result<Vec<_>, _>>()?,
                    ))
                }
            }
            TypeLiteral::Infer(_) => {
                // `arg:_`的情况总是视为`arg`
                Ok(self.type_env.new_t_var())
            }
            TypeLiteral::ObjLiteral(_, _) => todo!(),
            TypeLiteral::Fn(_, _) => todo!(),
        }
    }
    /// 变量声明语句中,变量的类型标注解析
    ///
    /// let x:**TypeAnnotation** = xx
    fn parse_typeliteral_type_annotation_variable(
        &mut self,
        type_literal: &TypeLiteral,
    ) -> Result<Type, ParseError> {
        match type_literal {
            TypeLiteral::Generic {
                name,
                type_args,
                loc: (line, column),
            } => {
                if let Some(t) = self.type_env.get_type(name) {
                    let pc = t.qvar_count();
                    if pc == 0 {
                        Err(analysis_error(
                            &format!("类型 {} 不是泛型", name),
                            *line,
                            *column,
                        ))
                    } else if pc != type_args.len() {
                        Err(analysis_error(
                            &format!(
                                "泛型 {} 需要 {} 个泛型参数,但找到 {} 个",
                                name,
                                type_args.len(),
                                pc
                            ),
                            *line,
                            *column,
                        ))
                    } else {
                        let arg_types = type_args
                            .iter()
                            .map(|t| self.parse_typeliteral_type_annotation_variable(t))
                            .collect::<Result<Vec<_>, _>>()?;
                        Ok(TypeEnv::call_type(t, &arg_types))
                    }
                } else {
                    Err(analysis_error(
                        &format!("类型 {} 不存在", name),
                        *line,
                        *column,
                    ))
                }
            }
            TypeLiteral::Normal(name, (line, column))
            | TypeLiteral::Variable(name, (line, column)) => {
                if let Some(t) = self.type_env.get_type(name) {
                    Ok(t)
                } else {
                    Err(analysis_error(
                        &format!("类型 {} 不存在", name),
                        *line,
                        *column,
                    ))
                }
            }
            TypeLiteral::Tuple(t, _) => Ok(Type::Tuple(
                t.iter()
                    .map(|x| self.parse_typeliteral_type_annotation_variable(x))
                    .collect::<Result<_, _>>()?,
            )),
            TypeLiteral::Infer(_) => Ok(self.type_env.new_t_var()),
            TypeLiteral::ObjLiteral(_, _) => todo!(),
            TypeLiteral::Fn(x, c) => {
                let p = x
                    .iter()
                    .map(|t| self.parse_typeliteral_type_annotation_variable(t))
                    .collect::<Result<Vec<_>, _>>()?;
                let mut iter = p.into_iter();
                debug_assert!(iter.len() >= 2);
                // 至少有两个元素
                let (tt, mut ttt) = unsafe {
                    (
                        iter.next().unwrap_unchecked(),
                        iter.next().unwrap_unchecked(),
                    )
                };
                for t in iter {
                    ttt = Type::Fn(Box::new(ttt), Box::new(t));
                }
                Ok(Type::Fn(Box::new(tt), Box::new(ttt)))
            }
        }
    }
    /// 分析表达式
    ///
    /// 此操作会产生大量副作用:类型约束的生成,作用域和类型作用域的变化
    pub fn analysis_expr(
        &mut self,
        BoxExpr(expr, (mut line, mut column)): &'a BoxExpr,
    ) -> Result<Type, ParseError> {
        let r = match expr {
            Expr::字面量 { value } => match *value {
                "true" | "false" => Ok(Type::Basic(BasicType::Bool)),
                _ => {
                    if value.parse::<i32>().is_err() && value.parse::<f32>().is_err() {
                        Ok(Type::Basic(BasicType::Str))
                    } else {
                        Ok(Type::Basic(BasicType::Num))
                    }
                }
            },
            Expr::元组 { elements } => Ok(Type::Tuple(
                elements
                    .iter()
                    .map(|elem| self.analysis_expr(elem))
                    .collect::<Result<Vec<Type>, ParseError>>()?,
            )),
            Expr::单元 {} => Ok(Type::Basic(BasicType::Unit)),
            Expr::标识符 { value: name } => {
                self.type_env
                    .get_type(name)
                    .ok_or(analysis_error("未定义的变量", line, column))
            }
            Expr::二元表达式 {
                operator,
                left,
                right,
            } => todo!(),
            Expr::赋值表达式 {
                operator,
                left,
                right,
            } => todo!(),
            Expr::块表达式 { body } => {
                self.create_scope();
                self.create_type_env();
                body.iter().try_for_each(|stat| self.analysis_stat(stat))?;
                let rt = take(&mut self.scope.return_type);
                unsafe {
                    self.delete_scope();
                    self.delete_type_env();
                }
                Ok(rt)
            }
            Expr::对象表达式 { property } => todo!(),
            Expr::函数表达式 { parma, body } => {
                // 提前创建作用域和类型环境
                self.create_scope();
                self.create_type_env();
                let mut ts = vec![];
                for (pattern, type_literal) in parma.iter() {
                    match pattern {
                        Pattern::Id(id) => {
                            let t = match type_literal {
                                Some(_t) => self.parse_typeliteral_type_annotation_parma(_t),
                                // 不存在相当于 `arg:_`
                                None => self.parse_typeliteral_type_annotation_parma(
                                    &TypeLiteral::Infer((0, 0)),
                                ),
                            }?;
                            self.type_env
                                .type_variable
                                .insert(Cow::Borrowed(*id), t.clone());
                            ts.push(t)
                        }
                        _ => todo!("函数参数的模式匹配暂未实现"),
                    }
                }
                // 参数处理完毕
                let rt = match body.as_ref() {
                    BoxExpr(Expr::块表达式 { ref body }, _) => {
                        body.iter().try_for_each(|stat| self.analysis_stat(stat))?;
                        take(&mut self.scope.return_type)
                    }
                    expr => self.analysis_expr(expr)?,
                };
                ts = ts
                    .into_iter()
                    .map(|t| self.type_env.find_meta(&t))
                    .collect();
                ts.push(rt);
                unsafe {
                    self.delete_scope();
                    self.delete_type_env();
                };
                Ok(ts.try_into()?)
            }
            Expr::函数调用表达式 { callee, args } => {
                let callee_type = self.analysis_expr(callee)?;

                // println!("该调用函数类型 : {:?}", callee_type);
                match callee_type {
                    Type::QVar(..) => todo!(),
                    Type::TVar(..) => todo!(),
                    Type::Fn(ref t1, ref t2) => {
                        let mut t1 = t1;
                        let mut t2 = t2;
                        let mut flag = false;
                        let mut result = Err(analysis_error("message", line, column));
                        // t1:TVar(0,1) t2:TVar(1,1) -> Num
                        // 这种实现方式允许柯里化
                        for arg in args {
                            if flag {
                                result = Err(analysis_error("多余的参数", line, column));
                                break;
                            }
                            let at = self.analysis_expr(arg)?;
                            self.type_env.unify(&at, t1)?;
                            // 没有报错,说明该表达式的返回值是t2
                            // 这里的操作是 &Box<Type> -> Ok Type
                            result = Ok(t2.as_ref().clone());
                            t1 = match t2.as_ref() {
                                Type::Fn(a, b) => {
                                    //
                                    t2 = b;
                                    a
                                }
                                _ => {
                                    // 当t2不是函数类型,说明参数已经满了
                                    // 因此直接设置flag为true,如果还有多余的arg就报错
                                    flag = true;
                                    t1
                                }
                            };
                        }
                        result
                    }
                    _ => Err(analysis_error(
                        &format!("表达式类型为 {:?},但需要 函数类型 ", callee_type),
                        line,
                        column,
                    )),
                }
            }
        }?;
        Ok(self.type_env.gen_move(r))
    }
    /// 当前作用域是不是顶层?
    pub fn is_toplevel(&self) -> bool {
        self.scope.parent_scope.is_none()
    }
    /// 分析语句
    pub fn analysis_stat<'b: 'a>(&mut self, stat: &'b Stat) -> Result<(), ParseError> {
        match stat {
            Stat::表达式语句 { expression: expr } => {
                if self.is_toplevel() {
                    return Err(analysis_error("模块顶层不允许出现裸表达式", 0, 0));
                }
                // 裸表达式的类型必须是 Unit
                // 否则视为返回值
                let t = self.analysis_expr(expr)?;
                // 如果表达式类型是Unit,直接通过
                if self.type_env.find_meta(&t) == Type::Basic(BasicType::Unit) {
                    return Ok(());
                }
                if matches!(&self.scope.return_type, &Type::Basic(BasicType::Unit)) {
                    self.scope.return_type = t;
                } else if matches!(&self.scope.return_type, &Type::TVar(..)) {
                    // 如果作用域的返回值是一个类型变量
                    // 结果又出现了下一个返回值,说明该类型变量必定是Unit类型
                    // 发射一个Unit约束给类型变量
                    self.type_env
                        .unify(&Type::Basic(BasicType::Unit), &self.scope.return_type)?;
                    // self.scope.return_type = t;
                } else {
                    // 还要处理这种`T<'a>`类型
                    // 除此之外,报错即可
                    return Err(analysis_error(
                        &format!(
                            "返回值是 {:?} 类型,但此处的表达式是 {:?} 类型",
                            self.scope.return_type, t
                        ),
                        0,
                        0,
                    ));
                }
                Ok(())
            }
            Stat::变量声明语句 {
                is_const,
                pattern,
                type_annotation,
                right,
            } => {
                let name = self.parse_pattern(pattern);
                let lt = if let Some(type_literal) = type_annotation {
                    self.parse_typeliteral_type_annotation_variable(type_literal)?
                } else {
                    self.parse_typeliteral_type_annotation_variable(&TypeLiteral::Infer((0, 0)))?
                };
                let rt = self.analysis_expr(right)?;
                //
                // let rt = self.type_env.gen(&rt);
                //
                // let rt = BoxTypeEnv::<'_>::inst(&rt, self.type_env.level);
                // println!("左侧类型标注: {:?} --- 右侧表达式的类型是 {:?}", lt, rt);
                //
                // let lt = self.type_env.find_meta(&lt).unwrap();
                // let rt = self.type_env.find_meta(&rt).unwrap();
                // println!(
                // "左侧表达式的代表元是 {:?} --- 右侧表达式的代表元是 {:?}",
                // lt, rt
                // );
                self.type_env.unify(&lt, &rt)?;
                // 如果unify不出问题,现在 lt 和 rt的代表元是一致的
                // 所以随便找一个代表元直接准备存入类型变量表
                let lt = self.type_env.find_meta(&lt);
                // println!(
                //     "右侧表达式类型是 {:?}",
                //     self.type_env.find_meta(&rt).unwrap()
                // );
                self.type_env.type_variable.insert(Cow::Borrowed(name), lt);

                // println!("uf: {:?}", self.type_env.uf);
                // println!("typeEnv: {:?}", self.type_env);
                Ok(())
            }
            Stat::函数声明语句 {
                is_async,
                name,
                args,
                ret_type_annotation,
                block_expr,
            } => todo!(),
            Stat::返回值语句 { expression } => todo!(),
            Stat::类型声明语句(tl1, tl2) => {
                // 解析左侧类型字面量
                let (name, arg_name) = Self::parse_typeliteral_def(tl1)?;
                let mut set = std::collections::HashSet::new();
                for name in arg_name.iter() {
                    if !set.insert(name) {
                        return Err(analysis_error("有重名的类型参数", 0, 0));
                    }
                }
                drop(set);
                let arg_len = arg_name.len();
                self.create_type_env();
                for x in arg_name {
                    let var = self.type_env.new_t_var();
                    self.type_env.type_variable.insert(Cow::from(x), var);
                }
                let rt = {
                    let rt = self.parse_typeliteral(tl2)?;
                    unsafe { self.delete_type_env() };
                    if arg_len > 0 {
                        self.type_env.gen_move(rt)
                    } else {
                        rt
                    }
                };
                // println!("{:?}", self.type_env);
                self.type_env.type_variable.insert(Cow::from(name), rt);
                // println!("{:?}", self.type_env);
                Ok(())
            }
            Stat::for循环 {
                forin,
                pattern,
                block,
            } => todo!(),
            Stat::while循环 { whilein, block } => {
                // 确保条件是Bool类型,确保块表达式的返回值是 ()
                // 即可
                let t1 = self.analysis_expr(whilein)?;
                self.type_env.unify(&Type::Basic(BasicType::Bool), &t1)?;
                let t2 = self.analysis_expr(block)?;
                self.type_env.unify(&Type::Basic(BasicType::Unit), &t2)?;
                Ok(())
            }
        }
    }
    /// 模式匹配的解析
    pub fn parse_pattern<'b>(&self, pattern: &'b Pattern) -> &'b str {
        match pattern {
            Pattern::Id(name) => name,
            Pattern::Ignore => todo!(),
            Pattern::TupleMatch(_) => todo!(),
            Pattern::ArrayMatch(_) => todo!(),
            Pattern::ObjectMatch(_) => todo!(),
            Pattern::EnumMember {
                enum_name,
                enum_member,
                pattern_group,
            } => todo!(),
        }
    }
}
/// 作用域
#[derive(Debug, Default)]
pub struct Scope {
    /// 父作用域
    parent_scope: Option<Box<Scope>>,
    /// 返回类型
    return_type: Type,
}
impl Scope {
    fn new(p: Option<Box<Scope>>) -> Self {
        Self {
            return_type: Type::Basic(BasicType::Unit),
            parent_scope: p,
        }
    }
}
// // 将字符串 [a-zA-Z][a-zA-Z]? 转换成数字
// pub fn type_literal_var_to_usize(str: &str) -> usize {
//     unsafe {
//         // 限制过了str 必须是 [a-zA-Z][a-zA-Z]?
//         // 因此是safe的
//         match str.len() {
//             1 => {
//                 let p = str.chars().next().unwrap_unchecked();
//                 p.to_digit(36).unwrap_unchecked() as usize
//             }
//             2 => {
//                 let mut it = str.chars();
//                 let (p, c) = (it.next().unwrap_unchecked(), it.next().unwrap_unchecked());
//                 p.to_digit(36).unwrap_unchecked() as usize
//                     * c.to_digit(36).unwrap_unchecked() as usize
//             }
//             _ => unreachable_unchecked(),
//         }
//     }
// }
#[derive(Default)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct TypeEnv<'a> {
    /// 父环境
    parent: Option<Box<TypeEnv<'a>>>,
    /// 级别
    level: usize,
    /// 类型变量表
    type_variable: HashMap<Cow<'a, str>, Type>,
    ///
    next: usize,
    ///
    uf: UnionFind,
}
impl<'a> TypeEnv<'a> {
    pub fn new(parent: Option<Box<TypeEnv<'a>>>, level: usize) -> Self {
        TypeEnv {
            level,
            parent,
            type_variable: HashMap::new(),
            next: 0,
            uf: UnionFind::from_iter([
                Type::Basic(BasicType::Num),
                Type::Basic(BasicType::Unit),
                Type::Basic(BasicType::Bool),
                Type::Basic(BasicType::Str),
            ]),
        }
    }
    /// 初始化函数,只在 [Analyzer](crate::analyzer::Analyzer) 构建时使用一次
    pub fn init(mut self) -> Self {
        self.type_variable
            .insert(Cow::Borrowed("Num"), Type::Basic(BasicType::Num));
        self.type_variable
            .insert(Cow::Borrowed("Unit"), Type::Basic(BasicType::Unit));
        self.type_variable
            .insert(Cow::Borrowed("Bool"), Type::Basic(BasicType::Bool));
        self.type_variable
            .insert(Cow::Borrowed("Str"), Type::Basic(BasicType::Str));
        self
    }

    /// 找出代表元,必定成功
    pub fn find_meta(&mut self, t: &Type) -> Type {
        match t {
            Type::Basic(_) => t.clone(),
            Type::Tuple(t) => Type::Tuple(t.iter().map(|t| self.find_meta(t)).collect()),
            Type::Fn(t1, t2) => {
                Type::Fn(Box::new(self.find_meta(t1)), Box::new(self.find_meta(t2)))
            }
            Type::TVar(_, _) | Type::QVar(_) => {
                if !self.uf.has(t) {
                    self.uf.insert(t.clone());
                }
                // safe: uf中必定存在t
                // 同理, t也必定拥有代表元
                unsafe {
                    self.uf
                        .find_representing_meta(self.uf.find(t).unwrap_unchecked())
                        .unwrap_unchecked()
                }
            }
        }
    }
    /// 合并
    pub fn unify(&mut self, t1: &Type, t2: &Type) -> Result<(), ParseError> {
        // 说明两个类型相同 or 已经合并
        if t1 == t2 || self.uf.is_same_set(t1, t2) {
            return Ok(());
        }
        // if !self.uf.has(t1) {
        //     self.uf.insert(t1.clone());
        // }
        // if !self.uf.has(t2) {
        //     self.uf.insert(t2.clone());
        // }
        // 已经确保UF里存在t1,t2
        // 找出代表元
        let (_t1, _t2) = (self.find_meta(t1), self.find_meta(t2));
        let (line, column) = (0, 0);
        match (&_t1, &_t2) {
            (Type::Basic(t1), Type::Basic(t2)) if t1 == t2 => Ok(()),
            (Type::Tuple(t1), Type::Tuple(t2)) => t1
                .iter()
                .zip(t2.iter())
                .try_for_each(|(t1, t2)| self.unify(t1, t2)),
            (Type::Fn(ref t1, ref t2), Type::Fn(ref t3, ref t4)) => {
                self.unify(t1, t3)?;
                self.unify(t2, t4)?;
                Ok(())
            }
            (Type::QVar(_), _) => {
                let meta1 = self.find_meta(&_t1);
                if matches!(meta1, Type::QVar(_)) {
                    Ok(())
                } else {
                    self.unify(&meta1, t2)
                }
            }
            (_, Type::QVar(_)) => {
                let meta2 = self.find_meta(&_t2);
                if matches!(meta2, Type::QVar(_)) {
                    Ok(())
                } else {
                    self.unify(&meta2, t1)
                }
            }
            (Type::TVar(..), _) => {
                self.uf.union(&_t2, &_t1);
                let p = self.uf.find(t1).unwrap();
                self.uf.representing_meta.insert(p, t2.clone());
                Ok(())
            }
            (_, Type::TVar(..)) => {
                self.uf.union(&_t1, &_t2);
                let p = self.uf.find(t2).unwrap();
                self.uf.representing_meta.insert(p, t1.clone());
                Ok(())
            }
            _ => Err(analysis_error(
                &format!("类型 {t1:?} 和 {t2:?} 不兼容"),
                line,
                column,
            )),
        }
    }

    /// gen 的引用版本
    ///
    /// 尝试将一个类型多态化
    ///
    /// 可能会原封不动的返回
    pub fn gen_ref(&self, t: &Type) -> Type {
        match t {
            Type::TVar(i, level) =>
            // 类型变量处于更下层的作用域
            // 直接转换成QVar
            {
                if *level > self.level {
                    Type::QVar(*i)
                } else {
                    t.clone()
                }
            }

            Type::Tuple(ts) => Type::Tuple(ts.iter().map(|t| self.gen_ref(t)).collect()),
            Type::Fn(t1, t2) => Type::Fn(Box::new(self.gen_ref(t1)), Box::new(self.gen_ref(t2))),
            // 基元类型,QVar本身可以忽略
            Type::Basic(_) | Type::QVar(_) => t.clone(),
        }
    }
    /// gen 的所有权版本
    pub fn gen_move(&self, t: Type) -> Type {
        match t {
            Type::TVar(i, level) =>
            // 类型变量处于更下层的作用域
            // 直接转换成QVar
            {
                if level > self.level {
                    Type::QVar(i)
                } else {
                    t
                }
            }

            Type::Tuple(ts) => Type::Tuple(ts.into_iter().map(|t| self.gen_move(t)).collect()),
            Type::Fn(t1, t2) => {
                Type::Fn(Box::new(self.gen_move(*t1)), Box::new(self.gen_move(*t2)))
            }
            // 基元类型,QVar本身可以忽略
            Type::Basic(_) | Type::QVar(_) => t,
        }
    }
    /// [`gen`](crate::analyzer::TypeEnv::gen)的逆操作的引用版本
    ///
    /// 可能会原封不动的返回
    pub fn inst(t: &mut Type, level: usize) -> Type {
        match t {
            Type::TVar(..) => t.clone(),
            Type::Tuple(t) => Type::Tuple(t.iter_mut().map(|t| Self::inst(t, level)).collect()),
            Type::Fn(t1, t2) => Type::Fn(
                Box::new(Self::inst(t1, level)),
                Box::new(Self::inst(t2, level)),
            ),
            Type::Basic(_) => t.clone(),
            Type::QVar(i) => Type::TVar(*i + 100, level),
        }
    }
    /// [`gen`](crate::analyzer::TypeEnv::gen)的逆操作的所有权版本
    pub fn inst_move(t: Type, level: usize) -> Type {
        match t {
            Type::TVar(..) => t,
            Type::Tuple(t) => {
                Type::Tuple(t.into_iter().map(|t| Self::inst_move(t, level)).collect())
            }
            Type::Fn(t1, t2) => Type::Fn(
                Box::new(Self::inst_move(*t1, level)),
                Box::new(Self::inst_move(*t2, level)),
            ),
            Type::Basic(_) => t,
            Type::QVar(i) => Type::TVar(i + 100, level),
        }
    }
    /// 寻找某个名称对应的类型变量
    pub fn get_type(&self, name: &str) -> Option<Type> {
        let t = self.type_variable.get(name);
        if t.is_none() {
            if let Some(ref p_type_env) = self.parent {
                p_type_env.get_type(name)
            } else {
                None
            }
        } else {
            t.cloned()
        }
    }
    fn get_next(&mut self) -> usize {
        self.next += 1;
        self.next - 1
    }
    /// 顾名思义,在多态类型上调用另一个类型,替换 [`QVar`](crate::analyzer::Type) 到具体的类型
    pub fn call_type(t: Type, arg_types: &[Type]) -> Type {
        match t {
            Type::TVar(_, _) => t,
            Type::Tuple(ts) => Type::Tuple(
                ts.into_iter()
                    .map(|t| Self::call_type(t, arg_types))
                    .collect(),
            ),
            Type::Fn(t1, t2) => Type::Fn(
                Box::new(Self::call_type(*t1, arg_types)),
                Box::new(Self::call_type(*t2, arg_types)),
            ),
            Type::QVar(i) => arg_types[i].clone(),
            Type::Basic(_) => t,
        }
    }
    pub fn new_t_var(&mut self) -> Type {
        Type::TVar(self.get_next(), self.level)
    }
}
