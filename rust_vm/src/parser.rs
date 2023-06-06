//! 解析器模块
use std::collections::HashMap;

use crate::{
    error::{syntax_error, ParseError},
    tokenizer::{Token, TokenType, Tokenizer},
};
/// 位置,
type Loc = (u32, u32);
/// 语句
#[derive(Debug)]
pub enum Stat {
    /// ```
    ///
    /// 1+1
    ///
    /// ```
    表达式语句 { expression: BoxExpr },
    /// ```
    ///
    /// let {a:c}:T = 3
    ///
    /// ```
    变量声明语句 {
        /// 是不是常量声明?
        is_const: bool,
        /// 模式
        pattern: Pattern,
        /// 可选的类型标注
        ///
        /// 但如果`is_const`为`true`,那么类型标注是必须的
        type_annotation: Option<TypeLiteral>,
        /// 右侧的初始化表达式
        right: BoxExpr,
    },
    /// ```
    ///
    /// async fn (d:T,{s:p}:U) -> T {...}
    ///
    /// ```
    函数声明语句 {
        /// 是不是异步函数?
        is_async: bool,
        /// 函数名称
        name: &'static str,
        /// 参数列表,由(模式,可选的类型标注)元组构成的不定长数组
        args: Vec<(Pattern, Option<TypeLiteral>)>,
        /// 返回值的可选的类型标注
        ret_type_annotation: Option<TypeLiteral>,
        /// 语句块
        block_expr: BoxExpr,
    },
    /// ```
    ///
    /// return 3
    ///
    /// ```
    返回值语句 { expression: BoxExpr },
    /// ```
    ///
    /// type P<T> = (T,T)
    ///
    /// ```
    类型声明语句(TypeLiteral, TypeLiteral),
    /// ```
    ///
    /// for a in b {}
    ///
    /// ```
    for循环 {
        /// 被循环的表达式
        forin: BoxExpr,
        ///
        pattern: Pattern,
        /// 要执行的语句
        block: BoxExpr,
    },
    /// ```
    ///
    /// while a {}
    ///
    /// ```
    while循环 { whilein: BoxExpr, block: BoxExpr },
    //
    // 属性语句()
}
/// "模式"
#[derive(Debug)]
pub enum Pattern {
    /// `e`
    Id(&'static str),
    /// `_`
    Ignore,
    /// ```
    ///
    /// (e,l:r)
    ///
    /// ```
    TupleMatch(Vec<Pattern>),
    /// ```
    ///
    /// [x:xs]
    ///
    /// ```
    ArrayMatch(Vec<Pattern>),
    /// ```
    ///
    /// {a,b:c}
    ///
    /// ```
    ObjectMatch(HashMap<&'static str, Pattern>),
    /// ```
    ///
    /// E::a | E::b(_)
    ///
    /// ```
    EnumMember {
        enum_name: &'static str,
        enum_member: &'static str,
        pattern_group: Vec<Pattern>,
    },
}
#[derive(Debug)]
pub enum Expr {
    /// ```
    ///
    /// 23 | true
    ///
    /// ```
    字面量 { value: &'static str },
    /// ```
    ///
    /// (2,) | (3,2,3)
    ///
    /// ```
    元组 { elements: Vec<BoxExpr> },
    /// ```
    ///
    /// ()
    ///
    /// ```
    单元 {},
    /// ```
    ///
    /// a
    ///
    /// ````
    标识符 { value: &'static str },
    /// `1+1`
    二元表达式 {
        operator: &'static str,
        left: Box<BoxExpr>,
        right: Box<BoxExpr>,
    },
    /// `d` = `3`
    赋值表达式 {
        operator: &'static str,
        left: Box<BoxExpr>,
        right: Box<BoxExpr>,
    },
    /// `{1+1}`
    块表达式 { body: Vec<Stat> },
    /// `{a}` | `{a,l:1}`
    对象表达式 {
        property: Vec<(&'static str, BoxExpr)>,
    },
    /// \c => 1 | \() => 2 | \(a,b) => 3
    函数表达式 {
        parma: Vec<(Pattern, Option<TypeLiteral>)>,
        body: Box<BoxExpr>,
    },
    函数调用表达式 {
        callee: Box<BoxExpr>,
        args: Vec<BoxExpr>,
    },
}
/// 包装了expr和它对应的loc
#[derive(Debug)]
pub struct BoxExpr(pub Expr, pub Loc);
impl BoxExpr {
    pub fn new(expr: Expr, loc: Loc) -> Self {
        Self(expr, loc)
    }
}
/// 类型字面量
///
/// `S<T>` <=> `TypeLiteral{name:"s",type_arg:Some(vec![{name:"T",type_arg:None}])}`
#[derive(Debug)]
// #[derive(Clone)]
pub enum TypeLiteral {
    /// 带参数的类型字面量
    ///
    /// `T<U>`
    Generic {
        /// 类型名称
        name: &'static str,
        /// 类型参数
        type_args: Vec<TypeLiteral>,
        loc: Loc,
    },
    /// 无参数的类型字面量
    ///
    /// `T`
    Normal(&'static str, Loc),
    /// 类型变量
    ///
    /// 为了方便解析,泛型参数必须在标识符开头加上一个单引号
    ///
    /// T<'a,'b>
    Variable(&'static str, Loc),
    /// 元组类型字面量
    ///
    /// `(T,U)`
    Tuple(Vec<TypeLiteral>, Loc),
    /// 字面量 `_`
    Infer(Loc),
    /// 对象
    ObjLiteral(HashMap<&'static str, TypeLiteral>, Loc),
    /// 函数
    Fn(Vec<TypeLiteral>, Loc),
}
/// 解析器
pub struct Parser {
    /// 分词器
    pub tokenizer: Tokenizer,
    /// 当前token
    pub now_token: Token,
    /// 语法树
    pub ast: Vec<Result<Stat, ParseError>>,
    /// 上一个token
    pub previous_token: Token,
}
impl Parser {
    /// 启动解析
    pub fn start(&mut self) {
        if let Some(token) = self.get_next_token() {
            if let TokenType::结束 = token.token_type {
                return;
            }
            self.now_token = token;
            self.parse();
        }
    }
    /// 设置新的ns代码,在分词器源码末尾添加
    pub fn set_source_code(&mut self, source_code: &str) {
        self.tokenizer.source_code += source_code;
    }
    pub fn new(str: &str) -> Self {
        Parser {
            tokenizer: Tokenizer::new(str),
            now_token: Token {
                token_type: TokenType::空格,
                value: " ",
                line: 0,
                column: 0,
            },
            ast: vec![],
            previous_token: Token {
                token_type: TokenType::空格,
                value: " ",
                line: 0,
                column: 0,
            },
        }
    }
    /// 解析入口
    fn parse(&mut self) {
        loop {
            let stat = self.parse_stat();
            self.ast.push(stat);
            // 匹配掉 `;`
            self.match_token_value(&[";"]);
            if self.match_token_type(TokenType::结束) {
                break;
            }
        }
    }
    /// 拿取下一个token,并且将parser的各项数据与新token对齐
    fn eat_token(&mut self) {
        self.previous_token = self.now_token.clone();
        self.now_token = self.get_next_token().unwrap_or(Token {
            token_type: TokenType::结束,
            value: " ",
            line: self.now_token.line,
            column: self.now_token.column,
        });
    }
    /// 检查当前token的值,然后推进
    fn match_token_value(&mut self, value_arr: &[&str]) -> bool {
        if value_arr.iter().any(|c| *c == self.now_token.value) {
            self.eat_token();
            true
        } else {
            false
        }
    }
    /// 检查当前token的类型,然后推进
    fn match_token_type(&mut self, token_type: TokenType) -> bool {
        if token_type == self.now_token.token_type {
            self.eat_token();
            true
        } else {
            false
        }
    }
    /// 检查当前token的值,不推进
    fn check_token_value(&self, value_arr: &[&str]) -> bool {
        value_arr.iter().any(|c| *c == self.now_token.value)
    }
    /// 检查当前token的类型,不推进
    fn check_token_type(&self, token_type: TokenType) -> bool {
        token_type == self.now_token.token_type
    }
    /// 获取下一个token
    fn get_next_token(&mut self) -> Option<Token> {
        self.tokenizer.next().map(|token| {
            if let TokenType::空格
            | TokenType::单行注释
            | TokenType::块注释
            | TokenType::换行符
            | TokenType::文档注释 = token.token_type
            {
                self.eat_token();
                self.now_token.clone()
            } else {
                token
            }
        })
    }
    /// 获取上一个token
    fn get_previous_token(&self) -> &Token {
        &self.previous_token
    }
    /// 行为与match_token_value一致,不同之处在于如果匹配不成功就进入恐慌模式
    fn consume_token_value(&mut self, value_arr: &[&str], message: &str) -> Result<(), ParseError> {
        if value_arr.iter().any(|c| *c == self.now_token.value) {
            self.eat_token();
            Ok(())
        } else {
            Err(self.panic_mode(message))
        }
    }
    /// 行为与match_token_type一致,不同之处在于如果匹配不成功就进入恐慌模式
    fn consume_token_type(
        &mut self,
        token_type: TokenType,
        message: &str,
    ) -> Result<(), ParseError> {
        if token_type == self.now_token.token_type {
            self.eat_token();
            Ok(())
        } else {
            Err(self.panic_mode(message))
        }
    }
    /// 进入恐慌模式
    ///
    /// 开始吞token,直到now_token是下个语句开头,或者 `;`
    fn panic_mode(&mut self, message: &str) -> ParseError {
        while !self.check_token_value(&[";", "{", "let"]) {
            self.eat_token();
        }
        syntax_error(message, self.now_token.line, self.now_token.column)
    }
    /// 解析语句的入口
    fn parse_stat(&mut self) -> Result<Stat, ParseError> {
        if self.match_token_value(&["let"]) {
            let pattern = self.parse_pattern()?;
            let mut type_annotation = None;
            if self.match_token_value(&[":"]) {
                type_annotation = Some(self.parse_type_literal()?);
            }
            self.consume_token_value(&["="], "缺失等于号")?;
            Ok(Stat::变量声明语句 {
                is_const: false,
                pattern,
                type_annotation,
                right: self.parse_expr()?,
            })
        } else if self.match_token_value(&["while"]) {
            let expr = self.parse_expr()?;
            let block = self.parse_block_expr()?;
            Ok(Stat::while循环 {
                whilein: expr,
                block,
            })
        } else if self.match_token_value(&["for"]) {
            let pattern = self.parse_pattern()?;
            self.consume_token_value(&["in"], "缺失 `in`")?;
            let for_expr = self.parse_expr()?;
            let block = self.parse_block_expr()?;
            Ok(Stat::for循环 {
                forin: for_expr,
                pattern,
                block,
            })
        } else if self.match_token_value(&["type"]) {
            let type_name = self.parse_type_literal()?;
            self.consume_token_value(&["="], "缺失等于号")?;
            Ok(Stat::类型声明语句(
                type_name,
                self.parse_type_literal()?,
            ))
        } else if self.match_token_value(&["enum"]) {
            todo!()
        } else if self.match_token_value(&["fn"]) {
            self.consume_token_type(TokenType::标识符, "需要函数名")?;
            let name = self.get_previous_token().value;
            self.consume_token_type(TokenType::左括号, "缺失左括号")?;
            let mut args = vec![];
            while !self.match_token_type(TokenType::右括号) {
                let pat = self.parse_pattern()?;
                if self.check_token_value(&[":"]) {
                    self.match_token_value(&[":"]);
                    args.push((pat, Some(self.parse_type_literal()?)));
                } else {
                    self.consume_token_value(&[","], "缺失逗号")?;
                    args.push((pat, None));
                }
            }
            let mut ret_type_annotation = None;
            if self.check_token_value(&["->"]) {
                self.match_token_value(&["->"]);
                ret_type_annotation = Some(self.parse_type_literal()?);
            }
            Ok(Stat::函数声明语句 {
                is_async: false,
                name,
                args,
                ret_type_annotation,
                block_expr: self.parse_expr()?,
            })
        }
        //  else if self.match_token_value(&["return"]) {
        //     Ok(Stat::返回值语句 {
        //         expression: self.parse_expr()?,
        //     })
        // }
        else {
            // 以上条件都不满足说明是表达式语句
            let expression = self.parse_expr()?;
            self.match_token_value(&[";"]);
            Ok(Stat::表达式语句 { expression })
        }
    }
    /// 解析类型字面量,例如:
    ///
    /// `S` | `S<T>` | `S<T,U>` | `S<T<U>,P>` | `(S,)` | `(S<T>,U)`
    fn parse_type_literal(&mut self) -> Result<TypeLiteral, ParseError> {
        let loc = (self.tokenizer.line, self.tokenizer.column);
        let mut result = vec![self.parse_type_literal_utils()?];
        while self.match_token_value(&["->"]) {
            result.push(self.parse_type_literal_utils()?);
        }
        if result.len() == 1 {
            Ok(result.pop().unwrap())
        } else {
            Ok(TypeLiteral::Fn(result, loc))
        }
    }
    /// 辅助函数
    fn parse_type_literal_utils(&mut self) -> Result<TypeLiteral, ParseError> {
        let loc = (self.tokenizer.line, self.tokenizer.column);
        if self.match_token_value(&["("]) {
            if self.match_token_value(&[")"]) {
                Ok(TypeLiteral::Tuple(vec![], loc))
            } else if self.check_token_type(TokenType::标识符)
                || self.check_token_value(&["_"])
                || self.check_token_type(TokenType::泛型专用标识符)
            {
                let mut p = vec![self.parse_type_literal()?];
                while !self.match_token_value(&[")"]) {
                    self.consume_token_value(&[","], "")?;
                    p.push(self.parse_type_literal()?);
                }
                Ok(TypeLiteral::Tuple(p, loc))
            } else {
                Err(self.panic_mode("不合法的类型标注"))
            }
        } else if self.match_token_type(TokenType::标识符) {
            let name = self.get_previous_token().value;
            if self.match_token_value(&["<"]) {
                let mut type_arg = vec![self.parse_type_literal()?];
                while !self.match_token_value(&[">"]) {
                    self.consume_token_value(&[","], "")?;
                    type_arg.push(self.parse_type_literal()?);
                }
                Ok(TypeLiteral::Generic {
                    name,
                    type_args: type_arg,
                    loc,
                })
            } else {
                Ok(TypeLiteral::Normal(name, loc))
            }
        } else if self.match_token_type(TokenType::泛型专用标识符) {
            if self.check_token_value(&["<"]) {
                Err(self.panic_mode("泛型参数不允许有自己的参数"))
            } else {
                let name = self.get_previous_token().value;
                Ok(TypeLiteral::Variable(name, loc))
            }
        } else if self.match_token_value(&["_"]) {
            Ok(TypeLiteral::Infer(loc))
        } else if self.match_token_value(&["{"]) {
            if self.match_token_value(&["}"]) {
                Err(self.panic_mode("使用 `()` 来代替"))
            } else {
                let mut field_table = HashMap::new();
                while !self.match_token_value(&["}"]) {
                    self.consume_token_type(TokenType::标识符, "需要标识符")?;

                    let field_name = self.get_previous_token().value;
                    self.consume_token_value(&[":"], "需要 `:`")?;

                    field_table.insert(field_name, self.parse_type_literal()?);
                    self.match_token_value(&[";", ","]);
                }
                Ok(TypeLiteral::ObjLiteral(field_table, loc))
            }
        } else {
            Err(self.panic_mode("不合法的类型标注"))
        }
    }
    /// 解析 "模式"
    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        if self.match_token_value(&["("]) {
            if self.match_token_value(&[")"]) {
                return Err(self.panic_mode("无意义的模式,用 `_` 代替"));
            }
            let mut pattern_arr = vec![self.parse_pattern()?];
            while !self.match_token_value(&[")"]) {
                self.consume_token_value(&[","], "缺失逗号")?;
                if self.match_token_value(&[")"]) {
                    break;
                }
                pattern_arr.push(self.parse_pattern()?);
            }
            Ok(Pattern::TupleMatch(pattern_arr))
        } else if self.match_token_value(&["{"]) {
            if self.match_token_value(&["}"]) {
                return Err(self.panic_mode("不合法的类型标注"));
            }
            let mut pattern_map = HashMap::new();

            while !self.match_token_value(&["}"]) {
                self.consume_token_type(TokenType::标识符, "a")?;
                let name = self.get_previous_token().value;
                if self.match_token_value(&[":"]) {
                    pattern_map.insert(name, self.parse_pattern()?);
                } else if self.match_token_value(&[","]) {
                    pattern_map.insert(name, Pattern::Id(name));
                }
            }
            Ok(Pattern::ObjectMatch(pattern_map))
        } else if self.match_token_type(TokenType::标识符) {
            let name = self.get_previous_token().value;
            if self.match_token_value(&["::"]) {
                self.consume_token_type(TokenType::标识符, "缺失枚举成员名")?;
                let member_name = self.get_previous_token().value;
                if self.match_token_value(&["("]) {
                    if self.match_token_value(&[")"]) {
                        return Ok(Pattern::EnumMember {
                            enum_name: name,
                            enum_member: member_name,
                            pattern_group: vec![],
                        });
                    }
                    let mut pattern = vec![self.parse_pattern()?];
                    while !self.match_token_value(&[")"]) {
                        self.consume_token_value(&[","], "缺失逗号")?;
                        if self.match_token_value(&[")"]) {
                            break;
                        }
                        pattern.push(self.parse_pattern()?);
                    }
                    return Ok(Pattern::EnumMember {
                        enum_name: name,
                        enum_member: member_name,
                        pattern_group: pattern,
                    });
                }
                return Ok(Pattern::EnumMember {
                    enum_name: name,
                    enum_member: member_name,
                    pattern_group: vec![],
                });
            }
            Ok(Pattern::Id(name))
        } else if self.match_token_value(&["_"]) {
            Ok(Pattern::Ignore)
        } else {
            todo!()
        }
    }
    /// 解析表达式的入口
    fn parse_expr(&mut self) -> Result<BoxExpr, ParseError> {
        self.assignment()
    }
    /// 函数调用表达式
    fn call(&mut self) -> Result<BoxExpr, ParseError> {
        let column = self.get_previous_token().column;
        let line = self.get_previous_token().line;
        let mut expr = self.primary_expr()?;
        while self.match_token_value(&["("]) {
            if self.match_token_value(&[")"]) {
                expr = BoxExpr::new(
                    Expr::函数调用表达式 {
                        args: vec![],
                        callee: Box::new(expr),
                    },
                    (line, column),
                );
            } else {
                let mut args = vec![];
                crate::do_while!({ args.push(self.call()?) }, self.match_token_value(&[","]));
                expr = BoxExpr::new(
                    Expr::函数调用表达式 {
                        args,
                        callee: Box::new(expr),
                    },
                    (line, column),
                );
                self.consume_token_value(&[")"], "缺失 `)`")?;
            }
        }
        Ok(expr)
    }
    /// 乘除表达式
    fn factor(&mut self) -> Result<BoxExpr, ParseError> {
        let mut expr = self.call()?;
        let column = self.get_previous_token().column;
        let line = self.get_previous_token().line;
        while self.match_token_value(&["*", "/"]) {
            let operator = self.get_previous_token().value;
            let right = self.call()?;
            expr = BoxExpr::new(
                Expr::二元表达式 {
                    operator,
                    left: Box::new(expr),
                    right: Box::new(right),
                },
                (line, column),
            );
        }
        Ok(expr)
    }
    /// 加减表达式
    fn term(&mut self) -> Result<BoxExpr, ParseError> {
        // 加减法优先级很低
        // 首先调用乘法解析,之后的表达式都会递归解析
        let mut expr = self.factor()?;
        let column = self.get_previous_token().column;
        let line = self.get_previous_token().line;
        // 解析完后轮到自己
        while self.match_token_value(&["+", "-"]) {
            // match消耗了运算符,所以用 getPreviousToken 拿回来
            let operator = self.get_previous_token().value;
            // 右边再次递归解析
            let right = self.factor()?;
            // 它本身是新表达式的左侧
            // 现在可以生成一颗表达式树了
            expr = BoxExpr::new(
                Expr::二元表达式 {
                    operator,
                    left: Box::new(expr),
                    right: Box::new(right),
                },
                (line, column),
            );
            // 谁不优先谁就在函数调用栈的上层
            // 然后立即调用比自己优先一层的解析函数
        }
        Ok(expr)
    }
    /// 赋值表达式
    fn assignment(&mut self) -> Result<BoxExpr, ParseError> {
        let mut expr = self.term();
        let column = self.get_previous_token().column;
        let line = self.get_previous_token().line;
        while self.match_token_value(&["="]) {
            let right = self.assignment();
            let operator = self.get_previous_token().value;
            expr = Ok(BoxExpr::new(
                Expr::赋值表达式 {
                    operator,
                    left: Box::new(expr?),
                    right: Box::new(right?),
                },
                (line, column),
            ));
        }
        expr
    }
    /// 块表达式
    fn parse_block_expr(&mut self) -> Result<BoxExpr, ParseError> {
        let Token {
            mut line,
            mut column,
            ..
        } = self.get_previous_token();
        let mut body = vec![];
        loop {
            body.push(self.parse_stat()?);
            self.match_token_value(&[";"]);
            if self.match_token_value(&["}"]) {
                break;
            }
        }
        Ok(BoxExpr::new(Expr::块表达式 { body }, (line, column)))
    }
    /// ### 优先级最高,基本表达式
    pub fn primary_expr(&mut self) -> Result<BoxExpr, ParseError> {
        if self.match_token_type(TokenType::数字字面量)
            || self.match_token_type(TokenType::布尔值)
            || self.match_token_type(TokenType::字符串)
        {
            let Token {
                value,
                column,
                line,
                ..
            } = &self.get_previous_token();
            Ok(BoxExpr::new(Expr::字面量 { value }, (*line, *column)))
        } else if self.match_token_type(TokenType::标识符) {
            let Token {
                value,
                column,
                line,
                ..
            } = &self.get_previous_token();
            Ok(BoxExpr::new(Expr::标识符 { value }, (*line, *column)))
        } else if self.match_token_type(TokenType::左括号) {
            let column = self.get_previous_token().column;
            let line = self.get_previous_token().line;
            if self.match_token_type(TokenType::右括号) {
                return Ok(BoxExpr::new(Expr::单元 {}, (line, column)));
            }
            let expr = self.parse_expr()?;
            if self.match_token_value(&[","]) {
                let mut tuple = vec![expr];
                while !self.match_token_type(TokenType::右括号) {
                    tuple.push(self.parse_expr()?);
                    self.match_token_value(&[","]);
                }
                Ok(BoxExpr::new(Expr::元组 { elements: tuple }, (line, column)))
            } else {
                self.consume_token_type(TokenType::右括号, "缺失右括号")?;
                Ok(expr)
            }
        } else if self.check_token_value(&["{"]) {
            let Token { line, column, .. } = self.now_token;
            self.match_token_value(&["{"]);
            if self.match_token_value(&["}"]) {
                return Ok(BoxExpr::new(
                    Expr::块表达式 { body: vec![] },
                    (line, column),
                ));
            };
            if self.check_token_type(TokenType::标识符) {
                let mut property = vec![];
                while !self.match_token_value(&["}"]) {
                    self.consume_token_type(TokenType::标识符, "缺失 属性名")?;
                    let Token {
                        value: property_name,
                        line,
                        column,
                        ..
                    } = self.get_previous_token().clone();
                    let expr = if self.match_token_value(&[":"]) {
                        self.parse_expr()?
                    } else {
                        BoxExpr::new(
                            Expr::标识符 {
                                value: property_name,
                            },
                            (line, column),
                        )
                    };
                    property.push((property_name, expr));
                    self.match_token_value(&[","]);
                }
                Ok(BoxExpr::new(Expr::对象表达式 { property }, (line, column)))
            } else {
                self.parse_block_expr()
            }
        } else if self.match_token_value(&["\\"]) {
            let line = self.get_previous_token().line;
            let column = self.get_previous_token().column;
            // 任意参数
            if self.match_token_value(&["("]) {
                // 空参数
                if self.match_token_value(&[")"]) {
                    self.consume_token_value(&["=>"], "缺失 `=>`")?;
                    Ok(BoxExpr::new(
                        Expr::函数表达式 {
                            parma: vec![],
                            body: Box::new(self.parse_expr()?),
                        },
                        (line, column),
                    ))
                } else {
                    let mut parma = vec![];
                    crate::do_while!(
                        {
                            let arg = self.parse_pattern()?;
                            let mut type_annotation = None;
                            if self.match_token_value(&[":"]) {
                                type_annotation = Some(self.parse_type_literal()?);
                            }
                            parma.push((arg, type_annotation));
                        },
                        self.match_token_value(&[","])
                    );
                    self.consume_token_value(&[")"], "缺失 `)`")?;
                    self.consume_token_value(&["=>"], "缺失 `=>`")?;
                    Ok(BoxExpr::new(
                        Expr::函数表达式 {
                            parma,
                            body: Box::new(self.parse_expr()?),
                        },
                        (line, column),
                    ))
                }
            } else {
                // 单参数
                let parma = self.parse_pattern()?;
                let mut type_annotation = None;
                if self.match_token_value(&[":"]) {
                    type_annotation = Some(self.parse_type_literal()?);
                }
                self.consume_token_value(&["=>"], "缺失 `=>`")?;
                Ok(BoxExpr::new(
                    Expr::函数表达式 {
                        parma: vec![(parma, type_annotation)],
                        body: Box::new(self.parse_expr()?),
                    },
                    (line, column),
                ))
            }
        } else {
            Err(self.panic_mode("需要 表达式"))
        }
    }
}
