//! 解析器模块
use std::collections::HashMap;

use crate::{
    analyzer::NsType,
    tokenizer::{Token, TokenType, Tokenizer},
};
/// 语句
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum Stat {
    表达式语句 {
        expression: Expr,
    },
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
        right: Expr,
    },
    函数声明语句 {
        /// 是不是异步函数?
        is_async: bool,
        /// 函数名称
        name: String,
        /// 参数列表,元素是(模式,可选的类型标注)元组的不定长数组
        args: Vec<(Pattern, Option<TypeLiteral>)>,
        /// 返回值的可选的类型标注
        ret_type_annotation: Option<TypeLiteral>,
        /// 语句块
        block_expr: Expr,
    },
    返回值语句 {
        expression: Expr,
    },
}
/// "模式"
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum Pattern {
    Id(String),
    Ignore,
    TupleMatch(Vec<Pattern>),
    ArrayMatch(Vec<Pattern>),
    ObjectMatch(HashMap<String, Pattern>),
    EnumMember {
        enum_name: String,
        enum_member: String,
        pattern_group: Vec<Pattern>,
    },
}
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum Expr {
    字面量 {
        value: String,
        line: i32,
        column: i32,
    },
    元组 {
        elements: Vec<Expr>,
        line: i32,
        column: i32,
    },
    单元 {
        line: i32,
        column: i32,
    },
    标识符 {
        value: String,
        line: i32,
        column: i32,
    },
    二元表达式 {
        operator: String,
        left: Box<Expr>,
        right: Box<Expr>,
        line: i32,
        column: i32,
    },
    赋值表达式 {
        operator: String,
        left: Box<Expr>,
        right: Box<Expr>,
        line: i32,
        column: i32,
    },
    块表达式 {
        body: Vec<Stat>,
        line: i32,
        column: i32,
    },
    对象表达式 {
        property: Vec<(String, Expr)>,
    },
}

/// 类型字面量
///
/// `S<T>` <=> `TypeLiteral{name:"s",type_arg:Some(vec![{name:"T",type_arg:None}])}`
#[cfg_attr(debug_assertions, derive(Debug))]
#[derive(Clone)]
pub enum TypeLiteral {
    /// 带参数的类型字面量
    Generic {
        /// 类型名称
        name: String,
        /// 类型参数
        type_arg: Vec<TypeLiteral>,
    },
    /// 无参数的类型字面量
    Normal(String),
    /// 元组类型字面量
    Tuple(Vec<TypeLiteral>),
    /// 字面量 `_`
    Infer,
}
/// 解析器
pub struct Parser {
    /// 行,紧跟分词器
    pub line: i32,
    /// 列,紧跟分词器
    pub column: i32,
    /// 分词器
    pub tokenizer: Tokenizer,
    /// 当前token
    pub now_token: Token,
    /// 语法树
    pub ast: Vec<Stat>,
    /// 上一个token
    pub previous_token: Token,
    pub gen_bytecode: bool,
    pub translate: bool,
}
impl Parser {
    /// 启动解析
    pub fn start(&mut self) -> Result<(), String> {
        while let Some(token) = self.get_next_token() {
            if let TokenType::结束 = token.token_type {
                continue;
            }
            self.now_token = token;
            self.line = self.now_token.line;
            self.column = self.now_token.column;
            self.parse()?;
        }
        Ok(())
    }
    /// 让编译器生成字节码
    pub fn set_gen_bytecode(&mut self, gen_bytecode: bool) {
        self.gen_bytecode = gen_bytecode
    }
    /// 让编译器转译js
    pub fn set_translate(&mut self, translate: bool) {
        self.translate = translate
    }
    /// 设置新的ns代码,重置分词器
    pub fn set_source_code(&mut self, source_code: &str) {
        self.tokenizer = Tokenizer::new(source_code)
    }
    pub fn new(str: &str) -> Self {
        Parser {
            column: 1,
            line: 1,
            tokenizer: Tokenizer::new(str),
            now_token: Token {
                token_type: TokenType::空格,
                value: " ".to_string(),
                line: 0,
                column: 0,
            },
            ast: vec![],
            previous_token: Token {
                token_type: TokenType::空格,
                value: " ".to_string(),
                line: 0,
                column: 0,
            },
            gen_bytecode: false,
            translate: false,
        }
    }
    /// 解析入口
    fn parse(&mut self) -> Result<(), String> {
        let stat = self.parse_stat()?;
        self.ast.push(stat);
        Ok(())
    }
    /// 拿取下一个token,并且将parser的各项数据与新token对齐
    fn eat_token(&mut self) {
        self.previous_token = self.now_token.clone();
        self.now_token = self.get_next_token().unwrap_or_else(|| Token {
            token_type: TokenType::结束,
            value: " ".to_string(),
            line: self.now_token.line,
            column: self.now_token.column,
        });
        self.line = self.now_token.line;
        self.column = self.now_token.column;
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
    /// 行为与match_token_value一致,不同之处在于如果匹配不成功是一个错误
    fn consume_token_value(&mut self, value_arr: &[&str], message: &str) {
        if value_arr.iter().any(|c| *c == self.now_token.value) {
            self.eat_token();
        } else {
            todo!()
        }
    }
    /// 行为与match_token_type一致,不同之处在于如果匹配不成功是一个错误
    fn consume_token_type(&mut self, token_type: TokenType, message: &str) {
        if token_type == self.now_token.token_type {
            self.eat_token();
        } else {
            // 错误
            todo!()
        }
    }
    /// 解析语句的入口
    fn parse_stat(&mut self) -> Result<Stat, String> {
        if self.match_token_value(&["let"]) {
            let pattern = self.parse_pattern()?;
            let mut type_annotation = None;
            if self.match_token_value(&[":"]) {
                type_annotation = Some(self.parse_type_literal()?);
                println!("{:?}", type_annotation)
            }
            println!("{:?}", self.now_token);
            self.consume_token_value(&["="], "缺失等于号");
            Ok(Stat::变量声明语句 {
                is_const: false,
                pattern,
                type_annotation,
                right: self.parse_expr()?,
            })
        } else if self.match_token_value(&["while"]) {
            todo!()
        } else if self.match_token_value(&["for"]) {
            todo!()
        } else if self.match_token_value(&["type"]) {
            todo!()
        } else if self.match_token_value(&["enum"]) {
            todo!()
        } else if self.match_token_value(&["fn"]) {
            self.consume_token_type(TokenType::标识符, "需要函数名");
            let name = self.get_previous_token().value.to_string();
            self.consume_token_type(TokenType::左括号, "缺失左括号");
            let mut args = vec![];
            while !self.match_token_type(TokenType::右括号) {
                let pat = self.parse_pattern()?;
                if self.check_token_value(&[":"]) {
                    self.match_token_value(&[":"]);
                    args.push((pat, Some(self.parse_type_literal()?)));
                } else {
                    self.consume_token_value(&[","], "缺失逗号");
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
        } else {
            // 以上条件都不满足说明是表达式语句
            let expression = self.parse_expr()?;
            self.match_token_value(&[";"]);
            Ok(Stat::表达式语句 { expression })
        }
    }
    /// 解析类型字面量,例如:
    ///
    /// `S` | `S<T>` | `S<T,U>` | `S<T<U>,P>` | `(S,)` | `(S<T>,U)`
    fn parse_type_literal(&mut self) -> Result<TypeLiteral, String> {
        if self.match_token_value(&["("]) {
            if self.match_token_value(&[")"]) {
                Ok(TypeLiteral::Tuple(vec![]))
            } else if self.check_token_type(TokenType::标识符) {
                let mut p = vec![self.parse_type_literal()?];
                while !self.match_token_value(&[")"]) {
                    self.consume_token_value(&[","], "");
                    p.push(self.parse_type_literal()?);
                }
                Ok(TypeLiteral::Tuple(p))
            } else {
                Err("不合法的类型标注".to_string())
            }
        } else if self.match_token_type(TokenType::标识符) {
            let name = self.get_previous_token().value.clone();
            if self.match_token_value(&["<"]) {
                let mut type_arg = vec![self.parse_type_literal()?];
                while !self.match_token_value(&[">"]) {
                    self.consume_token_value(&[","], "");
                    type_arg.push(self.parse_type_literal()?);
                }
                Ok(TypeLiteral::Generic { name, type_arg })
            } else {
                Ok(TypeLiteral::Normal(name))
            }
        } else if self.match_token_value(&["_"]) {
            Ok(TypeLiteral::Infer)
        } else {
            Err("不合法的类型标注".to_string())
        }
    }
    /// 解析 "模式"
    fn parse_pattern(&mut self) -> Result<Pattern, String> {
        if self.match_token_value(&["("]) {
            if self.match_token_value(&[")"]) {
                return Err("无意义的模式,用 `_` 代替".to_string());
            }
            let mut pattern_arr = vec![self.parse_pattern()?];
            while !self.match_token_value(&[")"]) {
                self.consume_token_value(&[","], "缺失逗号");
                if self.match_token_value(&[")"]) {
                    break;
                }
                pattern_arr.push(self.parse_pattern()?);
            }
            Ok(Pattern::TupleMatch(pattern_arr))
        } else if self.match_token_value(&["{"]) {
            if self.match_token_value(&["}"]) {
                return Err("无意义的模式,用 `_` 代替".to_string());
            }
            let mut pattern_map = HashMap::new();

            while !self.match_token_value(&["}"]) {
                self.consume_token_type(TokenType::标识符, "a");
                let name = self.get_previous_token().value.clone();
                if self.match_token_value(&[":"]) {
                    pattern_map.insert(name, self.parse_pattern()?);
                } else if self.match_token_value(&[","]) {
                    pattern_map.insert(name.clone(), Pattern::Id(name));
                }
            }
            Ok(Pattern::ObjectMatch(pattern_map))
        } else if self.match_token_type(TokenType::标识符) {
            let name = self.get_previous_token().value.clone();
            if self.match_token_value(&["::"]) {
                self.consume_token_type(TokenType::标识符, "缺失枚举成员名");
                let member_name = self.get_previous_token().value.clone();
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
                        self.consume_token_value(&[","], "缺失逗号");
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
            crate::debug!("{:?}", self.now_token);
            todo!()
        }
    }
    /// 解析表达式的入口
    fn parse_expr(&mut self) -> Result<Expr, String> {
        self.assignment()
    }
    /// 乘除表达式
    fn factor(&mut self) -> Result<Expr, String> {
        let mut expr: Expr = self.primary_expr()?;
        let column = self.get_previous_token().column;
        let line = self.get_previous_token().line;
        while self.match_token_value(&["*", "/"]) {
            let operator = (self.get_previous_token().value).to_string();
            let right = self.primary_expr()?;
            expr = Expr::二元表达式 {
                operator,
                left: Box::new(expr),
                right: Box::new(right),
                line,
                column,
            };
        }
        Ok(expr)
    }
    /// 加减表达式
    fn term(&mut self) -> Result<Expr, String> {
        // 加减法优先级很低
        // 首先调用乘法解析,之后的表达式都会递归解析
        let mut expr = self.factor()?;
        let column = self.get_previous_token().column;
        let line = self.get_previous_token().line;
        // 解析完后轮到自己
        while self.match_token_value(&["+", "-"]) {
            // match消耗了运算符,所以用 getPreviousToken 拿回来
            let operator = (self.get_previous_token().value).to_string();
            // 右边再次递归解析
            let right = self.factor()?;
            // 它本身是新表达式的左侧
            // 现在可以生成一颗表达式树了
            expr = Expr::二元表达式 {
                operator,
                left: Box::new(expr),
                right: Box::new(right),
                line,
                column,
            };
            // 谁不优先谁就在函数调用栈的上层
            // 然后立即调用比自己优先一层的解析函数
        }
        Ok(expr)
    }
    /// 赋值表达式
    fn assignment(&mut self) -> Result<Expr, String> {
        let mut expr = self.term();
        let column = self.get_previous_token().column;
        let line = self.get_previous_token().line;
        while self.match_token_value(&["="]) {
            let right = self.assignment();
            let operator = self.get_previous_token().value.to_string();
            expr = Ok(Expr::赋值表达式 {
                operator,
                left: Box::new(expr?),
                right: Box::new(right?),
                line,
                column,
            });
        }
        expr
    }
    /// 块表达式
    fn block_expr(&mut self) -> Result<Expr, String> {
        let Token {
            mut line,
            mut column,
            ..
        } = self.get_previous_token();
        let mut body = vec![];
        loop {
            body.push(self.parse_stat()?);
            if self.match_token_value(&["}"]) {
                break;
            }
        }
        Ok(Expr::块表达式 { body, line, column })
    }
    /// ### 优先级最高,基本表达式
    fn primary_expr(&mut self) -> Result<Expr, String> {
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
            Ok(Expr::字面量 {
                value: value.to_string(),
                column: *column,
                line: *line,
            })
        } else if self.match_token_type(TokenType::标识符) {
            let Token {
                value,
                column,
                line,
                ..
            } = &self.get_previous_token();
            Ok(Expr::标识符 {
                value: value.to_string(),
                column: *column,
                line: *line,
            })
        } else if self.match_token_type(TokenType::左括号) {
            let column = self.get_previous_token().column;
            let line = self.get_previous_token().line;
            if self.match_token_type(TokenType::右括号) {
                return Ok(Expr::单元 { line, column });
            }
            let expr = self.parse_expr()?;
            if self.match_token_value(&[","]) {
                let mut tuple = vec![expr];
                while !self.match_token_type(TokenType::右括号) {
                    tuple.push(self.parse_expr()?);
                    self.match_token_value(&[","]);
                }
                Ok(Expr::元组 {
                    elements: tuple,
                    line,
                    column,
                })
            } else {
                self.consume_token_type(TokenType::右括号, "缺失右括号");
                Ok(expr)
            }
        } else if self.check_token_value(&["{"]) {
            let Token { line, column, .. } = self.now_token;
            self.match_token_value(&["{"]);
            if self.match_token_value(&["}"]) {
                return Ok(Expr::块表达式 {
                    body: vec![],
                    line,
                    column,
                });
            };
            let Tokenizer {
                line,
                column,
                point,
                ..
            } = self.tokenizer;
            if self.check_token_type(TokenType::标识符) {
                self.match_token_type(TokenType::标识符);

                if self.match_token_value(&[":"]) {
                    todo!()
                } else if self.match_token_value(&[","]) {
                    while !self.match_token_value(&["}"]) {
                        self.match_token_type(TokenType::标识符);
                    }
                    todo!()
                } else if self.match_token_value(&["}"]) {
                    todo!()
                } else {
                    self.tokenizer.line = line;
                    self.tokenizer.column = column;
                    self.tokenizer.point = point;
                    self.block_expr()
                }
            } else {
                self.block_expr()
            }
        } else {
            todo!()
        }
    }
}
