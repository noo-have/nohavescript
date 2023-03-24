//! 解析器模块
use crate::tokenizer::{Token, TokenType, Tokenizer};

#[cfg_attr(debug_assertions, derive(Debug))]
pub enum Stat {
    表达式语句 { expression: Expr },
}
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum Expr {
    字面量 {
        value: String,
        column: i32,
        line: i32,
    },
    元组 {
        elements: Box<Vec<Expr>>,
    },
    二元表达式 {
        operator: Token,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}
pub enum Node {}
pub struct Parser {
    pub line: i32,
    pub column: i32,
    pub tokenizer: Tokenizer,
    pub now_token: Token,
    pub AST: Vec<Stat>,
    pub tokens: Vec<Token>,
}
impl Parser {
    pub fn remake(mut self, str: &str) -> Self {
        self = Parser::new(str);
        self
    }
    /// 启动解析
    pub fn start(&mut self) {
        while let Some(token) = self.get_next_token() {
            self.now_token = token;
            self.line = self.now_token.line;
            self.column = self.now_token.column;
            self.parse();
        }
        crate::debug!("{:#?}", self.AST);
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
            AST: vec![],
            tokens: vec![],
        }
    }
    /// 解析入口
    pub fn parse(&mut self) {
        crate::debug!("{:#?}", self.now_token,);
        let stat = self.parse_stat();
        self.AST.push(stat);
    }
    /// 拿取下一个token,并且将parser的各项数据与新token对齐
    fn eat_token(&mut self) {
        self.tokens.push(self.now_token.clone());
        self.now_token = self.get_next_token().unwrap_or_else(|| Token {
            token_type: TokenType::结束,
            value: " ".to_string(),
            line: 0,
            column: 0,
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
    /// 获取下一个token,等价于tokenizer.next()
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
    fn get_previous_token(&self) -> Token {
        self.tokens.last().unwrap().clone()
    }
    /// 行为与match_token_value一致,不同之处在于如果匹配不成功是一个错误
    fn consume_token_value(&mut self, value_arr: &[&str]) {
        if value_arr.iter().any(|c| *c == self.now_token.value) {
            self.eat_token();
        } else {
            todo!()
        }
    }
    /// 行为与match_token_type一致,不同之处在于如果匹配不成功是一个错误
    fn consume_token_type(&mut self, token_type: TokenType) {
        if token_type == self.now_token.token_type {
            self.eat_token();
        } else {
            // 错误
            todo!()
        }
    }
    /// 解析语句的入口
    pub fn parse_stat(&mut self) -> Stat {
        if self.match_token_value(&["let"]) {
            // 变量声明语句
            todo!()
        } else {
            // 以上条件都不满足说明是表达式语句
            Stat::表达式语句 {
                expression: self.parse_expr(),
            }
        }
    }
    /// 解析表达式的入口
    pub fn parse_expr(&mut self) -> Expr {
        self.term()
    }
    fn factor(&mut self) -> Expr {
        let mut expr: Expr = self.primary_expr();
        while self.match_token_value(&["*", "/"]) {
            let operator = self.get_previous_token();
            let right = self.primary_expr();
            expr = Expr::二元表达式 {
                operator,
                left: Box::new(expr),
                right: Box::new(right),
            };
        }
        expr
    }
    fn term(&mut self) -> Expr {
        // 加减法优先级很低
        // 首先调用乘法解析,之后的表达式都会递归解析
        let mut expr = self.factor();
        // 解析完后轮到自己
        while self.match_token_value(&["+", "-"]) {
            // match消耗了运算符,所以用 getPreviousToken 拿回来
            let operator = self.get_previous_token();
            // 右边再次递归解析
            let right = self.factor();
            // 它本身是新表达式的左侧
            // 现在可以生成一颗表达式树了
            expr = Expr::二元表达式 {
                operator,
                left: Box::new(expr),
                right: Box::new(right),
            };
            // 谁不优先谁就在函数调用栈的上层
            // 然后立即调用比自己优先一层的解析函数
        }
        expr
    }
    /// ### 优先级最高,基本表达式
    pub fn primary_expr(&mut self) -> Expr {
        if self.match_token_type(TokenType::数字字面量)
            || self.match_token_type(TokenType::布尔值)
            || self.match_token_type(TokenType::标识符)
            || self.match_token_type(TokenType::字符串)
        {
            let Token {
                value,
                column,
                line,
                ..
            } = &self.get_previous_token();
            Expr::字面量 {
                value: value.to_string(),
                column: *column,
                line: *line,
            }
        } else if self.match_token_type(TokenType::左括号) {
            let expr = self.parse_expr();
            if self.match_token_value(&[","]) {
                let mut tuple = vec![expr];
                while !self.match_token_type(TokenType::右括号) {
                    let expr = self.parse_expr();
                    println!("{:?}", expr);
                    tuple.push(expr);
                    self.match_token_value(&[","]);
                }
                Expr::元组 {
                    elements: Box::new(tuple),
                }
            } else {
                self.consume_token_type(TokenType::右括号);
                expr
            }
        } else {
            todo!()
        }
    }
}
