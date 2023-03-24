//! 分词器模块
use fancy_regex::Regex;

/// 对Regex::new的简化
macro_rules! reg {
    ($a:expr) => {
        Regex::new($a).unwrap()
    };
}
pub struct Tokenizer {
    pub source_code: String,
    pub match_regexp: Vec<(Regex, TokenType)>,
    pub line: i32,
    pub point: i32,
    pub column: i32,
}
impl Iterator for Tokenizer {
    type Item = Token;
    fn next(&mut self) -> Option<Self::Item> {
        let str = &self.source_code[self.point as usize..];
        if str.is_empty() {
            return None;
        }
        let mut token = None;
        for (reg, token_type) in &self.match_regexp {
            if let Ok(Some(result)) = reg.captures(str) {
                let xx = result.get(0).unwrap().as_str();
                token = Some(Token {
                    column: self.column,
                    line: self.line,
                    value: xx.to_string(),
                    token_type: token_type.clone(),
                });
                self.eat_token(&token);
                break;
            }
        }
        token
    }
}
impl Tokenizer {
    pub fn eat_token(&mut self, token: &Option<Token>) {
        let c = token.clone().unwrap();
        match c.token_type {
            TokenType::换行符 => {
                self.line += 1;
                self.column = 1;
            }
            _ => {
                self.column += c.value.len() as i32;
            }
        };
        self.point += c.value.len() as i32;
    }
    pub fn new(code: &str) -> Self {
        Tokenizer {
            column: 1,
            line: 1,
            point: 0,
            source_code: code.to_string(),
            match_regexp: vec![
                (reg!(r"^\/\/.*"), TokenType::单行注释),
                (reg!(r"^\/\*([^*]|\*+[^*/])*\*+\/"), TokenType::块注释),
                (reg!(r"^\r?\n|(?<!\n)\r"), TokenType::换行符),
                (reg!(r"^[\x20]+"), TokenType::空格),
                (reg!(r"^(\-)?\d+(\.\d+)?"), TokenType::数字字面量),
                (
                    reg!(r"^(>=|<=|\+|->|-|\*\*|\*|\/|=>|==|=|&&|\?\?|\|\||,)"),
                    TokenType::运算符,
                ),
                (reg!(r"^(true|false)(?=\b)"), TokenType::布尔值),
                (reg!(r"^[a-zA-Z_][a-zA-Z0-9_]*"), TokenType::标识符),
                (reg!(r"^\("), TokenType::左括号),
                (reg!(r"^\)"), TokenType::右括号),
                (reg!("^\"[^\"]*\""), TokenType::字符串),
            ],
        }
    }
}
#[derive(Clone, PartialEq)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub enum TokenType {
    字符串,
    符号,
    单行注释,
    块注释,
    文档注释,
    标识符,
    数字字面量,
    关键字,
    换行符,
    空格,
    结束,
    运算符,
    布尔值,
    左括号,
    右括号,
}
#[derive(Clone)]
#[cfg_attr(debug_assertions, derive(Debug))]
pub struct Token {
    pub token_type: TokenType,
    pub value: String,
    /// 行
    pub line: i32,
    /// 列
    pub column: i32,
}
