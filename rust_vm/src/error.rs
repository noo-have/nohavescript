//! 编译器报错
#[derive(Debug)]
enum ErrorKind {
    AnalysisError,
    SyntaxError,
    RuntimeError,
}
#[derive(Debug)]
pub struct ParseError {
    message: String,
    line: u32,
    column: u32,
    kind: ErrorKind,
}
impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} 发生在 {}:{}", self.message, self.line, self.column)
    }
}
/// 运行时错误
pub fn runtime_error(message: &str) {
    todo!()
    // panic!("运行时错误: {}", message)
}
/// 语法错误
pub fn syntax_error(message: &str, line: u32, column: u32) -> ParseError {
    ParseError {
        message: message.to_string(),
        line,
        column,
        kind: ErrorKind::SyntaxError,
    }
}
/// 语义错误
pub fn analysis_error(message: &str, line: u32, column: u32) -> ParseError {
    ParseError {
        message: message.to_string(),
        line,
        column,
        kind: ErrorKind::AnalysisError,
    }
}
