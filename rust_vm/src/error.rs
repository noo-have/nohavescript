//! 编译器报错
#[derive(Debug)]
enum ErrorKind {
    /// 分析语句时发生的错误
    Analysis,
    /// 单纯的语法错误
    Syntax,
    /// 运行时错误
    Runtime,
}
pub struct ParseError {
    message: String,
    line: u32,
    column: u32,
    kind: ErrorKind,
}
impl std::fmt::Debug for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{} 发生在 {}:{}", self.message, self.line, self.column)
    }
}
/// 运行时错误
pub fn runtime_error(message: &str) {
    todo!()
    // panic!("运行时错误: {}", message)
}
/// syntax_error
pub fn syntax_error(message: &str, line: u32, column: u32) -> ParseError {
    ParseError {
        message: message.to_string(),
        line,
        column,
        kind: ErrorKind::Syntax,
    }
}
/// AnalysisError
pub fn analysis_error(message: &str, line: u32, column: u32) -> ParseError {
    ParseError {
        message: message.to_string(),
        line,
        column,
        kind: ErrorKind::Analysis,
    }
}
