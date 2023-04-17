//! 各模块公用的宏
#[macro_export]
/// 将参数原封不动的传入println宏
/// 但在build时,该语句不会被编译
macro_rules! debug {
    ($($a:expr$(,)?)*) => {
        #[cfg(debug_assertions)]
        println!($($a,)*);
    };
}
/// add!(expr) 等效于 ++expr
#[macro_export]
macro_rules! add {
    ($a:expr) => {{
        $a += 1;
        $a
    }};
}
/// do_while(block_expr,expr) 等效于
///
/// do block_expr while expr block_expr
#[macro_export]
macro_rules! do_while {
    ($b:block,$a:expr) => {
        $b;
        while $a $b;
    };
}
