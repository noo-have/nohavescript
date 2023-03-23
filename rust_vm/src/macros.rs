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
