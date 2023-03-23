//! 虚拟机模块
use std::{cell::RefCell, collections::HashMap, rc::Rc};

/// 简化匹配NsValue
macro_rules! if_is_type_do {
    ($($a:expr,$b:ident,$bind:ident)*;$c:block) => {
        if let ($(NsValue::$b($bind),)*) = ($($a,)*) $c
    };
}
/// 简化
/// ```rs
/// Rc::new(RefCell::new(v)) => rc_refCell!(v)
/// ```
macro_rules! rc_refCell {
    ($a:expr) => {
        Rc::new(RefCell::new($a))
    };
}
/// 字节码
pub enum ByteCode {
    /// 函数的返回
    Ret,
    /// 函数调用
    Call(String),
    /// 栈顶出栈,设置变量值
    SetVariant(String),
    /// 无条件跳转
    Go(i32),
    /// 栈顶出栈,true跳第一个参数,false跳第二个
    If(i32, i32),
    /// 从作用域里读取变量,入栈
    GetVariant(String),
    /// 声明全局变量
    GlobalDef(String),
    /// 分配一个Ns_string
    Str(String),
    /// 数组指针入栈,然后分配内存
    Arr,
    /// 栈顶出栈,通过NS_arr指针push进数组
    Idx,
    /// Ns_obj指针入栈,然后分配内存
    Obj,
    /// 栈顶出栈,通过Ns_obj指针设置Key-value
    Key(String),
    /// 让一个值入栈,布尔或数字
    Cst(NsValue),
    // 运算
    /// 加
    Add,
    /// 减
    Sub,
    /// 乘
    Mul,
    /// 除
    Div,
    /// 取负
    Neg,
    /// 或 ||
    Or,
    /// 非 !
    Not,
    /// 与 &&
    And,
    /// 等于 ==
    Eq,
    /// 不等于  !=
    Ne,
}
type God<T> = Rc<RefCell<T>>;
/// Ns端的对象
type NsObj = HashMap<String, NsValue>;
#[derive(Default)]
pub struct Function {
    arg_count: i32,
    frame: Vec<HashMap<String, NsValue>>,
    bytecode: Vec<ByteCode>,
}
/// Ns端的值
pub enum NsValue {
    Bool(bool),
    Number(f64),
    String(God<String>),
    Array(God<Vec<NsValue>>),
    Obj(God<NsObj>),
    Fn(God<Function>),
}
#[cfg(debug_assertions)]
impl std::fmt::Debug for NsValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            NsValue::Bool(value) => f.write_fmt(format_args!("{}", value)),
            NsValue::Number(value) => f.write_fmt(format_args!("{}", value)),
            NsValue::String(value) => f.write_fmt(format_args!("{}", value.borrow())),
            NsValue::Array(value) => f.debug_list().entries(value.borrow().iter()).finish(),
            NsValue::Obj(value) => f.debug_map().entries(value.borrow().iter()).finish(),
            NsValue::Fn(value) => todo!(),
        }
    }
}
impl Clone for NsValue {
    /// 对NsValue廉价的复制
    /// 1. 如果NsValue是布尔或数字,直接复制
    /// 2. 如果NsValue是字符串,数组,对象这种堆数据,复制的是对堆上数据的强引用指针
    fn clone(&self) -> Self {
        match self {
            NsValue::Bool(v) => NsValue::Bool(*v),
            NsValue::Number(v) => NsValue::Number(*v),
            NsValue::String(v) => NsValue::String(v.clone()),
            NsValue::Array(v) => NsValue::Array(v.clone()),
            NsValue::Obj(v) => NsValue::Obj(v.clone()),
            NsValue::Fn(v) => NsValue::Fn(v.clone()),
        }
    }
}
impl PartialEq for NsValue {
    fn eq(&self, other: &Self) -> bool {
        match self {
            NsValue::Bool(v1) => {
                if let NsValue::Bool(v2) = other {
                    v1 == v2
                } else {
                    false
                }
            }
            NsValue::Number(v1) => {
                if let NsValue::Number(v2) = other {
                    v1 == v2
                } else {
                    false
                }
            }
            NsValue::String(v1) => {
                if let NsValue::String(v2) = other {
                    Rc::eq(v1, v2)
                } else {
                    false
                }
            }
            NsValue::Array(v1) => {
                if let NsValue::Array(v2) = other {
                    Rc::eq(v1, v2)
                } else {
                    false
                }
            }
            NsValue::Obj(v1) => {
                if let NsValue::Obj(v2) = other {
                    Rc::eq(v1, v2)
                } else {
                    false
                }
            }
            NsValue::Fn(v1) => {
                if let NsValue::Fn(v2) = other {
                    Rc::ptr_eq(v1, v2)
                } else {
                    false
                }
            }
        }
    }
}
impl NsValue {
    /// 获取原始指针
    ///
    /// 用处1: 加速变量写入.
    /// n次哈希表插入操作被优化为1次插入+n-1次解引用指针操作
    fn as_ptr(&mut self) -> *mut NsValue {
        self as *mut NsValue
    }
}
#[derive(Default)]
/// 虚拟机
pub struct VM {
    /// 栈
    pub stack: Vec<NsValue>,
    /// 当前读到第几个指令
    pub ip: i32,
    /// 全局变量表
    pub global_variable: HashMap<String, NsValue>,
    ///
    pub scope: Vec<HashMap<String, NsValue>>,
}
impl VM {
    pub fn new() -> VM {
        Default::default()
    }
    fn read_bytecode(&mut self) {
        self.ip += 1
    }
    fn push(&mut self, value: NsValue) {
        self.stack.push(value)
    }
    fn pop(&mut self) -> NsValue {
        // safe声明
        // 在前端生成正确的字节码的情况下,这里实际上是不可能出错的
        self.stack.pop().unwrap()
    }
    pub fn run(&mut self, bytecode_list: &[ByteCode]) {
        self.parse_bytecode(bytecode_list)
    }
    fn parse_bytecode(&mut self, bytecode_list: &[ByteCode]) {
        use ByteCode::*;
        let mut p = 0;
        // let mut catch: Option<*mut NsValue> = None;
        while let Some(bytecode) = bytecode_list.get(self.ip as usize) {
            /// 作用域深度
            let mut scope_depth = 0;
            /// 局部变量数
            let local_count = 0;
            self.read_bytecode();
            p += 1;
            if p == 10000000 {
                break;
            }
            match bytecode {
                // 二元运算的优化:
                // 原本出两次栈顶,做运算再写入
                // 改为只需出一次栈顶,在栈顶拿取可变引用再写入即可
                Add => {
                    let b = self.pop();
                    let a = self.stack.last_mut().unwrap();
                    if let (NsValue::Number(v1), NsValue::Number(v2)) = (&a, &b) {
                        *a = NsValue::Number(v1 + v2);
                    }
                }
                Sub => {
                    let b = self.pop();
                    let a = self.pop();
                    if_is_type_do!(
                        a,Number,v1
                        b,Number,v2;
                        {
                            self.push(NsValue::Number(v1-v2))
                        }
                    )
                }
                Mul => {
                    let b = self.pop();
                    let a = self.pop();
                    if_is_type_do!(
                        a,Number,v1
                        b,Number,v2;
                        {
                            self.push(NsValue::Number(v1*v2))
                        }
                    )
                }
                Div => {
                    let b = self.pop();
                    let a = self.pop();
                    if_is_type_do!(
                        a,Number,v1
                        b,Number,v2;
                        {
                            self.push(NsValue::Number(v1/v2))
                        }
                    )
                }
                Neg => {
                    let a = self.pop();
                    let b = a.clone();
                    if_is_type_do!(
                        a,Number,v1;
                        {
                            self.push(NsValue::Number(v1));
                        }
                    );
                    if_is_type_do!(
                        b,Bool,v1;
                        {
                            self.push(NsValue::Bool(v1));
                        }
                    );
                }
                Or => {
                    let b = self.pop();
                    let a = self.pop();
                    if_is_type_do!(
                        a,Bool,v1
                        b,Bool,v2;
                        {
                            self.push(NsValue::Bool(v1||v2))
                        }
                    )
                }
                Not => {
                    let a = self.pop();
                    if_is_type_do!(
                        a,Bool,v1;
                        {
                            self.push(NsValue::Bool(!v1))
                        }
                    )
                }
                And => {
                    let b = self.pop();
                    let a = self.pop();
                    if_is_type_do!(
                        a,Bool,v1
                        b,Bool,v2;
                        {
                            self.push(NsValue::Bool(v1&&v2))
                        }
                    )
                }
                GlobalDef(variable) => {
                    let value = self.pop();
                    // 允许全局变量重定义,所以不做检查
                    // 方便repl这种环境
                    self.global_variable.insert(variable.to_string(), value);
                }
                Cst(value) => match value {
                    e @ NsValue::Bool(_) => self.push(e.clone()),
                    e @ NsValue::Number(_) => self.push(e.clone()),
                    _ => {}
                },
                Str(str) => {
                    // TODO 性能优化!!!
                    self.push(NsValue::String(rc_refCell!(str.to_string())))
                }
                Obj => {
                    // TODO 性能优化!!!
                    // ? 考虑换成裸指针而不是Rc指针
                    // 分配内存
                    let map = rc_refCell!(HashMap::default());
                    // 指针入栈
                    self.push(NsValue::Obj(map));
                }
                Key(str) => {
                    // TODO 性能优化!!!
                    // 弹出一个值
                    let value = self.pop();
                    // obj的指针
                    let obj_ptr = self.stack.last_mut().unwrap();
                    // safe声明
                    // 绝对不可能是obj指针以外的值,给出以下证明:
                    // 以obj指令开头,以下指令总是成对出现
                    // obj           obj指针入栈
                    // (cst value    value入栈
                    // Key keyName)* value出栈 栈平衡
                    // 因此上文的obj_ptr一定是obj指针
                    if_is_type_do!(
                        obj_ptr,Obj,ptr;
                        {
                            ptr.borrow_mut().insert(str.to_string(), value);
                        }
                    );
                }
                Arr => {
                    // TODO 性能优化!!!
                    // 分配内存
                    let arr = rc_refCell!(vec![]);
                    // 指针入栈
                    self.push(NsValue::Array(arr));
                }
                Idx => {
                    // TODO 性能优化!!!
                    let value = self.pop();
                    let arr_ptr = self.stack.last_mut().unwrap();
                    if_is_type_do!(
                        arr_ptr,Array,ptr;
                        {
                            ptr.borrow_mut().push(value);
                        }
                    )
                    // safe声明
                    // 绝对不可能是arr指针以外的值,给出以下证明:
                    // 以arr指令开头,以下指令总是成对出现
                    // arr           arr指针入栈
                    // (cst value    value入栈
                    // idx)*         value出栈 栈平衡
                    // 因此上文的arr_ptr一定是arr指针
                }
                GetVariant(variant) => {
                    // TODO 性能优化!!!
                    // 编程的很大部分都与局部变量的读写有关
                    // 所以局部变量的读写必须够快
                    let cc = &self.scope;
                    let mut r = true;

                    // 遍历作用域
                    for s in cc.iter().rev() {
                        if let Some(value) = s.get(variant) {
                            self.stack.push(value.clone());
                            r = !r;
                            break;
                        }
                    }
                    // 说明作用域里没找到
                    if r {
                        if let Some(value) = self.global_variable.get(variant) {
                            self.stack.push(value.clone());
                        }
                    }
                    //  else {
                    //     panic!("变量 {}未定义", &variant)
                    // }
                }
                SetVariant(variant) => {
                    // TODO 性能优化！！！
                    // 哈希表插入优化成裸指针解引用
                    if scope_depth == 0 {
                        // 全局作用域
                        *(self.global_variable.get_mut(variant).unwrap()) = self.pop();
                    } else {
                    }
                }

                Call(fn_name) => {
                    let cc = &self.scope;
                    let mut r = None;
                    for s in cc.iter().rev() {
                        if let Some(value) = s.get(fn_name) {
                            r = Some(value.clone());
                            break;
                        }
                    }
                    if r.is_none() {
                        if let Some(value) = self.global_variable.get(fn_name) {
                            r = Some(value.clone());
                        }
                    }
                    if let NsValue::Fn(v) = r.unwrap() {
                        let mut c = vec![];
                        if v.borrow().arg_count > 0 {
                            for _ in 0..v.borrow().arg_count {
                                c.push(self.pop());
                            }
                            // self.parse_bytecode(&v.borrow().bytecode);
                        }
                    }
                    // let p = NsFn::default();
                    // self.parse_bytecode(p.bytecode);
                }
                If(ip1, ip2) => {
                    let a = self.pop();
                    if_is_type_do!(
                        a,Bool,_a;
                        {
                            if _a {self.ip = *ip1;}else {self.ip = *ip2;}
                        }
                    )
                }
                Eq => {
                    let b = self.pop();
                    let a = self.pop();
                    if a == b {
                        self.push(NsValue::Bool(true));
                    } else {
                        self.push(NsValue::Bool(false));
                    }
                }
                Go(ip) => {
                    self.ip = *ip;
                }
                Ne => {
                    let b = self.pop();
                    let a = self.pop();
                    if_is_type_do!(
                        a,Number,v1
                        b,Number,v2;
                        {
                            if v1 == v2 {
                                self.push(NsValue::Bool(false));
                            } else {
                                self.push(NsValue::Bool(true));
                            }
                        }
                    );
                }
                Ret => todo!(),
            };
        }
    }
}
