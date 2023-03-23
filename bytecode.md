# 字节码设计

# 指令
指令用一个字节来表示,例如 0a -> Add指令,一个字节可以容纳256种不同的指令,完全足够

# 反汇编
字节码是长这样的
00 02 00 00 00 00 00 00
23 33 44 22 00 00 00 00
完全不可读,需要"反汇编"来转换成可读的形式,主要用于debug

# 

let a = 2
let c = () => {
  let s = 5
  let x = () => {s}
  return x
}
c(3)

cst 2   [2]
set a   []
fnBody  [fn]
 cst 5  [fn,5]
 set s  [fn]
 fnBody [fn,fn]
  get s [fn,fn,s]
  pop   [fn,fn]
 set x  [fn]
 get x  [fn,x]
 ret    [fn]
set c   []
cst 3   [3]
call c  []
// 