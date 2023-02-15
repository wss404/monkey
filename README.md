# monkey

本项目设计了一种全新的语言：`money`，基于它的特性来编写它的解释器和编译器。
`monkey`具有以下特性：
- 整型
- 布尔型
- 字符串
- 数组
- 哈希表
- 前缀运算符、中缀运算符、索引运算符
- 条件表达式
- 全局变量绑定和局部变量绑定
- 头等函数
- return表达式
- 闭包

### 解释器
按照从源代码到输出到顺序，解释器包含以下部分：
- 词法分析器
- 语法分析器
- 抽象语法树
- 内部对象系统
- 求值器

### 编译器
这是实现解释器之后的升级内容：将基于遍历语法树与即时求值的解释器升级成字节码编译器以及一个用来执行字节码的虚拟机。