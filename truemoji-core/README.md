# Truemoji Core

This library contains the core specification and implementation of the Truemoji language, which is a language for describing [propositional logic](https://en.wikipedia.org/wiki/Propositional_calculus). TLDR: I replaced the propositional logic operators with emojis! 

This project is greatly inspired by [this truth table generator](https://web.stanford.edu/class/cs103/tools/truth-table-tool/), along with help from the following resources:

* <https://adriann.github.io/rust_parser.html>
* <https://createlang.rs/01_calculator/ast.html>
* <https://pages.cs.wisc.edu/~fischer/cs536.s08/course.hold/html/NOTES/3.CFG.html#exp>

## Truemoji Language Specification

```markdown
<iff> := <implies> 🤝 <iff> | <implies>
<implies> := <or> 👉 <iff> | <or>
<or> := <and> 🙌 <or> | <and>
<and> := <not> 👏 <and> | <not>
<not> := 🚫 <formula> | <formula>
<formula> := [A-Za-z0-9] | 👍 | 👎 | 😮 <iff> 😶
```

## Crate Contents

This crate implements a lexer, parser, and abstract syntax tree (AST) for the Truemoji language.
The truth values for ASTs can be evaluated as long as each formula has an boolean value associated with it.
