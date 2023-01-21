# Truemoji Core

This library contains the core specification and implementation of the Truemoji language, which is a language for describing [propositional logic](https://en.wikipedia.org/wiki/Propositional_calculus). TLDR: I replaced the propositional logic operators with emojis! 

## Truemoji Language Specification

```markdown
<iff> := <implies> 🤝 <iff> | <implies>
<implies> := <or> 👉 <iff> | <or>
<or> := <and> 🙌 <or> | <and>
<and> := <not> 👏 <and> | <not>
<not> := 🚫 <formula> | <formula>
<formula> := [A-Za-z0-9] | 👍 | 👎 | 😮 <iff> 😶
```

## Resources Consulted

* <https://adriann.github.io/rust_parser.html>
* <https://createlang.rs/01_calculator/ast.html>
* <https://pages.cs.wisc.edu/~fischer/cs536.s08/course.hold/html/NOTES/3.CFG.html#exp>

## Crate Contents

This crate implements a lexer, parser, and abstract syntax tree (AST) for the Truemoji language.
The truth values for ASTs can be evaluated as long as each formula has an boolean value associated with it.
