# lox implementations

Implementations of the lox language from [Crafting Interpreters](https://craftinginterpreters.com/)

`treewalk/` is the treewalk interpreter implementation (implemented up to functions)
`bytecode/` is the bytecode interpreter implementation (WIP)

Both are written and zig and can be built via

```
$ cd bytecode
$ zig build
```

Try an example:

```
$ cd bytecode
$ zig build run -- ./examples/breakfast.lox
```

Use zig 0.11.0.
