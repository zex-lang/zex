# Zex

<p align="center">
  <img src="Docs/icon.png" alt="Zex" width="180">
</p>

<p align="center">
  <strong>A fast, expressive, dynamically-typed programming language</strong>
</p>

<p align="center">
  <a href="#features">Features</a> •
  <a href="#installation">Installation</a> •
  <a href="#quick-start">Quick Start</a> •
  <a href="#documentation">Documentation</a> •
  <a href="#license">License</a>
</p>

## Features

- **Simple syntax** — Clean, readable code with minimal boilerplate
- **First-class functions** — Closures, higher-order functions, and lambdas
- **Object-oriented** — Classes with inheritance and methods
- **Fast execution** — Register-based bytecode VM written in C

## Installation

```bash
git clone https://github.com/zex-lang/zex.git
cd zex
cmake -B build && cmake --build build
```

## Quick Start

```bash
./build/zex hello.zex
```

```zex
# hello.zex
var name = "World"
println("Hello, " + name + "!")

# Functions with implicit returns
fun square(x) { x * x }

# Closures
var numbers = [1, 2, 3, 4, 5]
var doubled = numbers.map(|n| n * 2)

# Classes
class Greeter {
    private name
    
    public Greeter(name) {
        self.name = name
    }
    
    public greet() {
        println("Hello, " + self.name + "!")
    }
}

Greeter("Zex").greet()
```

## Documentation

| Topic | Description |
|-------|-------------|
| [Language Reference](Docs/language-reference.md) | Complete guide to Zex syntax, data types, control flow, functions, classes, and more |
| [Built-in Functions](Docs/built-in-functions.md) | Documentation for global functions like `println` and `type` |
| [Standard Types](Docs/standard-types.md) | Built-in type classes (int, float, string, array, tuple) and their methods |


### CLI Options

```bash
zex script.zex              # Run a script
zex -c script.zex           # Compile to bytecode
zex --dump-ast script.zex   # Show AST
zex --dump-bytecode script.zex  # Show bytecode
```

## License

MIT © 2026