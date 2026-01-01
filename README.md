# Zex

A dynamically typed programming language built in C.

## Build

```bash
cmake -B build && cmake --build build
```

## Run

```bash
./build/zex examples/hello.zex
```

## Syntax

### Variables
```
var name = "Zex"
var age = 1
var pi = 3.14159
```

### Functions
```
fun greet(name) {
    println("Hello, " + name + "!")
}

fun factorial(n) {
    if n <= 1 {
        return 1
    }
    return n * factorial(n - 1)
}
```

### Classes
```
class Animal {
    fun init(self, name) {
        self.name = name
    }
    
    fun speak(self) {
        println(self.name + " makes a sound")
    }
}

class Dog < Animal {
    fun speak(self) {
        println(self.name + " says: Woof!")
    }
}

var dog = Dog("Rex")
dog.speak()
```

### Control Flow
```
if x > 10 {
    println("big")
} else {
    println("small")
}

var i = 0
while i < 5 {
    println(i)
    i += 1
}
```

## Debug

```bash
./build/zex script.zex --dump-ast
./build/zex script.zex --dump-bytecode
```

## License

MIT