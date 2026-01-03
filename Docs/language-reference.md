# Zex Language Reference

A comprehensive guide to the Zex programming language syntax and semantics.

## Table of Contents

- [Hello World](#hello-world)
- [Comments](#comments)
- [Variables](#variables)
- [Data Types](#data-types)
- [Operators](#operators)
- [Control Flow](#control-flow)
- [Functions](#functions)
- [Closures](#closures)
- [Classes](#classes)
- [Inheritance](#inheritance)
- [Exception Handling](#exception-handling)
- [Naming Conventions](#naming-conventions)

---

## Hello World

```zex
println("Hello, World!")
```

## Comments

Zex uses `#` for single-line comments:

```zex
# This is a comment
var x = 10  # Inline comment
```

---

## Variables

Variables are declared with the `var` keyword. Zex is dynamically typed.

```zex
var name = "Zex"
var age = 1
var pi = 3.14159
var active = true
var nothing = null
```

### Reassignment

```zex
var x = 10
x = 20  # Reassign to new value
```

---

## Data Types

Zex has the following built-in types:

| Type | Description | Example |
|------|-------------|---------|
| `int` | 64-bit integer | `42`, `-17`, `0` |
| `float` | 64-bit floating point | `3.14`, `-0.5`, `1.0` |
| `string` | UTF-8 text | `"hello"`, `'world'` |
| `bool` | Boolean | `true`, `false` |
| `null` | Null value | `null` |
| `array` | Dynamic array | `[1, 2, 3]` |
| `tuple` | Immutable sequence | `(1, 2, 3)` |

### Strings

Strings can use single or double quotes and support escape sequences:

```zex
var s1 = "Double quotes"
var s2 = 'Single quotes'
var s3 = "Line 1\nLine 2"
var s4 = "Tab:\there"
var s5 = "Unicode: \u2764"      # ❤
var s6 = "Emoji: \u{1F600}"     # 😀
```

**Escape Sequences:**
- `\n` - Newline
- `\t` - Tab
- `\r` - Carriage return
- `\\` - Backslash
- `\"` - Double quote
- `\'` - Single quote
- `\0` - Null character
- `\xNN` - Hex byte
- `\uNNNN` - Unicode (4 hex digits)
- `\u{N...}` - Unicode (1-6 hex digits)

### Arrays

Arrays are dynamic, zero-indexed collections:

```zex
var numbers = [1, 2, 3, 4, 5]
var mixed = [1, "hello", true, 3.14]

# Indexing
println(numbers[0])    # 1
println(numbers[-1])   # 5 (last element)

# Modification
numbers[0] = 100
```

### Tuples

Tuples are immutable sequences accessed by index:

```zex
var point = (10, 20)
var x = point.0        # 10
var y = point.1        # 20

# Single-element tuple (needs trailing comma)
var single = (42,)

# Empty tuple
var empty = ()

# Nested tuple access
var nested = ((1, 2), (3, 4))
println(nested.1.1)    # 4
```

### Type Conversion

Built-in type classes can be called as constructors:

```zex
var x = int(3.14)      # 3
var y = float(42)      # 42.0
var s = string(100)    # "100"
var b = bool(1)        # true
```

### Type Introspection

```zex
println(type(42))      # <class int>
println(type(3.14))    # <class float>
println(type("hi"))    # <class string>
```

---

## Operators

### Arithmetic Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `+` | Addition | `a + b` |
| `-` | Subtraction | `a - b` |
| `*` | Multiplication | `a * b` |
| `/` | Division | `a / b` |
| `%` | Modulo | `a % b` |
| `-` | Negation (unary) | `-a` |
| `+` | Positive (unary) | `+a` |

### Comparison Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `==` | Equal | `a == b` |
| `!=` | Not equal | `a != b` |
| `<` | Less than | `a < b` |
| `<=` | Less or equal | `a <= b` |
| `>` | Greater than | `a > b` |
| `>=` | Greater or equal | `a >= b` |

### Logical Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `&&` | Logical AND | `a && b` |
| `\|\|` | Logical OR | `a \|\| b` |
| `!` | Logical NOT | `!a` |

### Compound Assignment Operators

| Operator | Equivalent |
|----------|------------|
| `+=` | `a = a + b` |
| `-=` | `a = a - b` |
| `*=` | `a = a * b` |
| `/=` | `a = a / b` |
| `%=` | `a = a % b` |

---

## Control Flow

### If / Else

```zex
var score = 85

if score >= 90 {
    println("Grade: A")
} else if score >= 80 {
    println("Grade: B")
} else if score >= 70 {
    println("Grade: C")
} else {
    println("Grade: F")
}
```

### While Loop

```zex
var i = 0
while i < 5 {
    println("i = " + i)
    i += 1
}
```

### Do-While Loop

Executes at least once:

```zex
var j = 0
do {
    println("j = " + j)
    j += 1
} while j < 3
```

### For Loop (C-style)

```zex
for var i = 0; i < 5; i += 1 {
    println("i = " + i)
}
```

### For-In Loop

Iterate over arrays or strings:

```zex
var fruits = ["apple", "banana", "cherry"]
for var fruit in fruits {
    println("Fruit: " + fruit)
}

# Iterate over string characters
for var c in "Zex" {
    println(c)
}
```

### Break and Continue

```zex
# Break - exit loop early
while true {
    if condition {
        break
    }
}

# Break with value (like Rust)
var result = while true {
    if found {
        break "found it!"
    }
}

# Continue - skip to next iteration
for var i = 0; i < 10; i += 1 {
    if i == 5 {
        continue
    }
    println(i)
}
```

---

## Functions

Functions are declared with the `fun` keyword:

```zex
fun greet(name) {
    println("Hello, " + name + "!")
}

fun add(a, b) {
    return a + b
}

# Function calls
greet("Zex")
var sum = add(10, 20)
```

### Implicit Returns (Expression-Oriented)

Zex is **expression-oriented** like Rust. The last statement in a function body is automatically returned. This works with any statement type:

**Simple expressions:**
```zex
fun square(x) { x * x }
println(square(5))  # 25
```

**If/else as expressions:**
```zex
fun grade(score) {
    if score >= 90 { "A" }
    else if score >= 80 { "B" }
    else if score >= 70 { "C" }
    else { "F" }
}
println(grade(85))  # B
```

**Nested control flow:**
```zex
fun describe(x) {
    if x > 0 {
        if x > 10 { "big positive" } 
        else { "small positive" }
    } else {
        "negative or zero"
    }
}
```

**Blocks as expressions:**
```zex
fun compute() {
    {
        var a = 10
        var b = 20
        a + b  # Block returns this
    }
}
println(compute())  # 30
```

### Recursion

```zex
fun factorial(n) {
    if n <= 1 {
        return 1
    }
    return n * factorial(n - 1)
}

println(factorial(5))  # 120
```

### Variadic Functions

Use `..` prefix for rest parameters that collect multiple arguments into a tuple:

```zex
# Rest parameter collects args into tuple
fun print_all(..items) {
    for var item in items {
        println(item)
    }
}
print_all(1, 2, 3)  # items = (1, 2, 3)

# Mixed: required + rest
fun greet(prefix, ..names) {
    for var name in names {
        println(prefix + name)
    }
}
greet("Hello ", "Alice", "Bob")
```

**Spread Arguments:**
```zex
fun sum(..nums) {
    var total = 0
    for var n in nums { total += n }
    total
}

var t = (1, 2, 3)
println(sum(..t))       # 6 - spreads tuple into args
println(sum(10, ..t))   # 16 - mixed args

var arr = [4, 5, 6]
println(sum(..arr))     # 15 - also works with arrays
```

---

## Closures

Closures (lambda functions) use the pipe syntax `|params| body`:

```zex
# Single expression closure
var double = |x| x * 2
println(double(5))  # 10

# Multi-parameter closure
var add = |a, b| a + b
println(add(3, 4))  # 7

# Block body closure
var greet = |name| {
    println("Hello, " + name + "!")
}

# Zero-parameter closure
var sayHi = || println("Hi!")
```

### Higher-Order Functions

Closures work great with array methods:

```zex
var numbers = [1, 2, 3, 4, 5]

# Map
var doubled = numbers.map(|n| n * 2)
println(doubled)  # [2, 4, 6, 8, 10]

# Filter
var evens = numbers.filter(|n| n % 2 == 0)
println(evens)  # [2, 4]

# Reduce
var sum = numbers.reduce(|acc, n| acc + n, 0)
println(sum)  # 15
```

---

## Classes

Classes are declared with the `class` keyword:

```zex
class Counter {
    fun init(self, start, step) {
        self.count = start
        self.step = step
    }
    
    fun increment(self) {
        self.count += self.step
    }
    
    fun value(self) {
        return self.count
    }
}

# Create instance (no 'new' keyword)
var c = Counter(0, 2)
c.increment()
c.increment()
println(c.value())  # 4
```

### Constructor

The `init` method is the constructor. It receives `self` as the first parameter.

### The `self` Keyword

All methods must explicitly declare `self` as their first parameter to access instance properties:

```zex
class Person {
    fun init(self, name) {
        self.name = name
    }
    
    fun greet(self) {
        println("Hi, I'm " + self.name)
    }
}
```

---

## Inheritance

Use Ruby-style syntax with `<` for inheritance:

```zex
class Animal {
    fun init(self, name) {
        self.name = name
    }
    
    fun speak(self) {
        println("Some sound")
    }
}

class Dog < Animal {
    fun speak(self) {
        println(self.name + " says: Woof!")
    }
    
    fun fetch(self) {
        println(self.name + " is fetching!")
    }
}

var dog = Dog("Rex")
dog.speak()    # Rex says: Woof!
dog.fetch()    # Rex is fetching!
```

---

## Exception Handling

Zex uses Python-style exception handling:

### Try / Except

```zex
try {
    var x = 1 / 0
} except {
    println("Caught an error!")
}
```

### Catching Specific Exceptions

```zex
try {
    var arr = [1, 2, 3]
    println(arr[10])
} except IndexError as e {
    println("Caught: " + string(e))
}
```

### Else Block

Runs if no exception occurred:

```zex
try {
    var x = 10 + 5
} except {
    println("Error occurred")
} else {
    println("No exception!")
}
```

### Finally Block

Always runs:

```zex
try {
    println("In try block")
} except {
    println("In except block")
} finally {
    println("Always runs")
}
```

### Raise Exceptions

```zex
raise ValueError("Invalid input")
```

### Exception Types

- `Exception` - Base exception class
- `ValueError` - Invalid value
- `TypeError` - Invalid type
- `IndexError` - Index out of bounds
- `ZeroDivisionError` - Division by zero
- `AttributeError` - Attribute not found
- `NameError` - Name not found
- `RuntimeError` - General runtime error

---

## Naming Conventions

Zex follows these naming conventions:

| Element | Convention | Example |
|---------|------------|---------|
| Class names | PascalCase | `Counter`, `Person`, `HttpClient` |
| Variables | camelCase | `userName`, `itemCount`, `isActive` |
| Functions | camelCase | `calculateSum`, `getUserName` |
| Methods | camelCase | `getValue`, `setName`, `isEmpty` |
| Parameters | camelCase | `firstName`, `maxValue` |

### Examples

```zex
# Class name: PascalCase
class ShoppingCart {
    fun init(self, ownerName) {
        self.ownerName = ownerName  # camelCase
        self.itemCount = 0
    }
    
    # Method name: camelCase
    fun addItem(self, itemName) {
        self.itemCount += 1
        println("Added: " + itemName)
    }
    
    fun getTotalItems(self) {
        return self.itemCount
    }
}

# Variable and function names: camelCase
var myCart = ShoppingCart("Alice")
myCart.addItem("Apple")

fun calculateDiscount(totalPrice, discountRate) {
    return totalPrice * discountRate
}
```
