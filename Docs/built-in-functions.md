# Zex Built-in Functions

This document describes the global built-in functions available in Zex.

---

## println

Prints one or more values to standard output followed by a newline.

### Syntax

```zex
println(value1, value2, ..)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `values` | any | One or more values to print (variadic) |

### Return Value

`null`

### Description

Prints all provided values separated by spaces, followed by a newline. Values are automatically converted to their string representation.

### Examples

```zex
println("Hello, World!")
# Output: Hello, World!

println("Name:", "Alice", "Age:", 25)
# Output: Name: Alice Age: 25

println(42)
# Output: 42

println([1, 2, 3])
# Output: [1, 2, 3]

println(true, false, null)
# Output: true false null
```

---

## print

Prints values to standard output WITHOUT a trailing newline.

### Syntax

```zex
print(value1, value2, ..)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `values` | any | One or more values to print (variadic) |

### Return Value

`null`

### Description

Like `println`, but does not add a newline at the end. Useful for building output incrementally or prompts.

### Examples

```zex
print("Loading")
print(".")
print(".")
print(".")
println(" Done!")
# Output: Loading... Done!

print("Enter name: ")
var name = input()
```

---

## input

Reads a line of text from standard input.

### Syntax

```zex
input(prompt?)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `prompt` | string | Optional prompt to display before reading |

### Return Value

A string containing the line read from stdin (without trailing newline).

### Examples

```zex
# Simple input
var name = input()

# With prompt
var age = input("Enter your age: ")
println("You are " + age + " years old")

# Reading multiple values
var first = input("First name: ")
var last = input("Last name: ")
println("Hello, " + first + " " + last)
```

---

## type

Returns the type (class) of a value.

### Syntax

```zex
type(value)
```

### Parameters

| Parameter | Type | Description |
|-----------|------|-------------|
| `value` | any | The value to get the type of |

### Return Value

The class object representing the type of the value.

### Examples

```zex
println(type(42))          # <class int>
println(type(3.14))        # <class float>
println(type("hello"))     # <class string>
println(type(true))        # <class bool>
println(type(null))        # <class null>
println(type([1, 2, 3]))   # <class array>
println(type((1, 2)))      # <class tuple>

class Person {
    private name
    public Person(name) {
        self.name = name
    }
}

var p = Person("Alice")
println(type(p))           # <class Person>
```

### Type Comparison

You can compare types directly:

```zex
var x = 42
if type(x) == int {
    println("x is an integer")
}
```
