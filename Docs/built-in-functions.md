# Zex Built-in Functions

This document describes the global built-in functions available in Zex.

---

## println

Prints one or more values to standard output followed by a newline.

### Syntax

```zex
println(value1, value2, ...)
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
    fun init(self, name) {
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
