# Zex Standard Types

This document describes the built-in type classes and their methods in Zex.

---

## Table of Contents

- [Type Classes](#type-classes)
- [Exception Classes](#exception-classes)
- [int](#int)
- [float](#float)
- [string](#string)
- [array](#array)
- [tuple](#tuple)

---

## Type Classes

Zex has the following built-in type classes. They can be called as constructors for type conversion:

| Class | Description | Conversion Example |
|-------|-------------|-------------------|
| `int` | 64-bit integer | `int(3.14)` → `3` |
| `float` | 64-bit floating point | `float(42)` → `42.0` |
| `string` | UTF-8 text | `string(100)` → `"100"` |
| `bool` | Boolean | `bool(1)` → `true` |
| `null` | Null type | N/A |
| `tuple` | Immutable sequence | N/A |

---

## Exception Classes

Zex provides the following exception classes for error handling:

| Class | Description |
|-------|-------------|
| `Exception` | Base exception class |
| `ValueError` | Invalid value |
| `TypeError` | Invalid type |
| `IndexError` | Index out of bounds |
| `ZeroDivisionError` | Division by zero |
| `AttributeError` | Attribute not found |
| `NameError` | Name not found |
| `RuntimeError` | General runtime error |

---

## int

Integer type for 64-bit signed integers.

### Methods

#### abs()

Returns the absolute value of the integer.

```zex
var x = -42
println(x.abs())  # 42

var y = 10
println(y.abs())  # 10
```

---

## float

Floating point type for 64-bit double-precision numbers.

### Methods

#### abs()

Returns the absolute value.

```zex
var x = -3.14
println(x.abs())  # 3.14
```

#### floor()

Returns the largest integer less than or equal to the value.

```zex
println(3.7.floor())   # 3
println(-2.3.floor())  # -3
```

#### ceil()

Returns the smallest integer greater than or equal to the value.

```zex
println(3.2.ceil())   # 4
println(-2.7.ceil())  # -2
```

#### round()

Returns the nearest integer (rounds half away from zero).

```zex
println(3.5.round())   # 4
println(3.4.round())   # 3
println(-2.5.round())  # -3
```

---

## string

UTF-8 text strings with full Unicode support.

### Methods

#### len()

Returns the number of Unicode characters (not bytes).

```zex
var s = "Hello"
println(s.len())  # 5

var emoji = "Hi 👋"
println(emoji.len())  # 4 (3 chars + 1 emoji)
```

#### bytes()

Returns the number of bytes in the UTF-8 encoding.

```zex
var s = "Hello"
println(s.bytes())  # 5

var emoji = "Hi 👋"
println(emoji.bytes())  # 7 (3 + 4 bytes for emoji)
```

#### upper()

Returns a new string with all ASCII characters converted to uppercase.

```zex
var s = "Hello World"
println(s.upper())  # "HELLO WORLD"
```

#### lower()

Returns a new string with all ASCII characters converted to lowercase.

```zex
var s = "Hello World"
println(s.lower())  # "hello world"
```

#### trim()

Returns a new string with leading and trailing whitespace removed.

```zex
var s = "  hello  "
println(s.trim())  # "hello"
```

#### startsWith(prefix)

Returns `true` if the string starts with the given prefix.

```zex
var s = "Hello World"
println(s.startsWith("Hello"))  # true
println(s.startsWith("World"))  # false
```

#### endsWith(suffix)

Returns `true` if the string ends with the given suffix.

```zex
var s = "Hello World"
println(s.endsWith("World"))  # true
println(s.endsWith("Hello"))  # false
```

#### contains(substring)

Returns `true` if the string contains the given substring.

```zex
var s = "Hello World"
println(s.contains("lo Wo"))  # true
println(s.contains("xyz"))    # false
```

#### slice(start, end?)

Returns a substring from `start` to `end` (exclusive). Supports negative indices.

```zex
var s = "Hello World"
println(s.slice(0, 5))   # "Hello"
println(s.slice(6))      # "World"
println(s.slice(-5))     # "World"
println(s.slice(-5, -2)) # "Wor"
```

#### split(separator)

Splits the string by the separator and returns an array of substrings.

```zex
var s = "a,b,c"
println(s.split(","))  # ["a", "b", "c"]

var words = "Hello World"
println(words.split(" "))  # ["Hello", "World"]
```

#### replace(old, new)

Returns a new string with all occurrences of `old` replaced by `new`.

```zex
var s = "hello world"
println(s.replace("world", "Zex"))  # "hello Zex"
```

#### forEach(fn)

Iterates over each character, calling the function for each.

```zex
"abc".forEach(|c| println(c))
# Output:
# a
# b
# c
```

#### map(fn)

Returns a new string with each character transformed by the function.

```zex
var s = "abc"
var upper = s.map(|c| c.upper())
println(upper)  # "ABC"
```

#### filter(fn)

Returns a new string with only characters where `fn` returns truthy.

```zex
var s = "a1b2c3"
var letters = s.filter(|c| c >= "a" && c <= "z")
println(letters)  # "abc"
```

#### reduce(fn, initial)

Reduces the string to a single value by applying `fn` to each character.

```zex
var s = "abc"
var result = s.reduce(|acc, c| acc + c + "-", "")
println(result)  # "a-b-c-"
```

#### find(fn)

Returns the first character where `fn` returns truthy, or `null`.

```zex
var s = "abc123"
var digit = s.find(|c| c >= "0" && c <= "9")
println(digit)  # "1"
```

#### some(fn)

Returns `true` if `fn` returns truthy for any character.

```zex
var s = "abc123"
println(s.some(|c| c >= "0" && c <= "9"))  # true
```

#### every(fn)

Returns `true` if `fn` returns truthy for all characters.

```zex
var s = "123"
println(s.every(|c| c >= "0" && c <= "9"))  # true
```

---

## array

Dynamic arrays that can hold values of any type.

### Methods

#### len()

Returns the number of elements.

```zex
var arr = [1, 2, 3]
println(arr.len())  # 3
```

#### push(value)

Appends an element to the end of the array.

```zex
var arr = [1, 2]
arr.push(3)
println(arr)  # [1, 2, 3]
```

#### pop()

Removes and returns the last element.

```zex
var arr = [1, 2, 3]
var last = arr.pop()
println(last)  # 3
println(arr)   # [1, 2]
```

#### first()

Returns the first element, or `null` if empty.

```zex
var arr = [1, 2, 3]
println(arr.first())  # 1
```

#### last()

Returns the last element, or `null` if empty.

```zex
var arr = [1, 2, 3]
println(arr.last())  # 3
```

#### contains(value)

Returns `true` if the array contains the value.

```zex
var arr = [1, 2, 3]
println(arr.contains(2))  # true
println(arr.contains(5))  # false
```

#### empty()

Returns `true` if the array has no elements.

```zex
var arr = []
println(arr.empty())  # true
```

#### clear()

Removes all elements from the array.

```zex
var arr = [1, 2, 3]
arr.clear()
println(arr)  # []
```

#### reverse()

Reverses the array in place.

```zex
var arr = [1, 2, 3]
arr.reverse()
println(arr)  # [3, 2, 1]
```

#### join(separator)

Joins all elements into a string with the given separator.

```zex
var arr = ["a", "b", "c"]
println(arr.join("-"))  # "a-b-c"

var nums = [1, 2, 3]
println(nums.join(", "))  # "1, 2, 3"
```

#### map(fn)

Returns a new array with each element transformed by `fn`.

```zex
var arr = [1, 2, 3]
var doubled = arr.map(|x| x * 2)
println(doubled)  # [2, 4, 6]
```

#### filter(fn)

Returns a new array with elements where `fn` returns truthy.

```zex
var arr = [1, 2, 3, 4, 5]
var evens = arr.filter(|x| x % 2 == 0)
println(evens)  # [2, 4]
```

#### reduce(fn, initial)

Reduces the array to a single value.

```zex
var arr = [1, 2, 3, 4, 5]
var sum = arr.reduce(|acc, x| acc + x, 0)
println(sum)  # 15
```

#### forEach(fn)

Calls `fn` for each element.

```zex
var arr = [1, 2, 3]
arr.forEach(|x| println(x))
# Output:
# 1
# 2
# 3
```

#### find(fn)

Returns the first element where `fn` returns truthy, or `null`.

```zex
var arr = [1, 2, 3, 4, 5]
var found = arr.find(|x| x > 3)
println(found)  # 4
```

#### some(fn)

Returns `true` if `fn` returns truthy for any element.

```zex
var arr = [1, 2, 3]
println(arr.some(|x| x > 2))  # true
```

#### every(fn)

Returns `true` if `fn` returns truthy for all elements.

```zex
var arr = [2, 4, 6]
println(arr.every(|x| x % 2 == 0))  # true
```

---

## tuple

Immutable sequences accessed by index.

### Creating Tuples

```zex
var point = (10, 20)      # Two-element tuple
var single = (42,)        # Single-element tuple (trailing comma required)
var empty = ()            # Empty tuple
```

### Accessing Elements

Use dot notation with numeric indices:

```zex
var point = (10, 20)
var x = point.0    # 10
var y = point.1    # 20

# Nested access
var nested = ((1, 2), (3, 4))
println(nested.1.1)  # 4
```

### Methods

#### len()

Returns the number of elements.

```zex
var t = (1, 2, 3)
println(t.len())  # 3
```

#### first()

Returns the first element, or `null` if empty.

```zex
var t = (1, 2, 3)
println(t.first())  # 1
```

#### last()

Returns the last element, or `null` if empty.

```zex
var t = (1, 2, 3)
println(t.last())  # 3
```

#### contains(value)

Returns `true` if the tuple contains the value.

```zex
var t = (1, 2, 3)
println(t.contains(2))  # true
println(t.contains(5))  # false
```

#### empty()

Returns `true` if the tuple has no elements.

```zex
var t = ()
println(t.empty())  # true
```
