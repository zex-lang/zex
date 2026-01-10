# Contributing

## Code Style

Format code with clang-format before committing:

```bash
clang-format -i src/*.cpp include/zex/*.hpp
```

### Guidelines

- 4 space indentation
- opening brace on same line
- max 100 characters per line
- no trailing whitespace
- blank line at end of file

### Comments

File header comments describe the module purpose:

```cpp
// Zex lexer
// Converts source text into a stream of tokens
```

Inline comments explain non obvious logic:

```cpp
if (offset == 0 && base != 5) {  // RBP needs offset
```

Avoid these patterns in comments:

- section headers like `// === Section ===`
- dashes like `// --- separator ---`
- parentheses in descriptions

Keep comments concise and factual.

## Commit Messages

### Format

```
<type>: <subject>

<body>
```

### Types

| Type | Description |
|------|-------------|
| `feat` | new feature or capability |
| `fix` | bug fix |
| `refactor` | code restructuring |
| `perf` | performance improvement |
| `docs` | documentation only |
| `build` | build system or dependencies |

### Rules

- subject line max 50 characters
- subject line lowercase, no period
- body wrapped at 72 characters
- body explains what and why

### Examples

```
feat: add function call support
```

```
fix: correct stack alignment for local variables

Stack was misaligned by 8 bytes causing segfaults on SSE instructions.
Now properly maintains 16-byte alignment per System V ABI.
```
