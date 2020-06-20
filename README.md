![build](https://github.com/jonathansharman/gynjo/workflows/build/badge.svg)

# Gynjo

A command-line calculator and programming language with an emphasis on short, mathematically natural syntax. Pronounced /'dʒɪndʒoʊ/.

```
>> let f_to_c = f -> 5/9(f - 32) // Fahrenheit to Celsius
>> f_to_c 32
0
>> f_to_c 77
25
>> f_to_c 51
10 5/9 (10.5555555556)
```

## Contents
1. [Variable Assignment](#variables)
1. [Operators and Precedence](#operators-and-precedence)
1. [Blocks and Control Flow](#blocks-and-control-flow)
1. [Value Types](#value-types)
1. [Functions](#functions)
1. [Importing Scripts](#importing-scripts)

## Variable Assignment

Variables can be set using `let` *variable* `=` *value*. The variable `ans` refers to the most recent non-`()` result.

```
>> let a = 1
>> a + 2
3
>> ans
3
```

## Operators and Precedence

### Operators from High to Low Precedence
|            | Operation                                      | Syntax                                      |
| ---------: | :--------------------------------------------- | :------------------------------------------ |
|          1 | Function application with parentheses          | *f*`(`*arg1*`,` *arg2*`,` *...*`)`          |
|          2 | List/string index                              | *list/string*`[`*index*`]`                  |
|          3 | Exponentiation                                 | *value* `**` *value* or *value* `^` *value* |
|          4 | Function application without parentheses       | *f* *arg*                                   |
|          5 | Multiplication                                 | *value* `*` *value* or *value* *value*      |
|          5 | Division                                       | *value*`/`*value*                           |
|          6 | Numeric negation                               | `-`*value*                                  |
|          7 | String and list concatenation                  | *string/list* `\|` *string/list*            |
|          8 | Addition and subtraction                       | *value* `+`/`-` *value*                     |
|          9 | Type conversion                                | *value* `as` *type*                         |
|         10 | Comparisons                                    | *value* `<`/`<=`/`>`/`>=` *value*           |
|         11 | Equality, inequality, and approximate equality | *value* `=`/`!=`/`~` *value*                |
|         12 | Logical conjunction                            | *boolean* `and` *boolean*                   |
|         13 | Logical disjunction                            | *boolean* `or` *boolean*                    |
|         14 | Logical negation                               | `not` *boolean*                             |
|         14 | Break out of loop early                        | `break`                                     |
|         14 | Return from function early                     | `return` *value*                            |
|         14 | Import                                         | `import` *string*                           |
|         14 | Read from console                              | `read`                                      |
|         14 | Write to console                               | `write` *value*                             |
|         14 | Get type                                       | `get_type` *value*                          |

Implicit multiplication is supported and uses the same syntax as function application. This means that precedence is partially resolved during intepretation based on the values of the operands.

Function application varies in precedence depending on the use of parentheses so that, for example, `sin x^2` does `x^2` first but `sin(x)^2` does `sin(x)` first, to better match expectations.

List and string indexes are modulo length, so `[1, 2, 3][-1]` is equal to `3`, and `"hello"[5]` is equal to `"h"`.

Lists can be concatenated with lists, and strings can be concatenated with arbitrary values on either side.

Approximate equality is like equality, except that numeric types compare approximately equal if their real-valued display representations are equal. By default, `real` values are displayed using twelve significant digits. This display setting can be overridden in the current environment by setting the variable `precision` to some positive number of digits.

```
>> let precision = 2
>> 1/3 as real ~ .33
true
```

## Blocks and Control Flow

Blocks allow sequential evaluation of a semicolon-separated list of expressions inside `{}`, with the overall result being the result of the final (possibly empty) expression. Each non-final expression must evaluate to `()`.

```
>> {
     let a = 1;
     let b = 2;
     a + b
   }
3
```

### Control expressions
- `if` *test* `then` *expression* [ `else` *expression* ]
- `for` *variable* `in` *range* `do` *body*
- `while` *test* `do` *body*

```
>> if 2 < 1 then "foo" // Empty else evaluates to ()
>> if 2 < 1 then "foo" else "bar"
"bar"
>> for a in [1, 2, 3] do print a
1
2
3
>> while a > 0 do { print a; let a = a - 1 }
3
2
1
```

## Value Types

| Type             | Description                               | Examples                      |
| :--------------- | :---------------------------------------- | :---------------------------- |
| `type`           | One of the types in this table            | `integer`, `get_type(1)`      |
| `boolean`        | `true` or `false`                         | `true`, `false`               |
| `integer`        | Arbitrary-precision integer type          | `0`, `-1`, `42`               |
| `rational`       | Arbitrary-precision rational type         | `1/2`, `-5/3`                 |
| `real`           | High-precision floating-point type        | `0.1`, `-42.5`                |
| `string`         | String of ASCII text                      | `"hello"`, `"world"`          |
| `tuple`          | List of values, mainly used for arguments | `()`, `(1, 2)`                |
| `list`           | Functional singly-linked list of values   | `[]`, `[1, 2]`                |
| `closure`        | A function with its captured environment  | `x -> x`, `(a, b) -> a + b`   |
| `break_value`    | The result of a break expression          | `break`                       |
| `return_value`   | The result of a `return` expression       | `return 1`                    |

The Gynjo runtime tries to store numerical values as an integer if possible. The expression `(3/2) * 2` is a `rational` times an `integer`, which in general is a `rational`, but since the resulting value is integral (in the mathematical sense), its type is actually `integer`. That also means that `1.0` evaluates to an object of type `integer`. The runtime does not attempt to shrink `real`s into `rational`s since there could be round-off error.

Why have both `tuple` and `list` when both are just lists of values? The reason is syntactic: `(1)` is interpreted as `1` rather than a `tuple` containing `1`. This allows the use of parentheses to change the order of operations without turning something into a `tuple`. Also, the empty `tuple`, `()`, is use to represent no return value. Meanwhile, `list`s are always just `list`s.

### Possible Type Conversions via the `as` Operator
| From       | To         |
| :--------- | :--------- |
| any type   | itself     |
| any type   | `string`   |
| `integer`  | `rational` |
| `integer`  | `real`     |
| `rational` | `real`     |
| `list`     | `tuple`    |
| `tuple`    | `list`     |

## Functions

Functions are written `(`*arg1*`,` *arg2*`,` *...*`)` `->` *body*. Unary functions can omit the parentheses: *arg* `->` *body*.

Gynjo uses function scoping rather than lexical scoping, so each function application creates its own variable environment, but loops and blocks do not.

### Built-in Functions

| Signature     | Effect                                                       |
| :------------ | :----------------------------------------------------------- |
| `top(l)`      | Gets the top (or first) element of `l` (`list`)              |
| `pop(l)`      | Gets the elements of `l` (`list`) except for `top(l)`        |
| `print(v)`    | Converts `v` to a `string` and outputs it to the console     |
| `read()`      | Reads a line of input from the console as a `string`         |
| `get_type(v)` | Gets the type of `v`                                         |

## Importing Scripts

Very simple textual importation is supported via `import` expressions. The argument to the `import` expression should be a `string` that names a file containing a Gynjo script.

```
>> import "path/to/lib.gynj" // Executes the script at "path/to/lib.gynj"
```
