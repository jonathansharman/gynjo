![build](https://github.com/jonathansharman/gynjo/workflows/build/badge.svg)

# Gynjo

A command-line calculator and programming language with an emphasis on short, mathematically natural syntax. Pronounced /'dʒɪndʒoʊ/.

```
>> let f_to_c = f -> (5/9)(f - 32) // Fahrenheit to Celsius
>> f_to_c 32
0
>> f_to_c 77
25
>> f_to_c 51
10 5/9 (10.5555555556)
```

## Contents
1. [Variable Assignment](#variable-assignment)
1. [Value Types](#value-types)
1. [Operators and Precedence](#operators-and-precedence)
1. [Units and Dimensional Analysis](#units-and-dimensional-analysis)
1. [Blocks and Control Flow](#blocks-and-control-flow)
1. [Functions](#functions)
1. [Importing Scripts](#importing-scripts)

## Variable Assignment

Variables can be set using `let` *variable* `=` *value*. Variable names can contain English letters and underscores. The variable `ans` refers to the most recent non-`()` result.

```
>> let a = 1
>> a + 2
3
>> ans
3
```

## Value Types

| Type             | Description                               | Examples                             |
| :--------------- | :---------------------------------------- | :----------------------------------- |
| `type`           | One of the types in this table            | `integer`, `get_type(1)`             |
| `boolean`        | `true` or `false`                         | `true`, `false`                      |
| `integer`        | Arbitrary-precision integer quantity      | `0`, `-1`, `2.s`                     |
| `rational`       | Arbitrary-precision rational quantity     | `1/2`, `-5/3`, `2.m/3.kg`            |
| `real`           | High-precision floating-point quantity    | `0.1`, `-42.5`, `1.5.m`              |
| `string`         | String of ASCII text                      | `"hello"`, `"world"`                 |
| `tuple`          | List of values, mainly used for arguments | `()`, `(1, 2)`                       |
| `list`           | Functional singly-linked list of values   | `[]`, `[1, 2]`                       |
| `range`          | A lazy, half-open range of values         | `0..5`, `0..-5`, `0.m .. 8.m by 2.m` |
| `closure`        | A function with its captured environment  | `x -> x`, `(a, b) -> a + b`          |
| `break_value`    | The result of a break expression          | `break`                              |
| `return_value`   | The result of a `return` expression       | `return 1`                           |

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
| `range`    | `list`     |
| `range`    | `tuple`    |

## Operators and Precedence

### Operators from High to Low Precedence
|            | Operation                                      | Syntax                                      |
| ---------: | :--------------------------------------------- | :------------------------------------------ |
|          1 | Function application with parentheses          | *f*`(`*arg1*`,` *arg2*`,` *...*`)`          |
|          2 | List/string index or slice                     | *list/string*`[`*index* or *range*`]`       |
|          3 | Exponentiation                                 | *value* `**` *value* or *value* `^` *value* |
|          4 | Function application without parentheses       | *f* *arg*                                   |
|          5 | Implicit multiplication                        | *value* *value*                             |
|          6 | Explicit multiplication                        | *value* `*` *value*                         |
|          6 | Division                                       | *value*`/`*value*                           |
|          7 | Numeric negation                               | `-`*value*                                  |
|          8 | String and list concatenation                  | *string/list* `\|` *string/list*            |
|          9 | Addition and subtraction                       | *value* `+`/`-` *value*                     |
|         10 | Type conversion                                | *value* `as` *type*                         |
|         10 | Unit conversion                                | *value* `in` *unit*                         |
|         11 | Comparisons                                    | *value* `<`/`<=`/`>`/`>=` *value*           |
|         12 | Equality, inequality, and approximate equality | *value* `=`/`!=`/`~` *value*                |
|         13 | Logical conjunction                            | *boolean* `and` *boolean*                   |
|         14 | Logical disjunction                            | *boolean* `or` *boolean*                    |
|         15 | Logical negation                               | `not` *boolean*                             |
|         15 | Break out of loop early                        | `break`                                     |
|         16 | Range creation                                 | *value*`..`*value* [ `by` *value* ]         |
|         17 | Return from function early                     | `return` *value*                            |
|         17 | Read from console                              | `read`                                      |
|         17 | Write to console                               | `write` *value*                             |
|         17 | Get type                                       | `get_type` *value*                          |
|         17 | Conversion to base units                       | `basic` *value*                             |

Implicit multiplication is supported and uses the same syntax as function application. This means that precedence is partially resolved during intepretation based on the values of the operands. Implicit multiplication has higher precedence than explicit multiplication/division, so for example, `2a/3b` is equal to `(2a)/(3b)`.

Function application varies in precedence depending on the use of parentheses so that, for example, `sin x^2` does `x^2` first but `sin(x)^2` does `sin(x)` first, to better match expectations.

Lists can be concatenated with lists, and strings can be concatenated with arbitrary values on either side.

Approximate equality is like equality, except that numeric types compare approximately equal if their real-valued display representations are equal. By default, `real` values are displayed using twelve significant digits. This display setting can be overridden in the current environment by setting the variable `precision` to some positive number of digits.

```
>> let precision = 2
>> 1/3 as real ~ .33
true
```

### Indexing and Slicing

List and string indices are zero-based and modulo length, so `[1, 2, 3][-1]` is equal to `3`, and `"hello"[5]` is equal to `"h"`. Indexing into an empty list results in an out-of-bounds error.

Indexing into a list or string using a range value creates a slice: a subsequence of the list/string elements. Conceptually, slice indices can be thought of as sitting "between" the zero-indexed elements, so `[1, 2, 3][1..2]` and `[1, 2, 3][2..1]` are both `[2]`. Slices can infer missing start, end, and/or stride, so `[1, 2, 3][..]` is `[1, 2, 3]`, `[1, 2, 3][2.. by -1]` is `[2, 1]`, and `"hello"[1.. by 2]` is `"el"`. Out-of-bounds portions of a slice are simply ignored, so `[1, 2, 3][10..]` is `[]`.

## Units and Dimensional Analysis

Units are a `.` followed by English letters and/or underscores. Expressions can employ units freely, and the rules of dimensional analysis apply.

```
>> 6.apple / 3.orange
2.apple.orange^-1
>> ans * 5.orange
10.apple
```

By default, units are treated as base units. A unit can be defined in terms of other units using a unit declaration, using similar syntax to variable assignment.

```
>> let .N = .kg.m/.s^2
>> let .Pa = .N/.m^2
>> 5.Pa * 4.m^2 = 20.N
true
```

The Gynjo core library defines some SI and imperial units by default (see [core/units.gynj](core/units.gynj)).

```
>> let speed = 60.km/.hr
>> let time = 45.min
>> let distance = speed * time
>> distance
45.km
```

When performing addition or subtraction with two different units of the same dimension, the units of the left-hand side are used for the result. Likewise, different units of the same dimension are merged during multiplication or division.

```
>> 1.m + 100.cm
2.m
>> 1.m * 100.cm
1.m^2
```

Quantities can be explicitly converted to different units with the same dimensions using the `in` operator, or to all base units using the `basic` operator.

```
>> 5.Pa * 4.m^2 in .N
20.N
>> basic 90.km/.hr
25.m.s^-1
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

### Control Expressions
- `if` *test* `then` *expression* [ `else` *expression* ]
- `for` *variable* `in` *range* `do` *body*
- `while` *test* `do` *body*

```
>> if 2 < 1 then "foo" // Empty else evaluates to ()
>> if 2 < 1 then "foo" else "bar"
"bar"
>> for a in ["one", "two", "three"] do write a
"one"
"two"
"three"
>> for a in 1..6 by 2 do write a
1
3
5
>> while a > 0 do { write a; let a = a - 2 }
5
3
1
```

## Functions

Functions are written `(`*arg1*`,` *arg2*`,` *...*`)` `->` *body*. Unary functions can omit the parentheses: *arg* `->` *body*.

Gynjo uses function scoping rather than lexical scoping, so each function application creates its own variable environment, but loops and blocks do not.

### Built-in Functions

| Signature     | Effect                                                       |
| :------------ | :----------------------------------------------------------- |
| `pop(l)`      | Gets the elements of `l` (`list`) after the first            |

## Importing Scripts

Very simple textual importation is supported via `import` expressions. The argument to the `import` expression should be a `string` that names a file containing a Gynjo script.

```
>> import "path/to/lib.gynj" // Executes the script at "path/to/lib.gynj"
```

At startup, the interpreter will check for `.gynjo_profile` in the user's home directory and execute it if present.
