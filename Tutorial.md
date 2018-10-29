# Janus

Janus is likely the very first reversible programming language, designed by Christopher Lutz and Howard Derby in 1986. It was later rediscovered by Michael P. Frank and further Robert Glück and Tetsuo Yokoyama after which it became a central research topic in the PIRC group. Since then several versions of the language have been developed. We refer to the publications below for more details.

This page will give an introduction to the latest version of Janus, which is implemented here. The language is still much work in progress and new features are added as needed. We tend not to remove anything (or change semantics) but this cannot be guaranteed. Do not consider this complete.

The following will not explain the details of reversible computations and for this we refer to other sources.

## Example

Here is an example of a program that can give you a first impression. The program computes a Fibonacci pair.

```
procedure fib(int x1, int x2, int n)
    if n = 0 then
        x1 += 1
        x2 += 1
    else
        n -= 1
        call fib(x1, x2, n)
        x1 += x2
        x1 <=> x2
    fi x1 = x2

procedure main()
    int x1
    int x2
    int n
    n += 4
    call fib(x1, x2, n)
```

We will explain the details in the following.

## Program

A program is defined as a number of procedures. A procedure is defined by the `procedure` keyword followed by a name and a list of the inputs. All procedures are defined as call-by-reference, which means that procedures updates input values.

Like in C (and many other languages), programs must include a main procedure. Currently they does not take arguments and can only be defined with `main()`.

The body of a program is a new-line separated list statements.

### Comments
Comment are written with standard C notation.

## Types
The following will describe the different types available. Note that expressions

### "Magical" int
The main type of Janus is the `int`. Special to this is that the representation of `int`s are only defined at interpretation-time. That means that the program can have different behaviour dependent of the chosen representation. Currently there are are following possibilities:

* Arbitrary sized number. There is no overflow of the numbers when performing arithmetic.
* Binary (two's complement) representation limited to some size: normally 2^32. This will result in arithmetic using modular arithmetic
* Galois Field size. This means that the is a prime number of elements available and arithmetic is performed using finite field arithmetic.

### Fixed ints
To make Janus useful as a system language, the are 4 sizes of signed and unsigned ints: `i8`, `i16`, `i32`, `i64`, `u8`, `u16`, `u32`, and `u64`. Arithmetic with these will also be performed using modular arithmetic.

### Boolean
There is the standard `bool` available. These can be compared using the `true` and `false` keywords.

### Arrays
Arrays can be defined over any of the above types. An array is always defined to be a specific number of elements. Defining an array of seven integers is done by
```
int text[7]
```

### Stacks
Finally you can also define a `stack` of arbitrary elements


## Definitions
The beginning of the main procedure and local-delocal statements include definitions. All definitions must also have a variable assignment. All defined values are zero-cleared.

Definitions can also be directly assigned a value. E.g.

```
int x = 4
```

Assigns 4 to `x` at the definition.

Similarly, arrays can also be initialised using a C style notation.

```
int y[3] = {1,2,3}
int z[3] = {4}
```

<!-- constant and ancilla -->


## Statements
Compared to a conventional imperative language, statements in Janus looks slightly different. In the following we will quickly introduce the existing. Again, this will not explain design choices in relation to reversibility, but focus on syntax and semantics.

Note that updates and definitions (unlike C) are not terminated with semicolon.


### Reversible update
There exist four difference reversible updates in Janus. The three of them is for updating variables using plus, minus and xor respectively:

```
[Var] += [Expr]
[Var] -= [Expr]
[Var] ^= [Expr]
```

Note that [Var] in the above must not be used in [Expr].

The fourth update is used the swap the value of two variables:

```
[Var] <=> [Var]
```

### Conditional and assertions
Following the normal convention of reversible programming the conditional is extended with a `fi` assertion.

```
if [Cond]
then
  [Stmt1]
else
  [Stmt2]
fi [Assert]
```

Janus also contains an assertion statement, which is similar to C; also in semantics.

```
assert [Cond]
```

### Loops
The most general loop construct in Janus is the following, which can be seen as combination of the while-do and do-while loops, with the addition of an entry/iteration assertion.

```
from [Assert]
do
  [Stmt1]
loop
  [Stmt2]
until
  [Cond]
```

Janus also contains a for inspired loop, which iterates from a definition incrementing with a given amount until it is larger than or equal to a second expression.

```
iterate [Def] by [Expr] to [Expr]
  [Stmt]
end
```

If `by [Expr]` is omitted, it is parsed as `by 1`.

### Procedure calls
Procedure calls are made using the `call` keyword. As the language is reversible we can also reverse compute (or interpret) procedures using the `uncall` keyword.


### Local definitions
Definitions of local-delocal can be used to create temporary values. The `local` definitions and `delocal` assertions must be paired together as shown below.

```
local [Definition]
  [Stmts]
delocal [Assertion]
```

One special case of a local definition is the `ancilla`. This creates an ancilla variable that are on scope over all possible following statements.

```
ancilla [Definition]
```

By ancilla we mean a variable that can be changed, but must be reset to the initial value at output. Thus the `ancilla` definition is syntactic sugar to a local-delocal with the definition duplicated.

The `constant` definition is similar to `ancilla` definitions with the difference that the defined variable must not be changed. This is ensured by the type system.

```
constant [Definition]
```
### Stack
You can the two standard stack operations `push` and `pop` that works like procedures.

### Misc
The `skip` statement is used as a no operation. This can be useful in sub-statements that does not need to perform operations.

`show`, `print`, and `printf` can be used to output values to stdout. These will perform output in both forward and backward execution. That means that they actually are not reversible, but very useful for debugging.

Finally, the `error` statement can be use to throw a run-time error.

## Expressions

### Arithmetic
Janus has the following binary operators:

  * Addition: `+`
  * Subtraction: `-`
  * Multiplication: `*`
  * Exponentiation: `**`
  * Integer division: `/`
  * Modulo: `%`
  * Value negation: `~`

To this you can also use `size` to return the length of an array.

### Bit expressions
The following expressions that works on bits:
* Shift-left: "<<"
  * For values with a fixed size (e.g. 32 bits) this implements a rotate-left
* Shift-right: ">>"
  * For values with a fixed size (e.g. 32 bits) this implements a rotate-right
* Bitwise and: `&`
* Bitwise or: `|`
* Bitwise xor: `^`

### Integer comparison
  * Less-than or equal-to: `<=`
  * Less-than: `<`
  * Greater-than or equal-to: `>=`
  * Greater-than: `>`
  * Equal-to: `=`
  * Not equal-to: `!=`

### Logical operators:
  * Logical and: `&&`
  * Logical or: `||`
  * Logical Negation: `!`

### Type casting
It is possible to cast between different types in expressions using C notation
<!-- , "nil" -->

### Stack
You can query a stack using either `empty` (is the stack empty) or `top` that returns a copy of the top element.


