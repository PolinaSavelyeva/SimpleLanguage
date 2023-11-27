# SimpleLanguage

This repository contains simple programming language with parser, interpreter and code analyzer. The language includes basic constructs such as conditional and print statements, variable assignment and logical expressions.

## Syntax

### Conditional Statement

The syntax for the `if` statement is as follows,

```fsharp
if:<condition>then:
<statements>
else:
<statements>
end
```

where
- `<condition>` represents a boolean expression which contains operators such as `<`, `&&`, boolean variables [etc]((#logical-expressions).
- `<statements>` could be one or more statements e.g [print](#print-statement), [variable assignment](#variable-assignment) or other if statements. Make sure there are no tabs between the if-else construction and the statements. Line breaks are also required, as they appeared on the code-syntax block.

### Variable Assignment

The syntax for variable assignments is as follows,

```fsharp
<variableName>=<expression>
```

where
- `<variableName>` is the name of the variable. It can only be a sequence of lowercase letters.
- `<expression>` one of the available [logical](#logical-expressions) or [arithmetic](#arithmetic-expressions) expressions.

### Print Statement

The syntax for the `print` statement is as follows,

```fsharp
print:<expression>
```

where
- `<expression>` one of the available [logical](#logical-expressions) or [arithmetic](#arithmetic_expressions) expressions.

### Logical Expressions

Logical expressions can be used in every statement. The supported operators are:

- `==` equal
- `<` less than
- `>` greater than
- `<=` less than or equal
- `>=` greater than or equal
- `!=` inequality
- `&&` logical and
- `||` logical or

### Arithmetic Expressions

Arithmetic expressions can be used in every statement. The supported operators are:

- `+` addition
- `*` multiplication

## Examples

Here are some examples of code written in this language:

```fsharp
if:true>falsethen:
x=true
print:x<false
else:
x=true
if:x==truethen:
x=true
else:
x=true
end
print:true
x=true
end
x=3+4+5
print:true&&false<true||false 
```

## Common Mistakes
- Make sure you do not add line breaks at the beginning of the text document.
- If you use reserved variable names, such as print, the behaviour of the program may be unpredictable. Try not to use them at all.

## Running
1. Clone source repository to your remote.
2. Add desired code to a new text file or to an F# variable.
3. And then call `evaluateProgram` function from `SimpleLanguage.Main`.

## Template
To find more building and running options take a look at the [MiniScaffold](https://github.com/TheAngryByrd/MiniScaffold) template.
