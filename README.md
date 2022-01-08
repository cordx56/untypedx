# UnTypedX
UnTypedX is gradually typed language.

"untyped" is not good to stand for dynamically typed, but "untyped" is simple so we use this keyword.

## Example
```
let x = 1
let add = fn x y => x + y
println (add 1.1 2)
```

## Language Specification
### Reserved
`->`, `=>`, `:`, `fn`, `infix`, `infixr`
### Special Characters
These Characters are not allowed in identifier.

` ` `\t` `\r` `\n` `(` `)` `{` `}` `[` `]` `<` `>` `:` `,` `|`

### BNF
```
<atexp>                := <cons>
                        | <ident_except_infopr>
                        | "(" <multispace>* ")"
                        | "(" <multispace>* <exp> <multispace>* ")"
                        | "(" <multispace>* <exp> <multispace>* "," <multispace>* ")"
                        | "(" <multispace>* <exp> (<multispace>* "," <multispace>* <exp>)+ <multispace>* ")"
                        | <blockexp>
                        | <fnexp>
<appexp>               := <atexp> (<space>+ <atexp>)*
<infexp>               := <appexp> (<space>* <infopr> <space>* <appexp>)*
<exp>                  := (
                            <infexp>
                          | <untypedexp>
                          )
                          (
                            <space>* ":" <space>* <ty>
                          )?

<untypedexp>           := "untyped" <space>+ <exp>
<blcokexp>             := "{" <multispace>* <stmts> <multispace>* "}"
<fnexp>                := "fn" <multispace>+ <fnrule> (<multispace>* "|" <multispace>* <fnrule>)*
<fnrule>               := <fnpats> <multispace>* "=>" <multispace>* <exp>
<fnpats>               := (<multispace>* <pat>)*
<pat>                  := <atpat> (<space>* ":" <space>* <ty>)?
<atpat>                := "_"
                        | <cons>
                        | <ident_except_infopr>
                        | "(" <multispace>* ")"
                        | "(" <multispace>* <pat> <multispace>* ")"
                        | "(" <multispace>* <pat> <multispace>* "," <multispace>* ")"
                        | "(" <multispace>* <pat> (<multispace>* "," <multispace>* <pat>)+ ")"

<stmt>                 := <exp> <space>* "\n"
                        | <reginfexp>
<reginfstmt>           := ("infix" | "infixr") <space>+ <integer> <space>+ <ident_except_infopr> <space>* "=" <space>* <fnexp>
<stmts>                := (<multispace>* <stmt>)* <multispace>* <exp>?

<ty>                   := (
                            <ident_except_infopr> (<space>* "<" <space>* <ty> (<space>* "," <space>* <ty>)* <space>* ">")?
                          | "(" <multispace>* <ty> <multispace>* ")"
                          | "(" <multispace>* <ty> <multispace>* "," <multispace>* ")"
                          | "(" <multispace>* <ty> (<multispace>* "," <multispace>* <ty>)+ ")"
                          )
                          (
                            <space>* "->" <space>* <ty>
                          )?
```

### Types
#### Primitive types
`int`: Integer,
`real`: Floating point number,
`string`: String,

### Default infix operator
```
infix  3 =
infix  4 <>
infix  4 !=
infix  4 <
infix  4 >
infix  4 <=
infix  4 >=
infix  6 +
infix  6 -
infix  7 *
infix  7 /
infix  7 %
```
