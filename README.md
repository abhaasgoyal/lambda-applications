# Functional Programming Apps

## About the project

The project is regarding two aspects of functional programming applications:

1. Real World Application (a back-end on the wordsearch game) - [WordSearch](src/WordSearch)
2. Understanding Computational Logic - [Lambda Calculus](src/LambdaCalculus)

## Getting Started

* Building and running

```
cabal update
cabal build
cabal repl
:l <Module you want to run>
```
GHCI prompt can be used directly by going into specific folders

* Tests

Tests are stored in `Tests/` directory and can be invoked using `cabal test`


## Program Design

### WordSearch
WordSearch is implemented by the following types

```
type WordSearchGrid = [[ Char ]]
type Placement = (Posn,Orientation)
type Posn = (Int,Int)
data Orientation = Forward | Back | Up | Down | UpForward | UpBack | DownForward | DownBack deriving (Eq,Ord,Show,Read)
```

#### Functions
1. Creating N * N word search grid with maximum density (ratio of number of words used in making words and total number of words in the grid). Random letter generation in the grid is from the letters of the words of input string and a guarantee of only one word present in the grid is there.

`createWordSearch :: [String] -> Double -> IO WordSearchGrid`

2. Solving N*N word search grid with a list of words.

`solveWordSearch :: [String] -> WordSearchGrid -> [(String, Maybe Placement)]`


### Macro Lambda Calculus

A form of lambda calculus in which macro expressions are given from which the given expression can be expanded. They have the following grammar

```
<MacroExpr> ::= "def" <MacroName> "=" <Expr> "in" <MacroExpr> | Expr
<Expr> ::= <Var> | MacroName | <Expr> <Expr> | "\" Var "->" Expr | "(" <Expr> ")"
<MacroName> ::= <UChar> | <UChar> <MacroName>
<UChar> ::= "A" | "B" | ... | "Z"
<Var> ::= "x" Digits
<Digits> ::= Digit | Digit Digits
Digit ::= "0" | "1" | ... | "9"
```
In the Haskell implementation, macro expressions are denoted by the following type

```
data LamMacroExpr = LamDef [ (String,LamExpr) ] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr  |
               LamAbs Int LamExpr  | LamVar Int deriving (Eq,Show,Read)
```
1. The tuple `(String, LamExpr)` is useful for having a lookup table for a macro name and it's expansion

2. The `LamAbs` and `LamVar` constructors have `Int` parameters for variable names, making it easier to generate fresh variables during capture avoid substitution. In pretty printing, they are of the form `x1,x2... xn`

3. Function application is left associative and function abstraction is right associative

#### Functions

(Note that the usage of term expression means macro lambda expression in the context)
1. Pretty Printing any expression (removing all unnecessary brackets using left and right associative rules)

   `prettyPrint :: LamMacroExpr -> String`

2. Parse a string according to the grammar (spaces are handled in the right places). Additional checks such as repeated macro names in lambda definitions or free variables in macros are also taken into consideration

   `parseLamMacro :: String -> Maybe LamMacroExpr`

3. Pure Lambda expressions represented in macro form

   `exprToMac :: LamExpr -> LamMacroExpr`

4. Transform an expression to continuous passing style (CPS)

   `cpsTransform :: LamMacroExpr -> LamMacroExpr`

5. Applicative order reduction

   (a) **1 step**

   `innerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr`

   (b) **Bounded n steps**

   `innerRedN :: LamMacroExpr -> Int -> [Maybe LamMacroExpr]`

6. Normal order reduction

   (a) **1 step**

   `outerRedn1 :: LamMacroExpr -> Maybe LamMacroExpr`

   (b) **Bounded n steps**

   `outerRedN :: LamMacroExpr -> Int -> [Maybe LamMacroExpr]`

7. Compare inner and outer reduction of the expression and it's cps Transformation

   `compareInnerOuter :: LamMacroExpr -> Int -> (Maybe Int, Maybe Int, Maybe Int, Maybe Int)`

If the expression cannot be reduced after n steps the reduction step will return a `Nothing` value
