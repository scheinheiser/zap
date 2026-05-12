## Design
- [ ] Redo type declaration syntax.
- [ ] Parse and desugar these new declaration structures.

## Parser
- [ ] Migrate from use of exceptions to a custom error type.

## AST
- [x] Make a distinction between sugared and desugared syntax.
- [x] Make a desugarer.
  - [x] Curry lambdas (`fun x y -> x` ==> `fun x -> fun y -> x`)
  - [x] Remove if statements (turn them into match expressions)
  - [x] Turn list literals into cons (`[1; 2; 3]` ==> `1 :: 2 :: 3 :: []`)
  - [x] Turn function-level pattern matching into a single function with a match expression:
```haskell
dec sum : [Int] -> Int
def sum [] := 0
def sum (x :: xs) := x + sum xs

dec map : (A -> B) -> [A] -> [B]
def map _ [] := []
def map f (x :: xs) := f x :: map f xs

% to...

dec sum : [Int] -> Int
def sum = 
  fun xs ->
    match xs to
    | 0 => 0
    | x :: xs => x + sum xs

dec map : (A -> B) -> [A] -> [B]
def map =
  fun f -> fun xs ->
    match (f, xs) to
    | (_, []) => []
    | (f, x :: xs) => f x :: map f xs
```
