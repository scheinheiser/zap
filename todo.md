## AST
- [ ] Make a distinction between sugared and desugared syntax.
- [ ] Make a desugarer.
  - [ ] Curry lambdas (`fun x y => x` ==> `fun x => fun y => x`)
  - [ ] Remove if statements (turn them into match expressions)
  - [ ] Turn list literals into cons (`[1; 2; 3]` ==> `1 :: 2 :: 3 :: []`)
  - [ ] Turn function-level pattern matching into a single function with a match expression:
```sml
dec sum : [Int] -> Int
def sum [] := 0
def sum (x :: xs) := x + sum xs

% to...

dec sum : [Int] -> Int
def sum = 
  match xs with
  | 0 -> 0
  | x :: xs -> x + sum xs
```
