@module PatternMatching

type shape :=
  | Circle ~ int
  | Square
  | Rectangle

dec isNat : int -> bool.
def isNat n :=
  match n to
  | _;
    print "LOG: natural number detected.";;
    true
  | _; false

dec isRec : shape -> bool.
def isRec s :=
  match s to
  | Circle 10; true
  | _; false

dec add : int -> int -> int.
def add := op(+)

dec main : ().
def main := 
  let _ := isNat 10 in
  print "hello zap!"
