@module TypeQuantification

dec id : forall 'a. 'a -> 'a.
def id x := x

dec const : forall 'a 'b. 'a -> 'b -> 'a.
def const x _ := x

dec* map : forall 'a 'b. ('a -> 'b) -> 'a list -> 'b list.
def* map _ [] := []
def* map f (x :: xs) := f x :: map f xs

dec main : ().
def main := 
  let _ := const 10 "hi" in
  let _ := const @ruh true in
  print "hello zap!"
