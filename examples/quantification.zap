@module TypeQuantification

dec id : 'a -> 'a.
def id x := x

def bump n := n + 1

%{
dec const : forall 'a 'b. 'a -> 'b -> 'a.
def const x _ := x

dec* map : forall 'a 'b. ('a -> 'b) -> 'a list -> 'a list.
def* map _ [] := []
def* map f (x :: xs) := x :: map f xs
%}

dec call42 : (int -> int) -> int.
def call42 f := f 42

dec main : ().
def main := 
  let _ := call42 id in
  print "hello zap!"
