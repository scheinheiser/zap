@module MultiDefTest

@import Std

dec* map : ('a -> 'b) -> 'a list -> 'b list.
def* map _ [] := [];
def* map f (x :: xs) := f x :: map f xs;

def map hi := print hi;

dec bap : int -> bool.
def bap n := 
  if n > 10
  then true
  else false
;;

dec main : ().
def main := print "hello, zap!";
