@module OperatorTest

dec pash : string -> atom.
dec push : bool -> string.
dec pop : int -> bool.

dec ( & ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c.
def ( & ) f g x := f (g x);
@rassoc 9 &

dec ( $ ) : ('a -> 'b) -> 'a -> 'b.
def ( $ ) f v := f v;
@rassoc 1 $

dec main : ().
def main := 
  (pash & push & pop) 10 in
  print "hello zap!"
;;
