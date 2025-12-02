% TODO: Change this to revolve around actual code.
@module UserOperators

dec baz : string -> atom.
dec bar : bool -> string.
dec foo : int -> bool.

dec ignore : 'a -> ().
def ignore _ := ()

% dec id : forall 'a. 'a -> 'a.
def id x := x

dec ( & ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c.
def ( & ) f g x := f (g x)
@rassoc 9 &

dec ( $ ) : ('a -> 'b) -> 'a -> 'b.
def ( $ ) f v := f v
@rassoc 1 $

dec main : ().
def main := 
  ignore (id 42);
  ignore (foo $ 10);
  ignore (baz & bar & foo $ 10);
  print "hello zap"
