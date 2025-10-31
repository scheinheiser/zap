% TODO: Change this to revolve around actual code.
@module UserOperators

dec baz : string -> atom.
dec bar : bool -> string.
dec foo : int -> bool.

dec ignore : 'a -> ().
def ignore _ := ()

dec ( & ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c.
def ( & ) f g x := f (g x)
@rassoc 9 &

dec ( $ ) : ('a -> 'b) -> 'a -> 'b.
def ( $ ) f v := f v
@rassoc 1 $

% def testing := fun f => fun x => f (x + 1)

dec main : ().
def main := 
  print "hi";
  print "hello zap!";
  let () := ignore (baz & bar & foo $ 10) in
  ()
