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
  if true
  then ignore 10
  else {
    ignore 50;;
    print "hi";;
    let [] := ["sup"; "boink"] in
    let (10, b, @boink) := (20, "burh", @stoink) in
    ignore (foo $ 10);;
    ignore (baz & bar & foo $ 10)
  }
