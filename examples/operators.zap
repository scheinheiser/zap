% TODO: Change this to revolve around actual code.
@module UserOperators

dec pash : string -> atom.
dec push : bool -> string.
dec pop : int -> bool.

dec ( & ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c.
def ( & ) f g x := f (g x);
@rassoc 9 &

dec ( $ ) : ('a -> 'b) -> 'a -> 'b.
def ( $ ) f v := f v;
@rassoc 1 $

%{
def ifTest :=
  if true
  then 10
  else 4
;;
%}

dec myFunc : int.
def myFunc :=
  let n : int = {
    let one := 1 in
    let two := 2 in
    one + two
  } in n
;;

dec main : ().
def main := 
  (pash & push & pop) 10 in
  print "hello zap!"
;;
