% TODO: Change this to revolve around actual code.
@module MultiDefTest

@import Std

dec* map : ('a -> 'b) -> 'a list -> 'b list.
def* map _ [] := []
def* map f (x :: xs) := f x :: map f xs

dec map : ('a, 'b) -> ('a -> 'b) -> ('b, 'b).
def map (l, r) f := (f l, r)

dec main : ().
def main := print "hello, zap!"
