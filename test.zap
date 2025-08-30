@module Testing
@import Bruh

dec multiArgs : int -> string -> () -> bool.
def multiArgs one two three :=
  print one;
  print two;
  print three;
  3 * (2 + 10)
;;

def filter p (x :: xs) :=
  if p x
  then x :: (filter p xs)
  else filter p xs;
;;
