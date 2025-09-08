@module Testing
@import Bruh

dec multiArgs : int -> string -> () -> bool.
def multiArgs one two three :=
  print one in
  print two in
  print three in
  3 * 2 + 10
;;

def filter p (x :: xs) :=
  let test_lambda : int -> string = lam n -> show n in
  if p x
  then x :: filter p xs
  else filter p xs
;;
