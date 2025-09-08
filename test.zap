@module Testing
@import Bruh with (filter, Hi, Sup)

dec multiArgs : int -> string -> () -> bool.
def multiArgs one two three :=
  print one in
  print two in
  print three in
  print "hi" in
  3 * 2 + 10
;;

dec filter : 'a -> bool -> ['a] -> ['a].
def filter p (x :: xs) :=
  let test_lambda : int -> string = lam n -> show n in
  if p x
  then x :: filter p xs
  else filter p xs
;;

dec getLast : ['a] -> 'a.
def getLast [] := fail "empty list";
def getLast [] := x;
def getLast (_ :: xs) := getLast xs
;;
