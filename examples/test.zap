@module Testing
@import Bruh with (filter, Hi, Sup)

type myAlias := int

type list :=
  | Cons ~ 'a
  | Nil

type myRecord :=
  { field1 ~ int
  ; field2 ~ string
  ; aliasField ~ myAlias
  ; variantField ~ myVariant
  }

% Variant1 {100}
% myRecord {field1 = 10; field2 = "hi"; aliasField = 100; variantField = Variant2 "hi"}

dec show : int -> string.
dec length : string -> int.

% let-binding function
dec lamTest : int -> bool.
def lamTest num :=
  let test_lambda : int -> string = fun n -> show n in
  Cons 10 in
  length (test_lambda num) > 10
  % (test_lambda num) = "hi"
;;

dec caller : ('a -> 'b) -> 'a -> 'b.
def caller f v := f v;;

def shadowWarn :=
  let k := 10 in
  let k := 5 + k in
  k
;;

def failingUnification n :=
  n + 1 in
  n + 2 in
  (n = 4) || (n = 10) in

  % should fail
  % n :: ["hi"] in
  n
;;

dec patTest : (int, bool, string) -> int list -> string -> ().
def patTest (5, false, "hi") (10 :: rest) five :=
  print five
;;

dec main : ().
def main := 
  let str := "hello, zap" in
  print str
;;
