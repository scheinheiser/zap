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
  let test_lambda : int -> string = fun n => show n in
  let _ := Cons 10 in
  let _ := length (test_lambda num) > 10 in
  (test_lambda num) = "hi"
;;

dec caller : ('a -> 'b) -> 'a -> 'b.
def caller f v := f v;;

def failingUnification n :=
  let _ := n + 1 in
  let _ := n + 2 in
  let _ := (n = 4) || (n = 10) in

  % should fail
  % n :: ["hi"] in
  n
;;

def patTest (5, false, "hi") (10 :: rest) five :=
  print five
;;

dec main : ().
def main := 
  let _ := caller (fun n => n + 1) "hi" in
  let str := "hello, zap" in
  print str
;;
