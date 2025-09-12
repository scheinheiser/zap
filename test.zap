@module Testing
@import Bruh with (filter, Hi, Sup)

type myAlias := int

type myVariant :=
  | Variant1 ~ int
  | Variant2 ~ string

type myRecord :=
  { field1 ~ int
  ; field2 ~ string
  ; aliasField ~ myAlias
  ; variantField ~ myVariant
  }

dec multiArgs : int -> string -> () -> bool.
def multiArgs one two three :=
  print one in
  print two in
  print three in
  print "hi" in
  3 * 2 + 10
;;

% let-binding function
dec lamTest : int -> bool.
def lamTest num :=
  % you can have let-bindings act as functions through the use of lambdas
  let test_lambda : int -> string = lam n -> show n in
  (length (test_lambda num) > 10)
;;
