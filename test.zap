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

% let-binding function
dec lamTest : int -> bool.
def lamTest num :=
  let test_lambda : int -> string = lam n -> show n in
  length (test_lambda num) > 10
;;

def patTest (5, false, "hi") (10 :: rest) (five) :=
  print five
;;
