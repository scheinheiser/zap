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

dec ignore : 'a -> ().
def ignore _ := ()

dec caller : ('a -> 'b) -> 'a -> 'b.
def caller f v := f v

def patTest (5, false, "hi") (10 :: rest) five :=
  print five

% def testing := fun f => fun x => f (x + 1)

% this should be demoted to an int -> int function.
dec besting : 'a -> int.
def besting n := n + 1

% this should cause an error due to the assumption that 'a is completely general.
dec besting : forall 'a. 'a -> int.
def besting n := n + 1

dec main : ().
def main := 
  % let _ := caller (fun n => n + 1) "hi" in
  let str := "hello, zap" in
  print str
