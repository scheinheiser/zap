% this is a comment.

%{
  this is
  a multiline
  comment
%}

% zap uses modules.
@module SyntaxDemo

@import Std without (filter, map)
% @import Data.List with (filter, map)

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

dec* filter : ('a -> bool) -> ['a] -> ['a].
def* filter _ [] := [];
def* filter p (x :: xs) :=
  if p x
  then x :: filter p xs
  else filter p xs
;;

% pattern guards
dec* cfilter : ('a -> bool) -> ['a] -> ['a].
def* cfilter _ [] := [];
def* cfilter p (x :: xs) : when (p x) = x :: cfilter p xs;
def* cfilter p (_ :: xs) := cfilter p xs
;;

% block pattern guards
dec* clampNums : [int] -> [int].
def* clampNums [] := [];
def* clampNums (x :: xs) := x :: clampNums xs;
def* clampNums (x :: xs)
  : when {
    x < 10 && x > 0
  } = x :: clampNums xs;
def* clampNums (_ :: xs) := clampNums xs
;;

dec* getLast : ['a] -> 'a.
def* getLast [] := fail "empty list";
def* getLast [x] := x;
def* getLast (_ :: xs) := getLast xs
;;

% eta reduction
dec map : ('a -> 'b) -> ['a] -> ['b].
def map := go
  with
    % you can omit the dec for functions/variables in with-blocks
    def* go _ [] := [];
    def* go f (x :: xs) := f x :: go f xs;
;;

% let-binding function
dec lamTest : int -> bool.
def lamTest num :=
  % you can have let-bindings act as functions through the use of lambdas
  let test_lambda : int -> string = fun n => show n in
  length (test_lambda num) > 10
;;

dec sayHello : string -> ().
def sayHello name :=
  %{
    you can explicitly declare the type, or allow it to be inferred - this would also be valid:
    let greeted := "hello" <> name in
  %}
  let greeted : string = "hello" <> name in
  print greeted
;;

% partial application
dec addOne : int -> int.
def addOne := op+ 1;

% atoms
dec okOrNot : bool -> atom.
def okOrNot true := @ok;
def okOrNot false := @fail;

dec main : ().
def main := print "hello world!";
