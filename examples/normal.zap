module NormalForm

dec chooseType : Bool -> Type
def chooseType true := Int
def chooseType false := String

dec val : chooseType true
def val := 5

%{
this will become (under desugaring):
dec chooseType : Bool -> Type
def chooseType :=
  fun __match__1 ->
    match __match__1 to
    | true => Int
    | false => String

say I have this expression:
dec val : chooseType true
def val := 10

from an initial substitution (which I already have in my `normalise` function), we get:
dec val : (match __match__1 to | true => Int | false => String)
def val := 10

so then I'd need an evaluation to normal form (somehow) to get this:
dec val : Int
def val := 10

which would then be typechecked and found to be true.
%}
