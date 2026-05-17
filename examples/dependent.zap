@module DepedentTypesShowcase

% explicit arguments
dec id : (a : Type) -> a -> a
def id _ x := x

% implicit arguments. they must be at the beginning of the arrow type.
% dec id' : { a : Type } -> a -> a
def id' x := x

dec match_test : (a: Type) -> a -> a
def match_test _ n :=
  match n to
  | Int => 1
  | Float => 2.0
  | String => "3" 
  | Char => '4'
  | Bool => true
  | Atom => @six 
  | () => ()
  | _ => n

dec main : IO Unit 
def main := print "hello, zap!"
