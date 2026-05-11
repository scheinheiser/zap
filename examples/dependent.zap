@module DepedentTypesShowcase

% explicit arguments
dec id_exp : (A : Type) -> A -> A.
def id_exp _ x := x

% implicit arguments. they must be at the beginning of the arrow type.
% dec id_imp : { A : Type } -> A -> A.
def id_imp x := x

dec match_test : A -> A -> A.
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

dec main : IO ().
def main := print "hello, zap!"
