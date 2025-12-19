@module DepedentTypesShowcase

% explicit arguments
dec id_exp : (A : Type) -> A -> A.
def id_exp _ x := x

% implicit arguments. they must be at the beginning of the arrow type.
% dec id_imp : < A : Type base > -> A -> A.
def id_imp x := x

dec main : IO ().
def main := print "hello, zap!"
