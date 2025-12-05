@module DepedentTypesShowcase

% explicit arguments
dec id_exp : (A : Type base) -> A -> A.
def id_exp A x := x

% implicit arguments. they must be at the beginning of the arrow type.
dec id_imp : < A : Type base > -> A -> A.
def id_imp x := x

dec main : IO ().
def main := print "hello, zap!"
