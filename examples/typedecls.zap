@module TypeDeclSyntax

% a simple alias to give users more context about a type.
@alias IsWorker := Bool

% the type signature is declared similarly to let expressions.
@union Basket : (price: Type) -> Type =
  | Apple    : price -> Basket price % explicit type signature for variants.
  | Banana   : price -> Basket price
  | Bag      : Basket price % empty variant
  | ( .: )   : Basket price -> Basket price -> Basket price % recursive, operator constructor.

% unions can contain only operator constructors.
@union List : (a: Type) -> Type =
  | Nil    : List a
  | ( @: ) : a -> List a -> List a

% the constructor is declared before the definition
@record Person := MkPerson
  { name   : String
  ; age    : Int
  ; status : IsWorker
  ; basket : Basket
  }

dec unsafeHead : (a : Type) -> List a -> a
% def unsafeHead _ (h :: _) := h

dec getPrice : (price : Type) -> Basket price -> p
def getPrice (Apple p)  := p
def getPrice (Banana p) := p
def getPrice (l .: r)   := getPrice l + getPrice r
def getPrice Bag        := 0

dec addEmployee : String -> Int -> Basket -> Person
def addEmployee name age b := MkPerson { name = name; age = age; status = true; basket = b}
