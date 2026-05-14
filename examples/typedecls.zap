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
@record Person : (price: Type) -> Type = MkPerson
  { name   : String
  ; age    : Int
  ; status : IsWorker
  ; basket : Basket price
  }

dec unsafeHead : (a : Type) -> List a -> a
% def unsafeHead _ (h :: _) := h

dec getPrice : (price : Type) -> Basket price -> p
def getPrice _ (Apple p)  := p
def getPrice _ (Banana p) := p
def getPrice _ (l .: r)   := getPrice l + getPrice r
def getPrice _ Bag        := 0

dec addEmployee : (price: Type) -> String -> Int -> Basket price -> Person price
def addEmployee _ name age b := MkPerson { name = name; age = age; status = true; basket = b}

dec personBasketPrice : (a : Type) -> Person a -> a
def personBasketPrice price p := getPrice price p.basket

dec addItem : (a : Type) -> Person a -> Basket a -> Person a
def addItem _ p item := { p where basket = item .: p.basket }
