@module TypeDeclSyntax

% a simple alias to give users more context about a type.
@alias IsWorker := Bool

% the type signature is declared similarly to let expressions.
@sum Basket  : (price: Type) -> Type =
  | Apple    : price -> Basket % explicit type signature for variants.
  | Banana   : price -> Basket
  | Bag      : Basket % empty variant
  | ( .: )   : Basket -> Basket -> Basket % recursive, operator constructor.

% the constructor is declared before the definition
@record Person := MkPerson
  { name   : String
  ; age    : Int
  ; status : IsWorker
  ; basket : Basket
  }

% sums can contain only operator constructors.
@sum List : (a: Type) -> Type =
  | ( [] ) : List A
  | ( :: ) : A -> List A

dec unsafeHead : (a : Type) -> List a -> a
def unsafeHead _ (h :: _) := h

dec getPrice : (price : Type) -> Basket price -> p
def getPrice (Apple p)  := p
def getPrice (Banana p) := p
def getPrice (l .: r)   := getPrice l + getPrice r

dec addEmployee : String -> Int -> Basket -> Person
def addEmployee name age b := MkPerson { name = name; age = age; status = true; basket = b}
