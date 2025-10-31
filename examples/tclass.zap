% TODO: Change this to revolve around actual code, and work on syntax.
@module TClass

class 'a Ord : minimal (=) | (/=) =
  dec (=) : 'a -> 'a -> bool.
  def (=) l r := not (l /= r)

  dec (/=) : 'a -> 'a -> bool.
  def (/=) l r := not (l = r)
;;

class 'a Num :=
  dec (+) : 'a -> 'a -> 'a.
  dec (-) : 'a -> 'a -> 'a.
  dec (*) : 'a -> 'a -> 'a.
  dec (/) : 'a -> 'a -> 'a.

  dec compare : Ord 'a => 'a -> 'a -> bool.
;;

instance int Num where
  def (+) l r := l + r
  def (-) l r := l - r
  def (*) l r := l * r
  def (/) l r := l / r

  def compare l r := l >= r
;;

dec (^=) : ['a Ord; 'b Ord] => 'a -> 'b -> bool.

dec (^+) : ['a Num] => 'a -> 'b -> 'a.
