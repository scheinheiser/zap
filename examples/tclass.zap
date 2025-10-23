% TODO: Change this to revolve around actual code, and work on syntax.
@module TClass

class Ord 'a : minimal (=) | (/=) =
  dec (=) : 'a -> 'a -> bool.
  def (=) l r := not (l /= r)

  dec (/=) : 'a -> 'a -> bool.
  def (/=) l r := not (l = r);
;;

class Num 'a :=
  dec (+) : 'a -> 'a -> 'a.
  dec (-) : 'a -> 'a -> 'a.
  dec (*) : 'a -> 'a -> 'a.
  dec (/) : 'a -> 'a -> 'a.

  dec compare : Ord 'a => 'a -> 'a -> bool.
;;

instance Num int where
  def (+) l r := l + r;
  def (-) l r := l - r;
  def (*) l r := l * r;
  def (/) l r := l / r;

  def compare l r := l >= r
;;

dec (^=) : (Ord 'a, Ord 'b) => 'a -> 'b -> bool.

dec (^+) : Num 'a => 'a -> 'b -> 'a.
