@module TClass

trait 'a Num with
  dec (+) : 'a -> 'a -> 'a.
  dec (-) : 'a -> 'a -> 'a.
  dec (*) : 'a -> 'a -> 'a.
  dec (/) : 'a -> 'a -> 'a.

  dec compare : 'a -> 'a -> bool.
;;

instance int Num where
  def (+) l r := l + r;
  def (-) l r := l - r;
  def (*) l r := l * r;
  def (/) l r := l / r;

  def compare l r := l >= r
;;

dec (^+) <'a impl Num; 'b impl Num>: 'a -> 'b -> 'a.
