type t = Location.t option * string

val pp_err : Format.formatter -> t -> unit
val report_err : t -> 'a
