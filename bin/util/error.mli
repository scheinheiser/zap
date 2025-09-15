type t = Location.t option * string

val pp_err : Format.formatter -> t -> unit
val pp_warning : Format.formatter -> t -> unit
val format_err : t -> string
val report_err : t -> 'a
val report_warning : t -> unit
