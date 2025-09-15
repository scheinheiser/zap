type t = Location.t option * string

let pp_err out ((loc, msg) : t) =
  match loc with
  | Some l -> Format.fprintf out "[ERROR] %a; %s@." Location.pp_location l msg
  | None -> Format.fprintf out "[ERROR]: %s@." msg
;;

let pp_warning out ((loc, msg) : t) =
  match loc with
  | Some l -> Format.fprintf out "[WARNING] %a; %s@." Location.pp_location l msg
  | None -> Format.fprintf out "[WARNING]: %s@." msg
;;

let format_err ((loc, msg) : t) : string =
  match loc with
  | Some l -> Format.asprintf "[ERROR] %a; %s@." Location.pp_location l msg
  | None -> Format.asprintf "[ERROR]: %s@." msg
;;

let report_err (err : t) =
  pp_err Format.err_formatter err;
  exit 1
;;

let report_warning (warning : t) = pp_warning Format.std_formatter warning
