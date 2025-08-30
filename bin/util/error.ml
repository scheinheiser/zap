type t = Location.t option * string

let pp_err out ((loc, msg) : t) =
  match loc with
  | Some l -> Format.fprintf out "[ERROR] %a; %s@." Location.pp_location l msg
  | None -> Format.fprintf out "[ERROR]: %s@." msg
;;

let report_err (err : t) =
  pp_err Format.err_formatter err;
  exit 1
;;
