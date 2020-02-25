open Batteries
open Rtl_print
open Linear
open Prog
open Utils


let dump_linear_fun oc lives lfname l =
  Format.fprintf oc "%s(%s):\n" lfname
    (String.concat ", " $ List.map print_reg l.linearfunargs);
  let lives = match lives with
    | None -> None
    | Some lives -> 
      (Hashtbl.find_option lives lfname)
  in
  dump_rtl_node lfname lives oc l.linearfunbody

let dump_linear_prog oc lives lp =
  dump_prog (fun oc -> dump_linear_fun oc lives) oc lp
