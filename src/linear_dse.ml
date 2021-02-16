open Rtl
open Linear
open Linear_liveness
open Batteries
open BatList
open Prog
open Utils
open Report
open Linear_print
open Report
open Options

let dse_instr (ins: rtl_instr) live =
   [ins]


let dse_fun live {linearfunargs; linearfunbody; linearfuninfo; } =
  let body =
    linearfunbody
    |> List.mapi (fun i ins -> dse_instr ins (Hashtbl.find_default live i Set.empty))
    |> List.concat in
  { linearfunargs; linearfunbody = body; linearfuninfo; }


let dse_prog p live =
  if !Options.no_linear_dse
  then p
  else
  List.map (fun (fname,gdef) ->
      match gdef with
        Gfun f ->
        let live = Hashtbl.find_default live fname (Hashtbl.create 17, Hashtbl.create 17) |> snd in
        let f = dse_fun live f in
        (fname, Gfun f)
      ) p

let pass_linear_dse linear lives =
  let linear = dse_prog linear lives in
  record_compile_result "DSE";
  dump (!linear_dump >*> fun s -> s ^ "1")
    (fun oc -> dump_linear_prog oc (Some lives)) linear
    (fun file () -> add_to_report "linear-after-dse" "Linear after DSE"
        (Code (file_contents file)));
  OK linear
