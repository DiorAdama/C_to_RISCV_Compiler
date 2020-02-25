open Rtl
open Linear
open Linear_liveness
open Batteries
open BatList
open Prog
open Utils

let dse_instr (ins: rtl_instr) live =
   [ins]


let dse_fun live {linearfunargs; linearfunbody; linearfuninfo; } =
  let body =
    linearfunbody
    |> List.mapi (fun i ins -> dse_instr ins (Hashtbl.find_default live i Set.empty))
    |> List.concat in
  { linearfunargs; linearfunbody = body; linearfuninfo; }


let dse_prog p live =
  List.map (fun (fname,gdef) ->
      match gdef with
        Gfun f ->
        let live = Hashtbl.find_default live fname (Hashtbl.create 17, Hashtbl.create 17) |> snd in
        let f = dse_fun live f in
        (fname, Gfun f)
      ) p
