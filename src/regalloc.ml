open Batteries
open BatList
open BatEnum
open Prog
open Linear
open Rtl
open Linear_liveness

type regalloc_decision = Spill of int | NoSpill of int
type loc = Reg of int | Stk of int




   let regalloc_fun (f: linear_fun) (live_in, live_out) all_colors =
   (Hashtbl.create 0, 0)


let regalloc lp lives all_colors =
  let allocations =
    Hashtbl.create 17 in

  List.iter (function (fname,Gfun f) ->
      begin match Hashtbl.find_option lives fname with
      | Some (live_in, live_out) ->
        let (allocation, curstackslot) = regalloc_fun f (live_in, live_out) all_colors in
        Hashtbl.replace allocations fname (allocation, curstackslot)
      | None -> ()
      end
    ) lp;
  allocations
