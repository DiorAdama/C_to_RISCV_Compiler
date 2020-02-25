open Batteries
open BatList
open Prog
open Elang
open Cfg
open Elang_run
open Cfg_run
open Rtl
open Rtl_print
open Rtl_run
open Linear
open Builtins
open Utils

let rec exec_linear_instr oc lp fname f st (i: rtl_instr) =
  match i with
  | Rbinop (b, rd, rs1, rs2) ->
    begin match Hashtbl.find_option st.regs rs1,
                Hashtbl.find_option st.regs rs2 with
    | Some v1, Some v2 ->
               Hashtbl.replace st.regs rd (eval_binop b v1 v2);
      OK (None, st)
    | _, _ -> Error (Printf.sprintf "Binop applied on undefined registers (%s and %s)" (print_reg rs1) (print_reg rs2))
    end
  | Runop (u, rd, rs) ->
    begin match Hashtbl.find_option st.regs rs with
      | Some v ->
      Hashtbl.replace st.regs rd (eval_unop u v);
      OK (None, st)
    | _ -> Error (Printf.sprintf "Unop applied on undefined register %s" (print_reg rs))
    end
  | Rconst (rd, i) ->
    Hashtbl.replace st.regs rd i;
    OK (None, st)
  | Rbranch (cmp, r1, r2, s1) ->
    begin match Hashtbl.find_option st.regs r1,
                Hashtbl.find_option st.regs r2 with
    | Some v1, Some v2 ->
      if eval_rtl_cmp cmp v1 v2 then exec_linear_instr_at oc lp fname f st s1 else OK (None, st)
    | _, _ -> Error (Printf.sprintf "Branching on undefined registers (%s and %s)" (print_reg r1) (print_reg r2))
    end
  | Rjmp s -> exec_linear_instr_at oc lp fname f st s
  | Rmov (rd, rs) ->
    begin match Hashtbl.find_option st.regs rs with
    | Some s ->
      Hashtbl.replace st.regs rd s;
      OK (None, st)
    | _ -> Error (Printf.sprintf "Mov on undefined register (%s)" (print_reg rs))
    end
  | Rprint r ->
    begin match Hashtbl.find_option st.regs r with
      | Some s ->
        Format.fprintf oc "%d\n" s;
        OK (None, st)
      | _ -> Error (Printf.sprintf "Print on undefined register (%s)" (print_reg r))
    end
  | Rret r ->
    begin match Hashtbl.find_option st.regs r with
      | Some s -> OK (Some s, st)
      | _ -> Error (Printf.sprintf "Ret on undefined register (%s)" (print_reg r))
    end
  | Rlabel n -> OK (None, st)

and exec_linear_instr_at oc lp fname ({  linearfunbody;  } as f) st i =
  let l = List.drop_while (fun x -> x <> Rlabel i) linearfunbody in
  exec_linear_instrs oc lp fname f st l

and exec_linear_instrs oc lp fname f st l =
  List.fold_left (fun acc i ->
      match acc with
      | Error _ -> acc
      | OK (Some v, st) -> OK (Some v, st)
      | OK (None, st) ->
        exec_linear_instr oc lp fname f st i
    ) (OK (None, st)) l

and exec_linear_fun oc lp st fname f params =
  let regs' = Hashtbl.create 17 in
  match List.iter2 (fun n v -> Hashtbl.replace regs' n v) f.linearfunargs params with
  | exception Invalid_argument _ ->
   Error (Format.sprintf "Linear: Called function %s with %d arguments, expected %d\n" fname
            (List.length params) (List.length f.linearfunargs))
  | _ ->
    let l = f.linearfunbody in
    let regs_save = Hashtbl.copy st.regs in
    let st' = {st with regs = regs' } in
    exec_linear_instrs oc lp fname f st' l >>= fun (v,st) ->
    OK(v, {st with regs = regs_save })

and exec_linear_prog oc lp memsize params =
  let st = init_state memsize in
  find_function lp "main" >>= fun f ->
  let n = List.length f.linearfunargs in
  let params = take n params in
  exec_linear_fun oc lp st "main" f params >>= fun (v, st) ->
  OK v


