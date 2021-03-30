open Batteries
open BatList
open Elang
open Cfg
open Elang_run
open Cfg_run
open Rtl
open Rtl_print
open Utils
open Builtins
open Prog

type state = {
  mem: Mem.t;
  regs: (reg, int) Hashtbl.t;
}

let init_state memsize =
  {
    mem = Mem.init memsize;
    regs = Hashtbl.create 17
  }

let eval_rtl_cmp = function
    Rcle -> (<=)
  | Rclt -> (<)
  | Rcge -> (>=)
  | Rcgt -> (>)
  | Rceq -> (=)
  | Rcne -> (<>)

let rec exec_rtl_instr oc rp rtlfunname f st (i: rtl_instr) =
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
      (if eval_rtl_cmp cmp v1 v2 then exec_rtl_instr_at oc rp rtlfunname f st s1 else OK (None, st))
    | _, _ -> Error (Printf.sprintf "Branching on undefined registers (%s and %s)" (print_reg r1) (print_reg r2))
    end
  | Rjmp s -> exec_rtl_instr_at oc rp rtlfunname f st s
  | Rmov (rd, rs) ->
    begin match Hashtbl.find_option st.regs rs with
    | Some s ->
      Hashtbl.replace st.regs rd s;
      OK (None, st)
    | _ -> Error (Printf.sprintf "Mov on undefined register (%s)" (print_reg rs))
    end
  | Rret r ->
    begin match Hashtbl.find_option st.regs r with
      | Some s -> OK (Some s, st)
      | _ -> Error (Printf.sprintf "Ret on undefined register (%s)" (print_reg r))
    end

(*  
  | Rprint r ->
    begin match Hashtbl.find_option st.regs r with
      | Some s ->
        Format.fprintf oc "%d\n" s;
        OK (None, st)
      | _ -> Error (Printf.sprintf "Print on undefined register (%s)" (print_reg r))
    end
*)
  | Rlabel n -> OK (None, st)

  | Rcall (ret, "print", reg_args) -> (
      let f_fold arg_val_list reg_argi = 
        arg_val_list >>= fun arg_val_list -> 
          begin match Hashtbl.find_option st.regs reg_argi with
          | Some s -> OK (arg_val_list @ [s])
          | _ -> Error (Printf.sprintf "function %s called on an undefined register %s" "print" (print_reg reg_argi))
          end
      in
      List.fold_left f_fold (OK []) reg_args >>= fun argval_list -> 
      do_builtin oc st.mem "print" argval_list >>= fun ans -> 
      OK (None, st)
  )

  | Rcall (ret, "print_char", [reg_arg]) -> (
    match Hashtbl.find_option st.regs reg_arg with
    | Some s -> 
        do_builtin oc st.mem "print" [s] >>= fun ans -> 
        OK (None, st)
    | None -> Error (Printf.sprintf "function %s called on an undefined register %s" "print_char" (print_reg reg_arg))
  )

  | Rcall (ret, fname, reg_args) -> (
      let f_fold arg_val_list reg_argi = 
        arg_val_list >>= fun arg_val_list -> 
          begin match Hashtbl.find_option st.regs reg_argi with
          | Some s -> OK (arg_val_list @ [s])
          | _ -> Error (Printf.sprintf "function %s called on an undefined register %s" fname (print_reg reg_argi))
          end
      in
      List.fold_left f_fold (OK []) reg_args >>= fun argval_list -> 
        find_function rp fname >>= fun func_def ->
        exec_rtl_fun oc rp st fname func_def argval_list >>= fun (ans, st) -> (
          match ret, ans with 
            | None, None -> OK (None, st)
            | Some rd, Some i -> 
                Hashtbl.replace st.regs rd i;
                OK (None, st)
            | _ -> Error (Printf.sprintf "Expected a return value from function %s" fname)
        )     
  )
  | _ -> Error "Unrecognized RTL instruction"
        
and exec_rtl_instr_at oc rp rtlfunname ({ rtlfunbody;  } as f: rtl_fun) st i =
  match Hashtbl.find_option rtlfunbody i with
  | Some l -> exec_rtl_instrs oc rp rtlfunname f st l
  | None -> Error (Printf.sprintf "Jump to undefined label (%s_%d)" rtlfunname i)

and exec_rtl_instrs oc rp rtlfunname f st l =
  List.fold_left (fun acc i ->
      match acc with
      | Error _ -> acc
      | OK (Some v, st) -> OK (Some v, st)
      | OK (None, st) ->
        exec_rtl_instr oc rp rtlfunname f st i
    ) (OK (None, st)) l

and exec_rtl_fun oc rp st rtlfunname f params =
  let regs' = Hashtbl.create 17 in
  match List.iter2 (fun n v -> Hashtbl.replace regs' n v) f.rtlfunargs params with
  | exception Invalid_argument _ ->
    Error (Format.sprintf "RTL: Called function %s with %d arguments, expected %d\n"
             rtlfunname
             (List.length params)
             (List.length f.rtlfunargs)
          )
  | _ ->
    match Hashtbl.find_option f.rtlfunbody f.rtlfunentry with
    | None ->
      Error (Printf.sprintf "Unknown node (%s_%d)" rtlfunname f.rtlfunentry)
    | Some l ->
      let regs_save = Hashtbl.copy st.regs in
      let st' = {st with regs = regs'; } in
      exec_rtl_instrs oc rp rtlfunname f st' l >>= fun (v, st) ->
      OK(v, {st with regs = regs_save })

and exec_rtl_prog oc rp memsize params =
  let st = init_state memsize in
  find_function rp "main" >>= fun f ->
  let n = List.length f.rtlfunargs in
  let params = take n params in
  exec_rtl_fun oc rp st "main" f params >>= fun (v, st) ->
  OK v



  