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

let rec exec_linear_instr oc lp fname f st (i: rtl_instr) (sp: int)=
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
      if eval_rtl_cmp cmp v1 v2 then exec_linear_instr_at oc lp fname f st s1 sp else OK (None, st)
    | _, _ -> Error (Printf.sprintf "Branching on undefined registers (%s and %s)" (print_reg r1) (print_reg r2))
    end
  | Rjmp s -> exec_linear_instr_at oc lp fname f st s sp
  | Rmov (rd, rs) ->
    begin match Hashtbl.find_option st.regs rs with
    | Some s ->
      Hashtbl.replace st.regs rd s;
      OK (None, st)
    | _ -> Error (Printf.sprintf "Mov on undefined register (%s)" (print_reg rs))
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
  | Rret r ->
    begin match Hashtbl.find_option st.regs r with
      | Some s -> OK (Some s, st)
      | _ -> Error (Printf.sprintf "Ret on undefined register (%s)" (print_reg r))
    end
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
        do_builtin oc st.mem "print_char" [s] >>= fun ans -> 
        OK (None, st)
    | None -> Error (Printf.sprintf "@ linear_run: function %s called on an undefined register %s" "print_char" (print_reg reg_arg))
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
      find_function lp fname >>= fun func_def ->
      exec_linear_fun oc lp st fname func_def argval_list sp >>= fun (ans, st) -> (
        match ret, ans with 
          | None, None -> OK (None, st)
          | Some rd, Some i -> 
              Hashtbl.replace st.regs rd i;
              OK (None, st)
          | _ -> Error (Printf.sprintf "Expected a return value from function %s" fname)
      )     
  )

  | Rstk (rd, offs) -> (
      Hashtbl.replace st.regs rd (sp + offs);
      OK (None, st)
  )
  
  | Rload (rd, rs, sz) -> (
      match Hashtbl.find_option st.regs rs with 
        | None -> Error (Printf.sprintf "@rtl_run Rload called on undefined register rs = %s" (print_reg rs))
        | Some addr -> 
            Mem.read_bytes_as_int st.mem addr sz >>= fun v -> 
            Hashtbl.replace st.regs rd v;
            OK (None, st)
  )

  | Rstore (rd, rs, sz) -> (
      match Hashtbl.find_option st.regs rs with 
        | None -> Error (Printf.sprintf " @rtl_run Rstore called on undefined register rs = %s" (print_reg rs)) 
        | Some v -> 
            match Hashtbl.find_option st.regs rd with 
              | None -> Error (Printf.sprintf "@rtl_run Rstore called on undefined register rd = %s" (print_reg rd))
              | Some addr -> 
                  let byte_list = split_bytes sz v in 
                  Mem.write_bytes st.mem addr byte_list >>= fun _ -> 
                    OK (None, st)
  )

  | _ -> Error "Unrecognized RTL instruction"


and exec_linear_instr_at oc lp fname ({  linearfunbody;  } as f) st i (sp: int)=
  let l = List.drop_while (fun x -> x <> Rlabel i) linearfunbody in
  exec_linear_instrs oc lp fname f st l sp

and exec_linear_instrs oc lp fname f st l (sp: int) =
  List.fold_left (fun acc i ->
      match acc with
      | Error _ -> acc
      | OK (Some v, st) -> OK (Some v, st)
      | OK (None, st) ->
        exec_linear_instr oc lp fname f st i sp
    ) (OK (None, st)) l

and exec_linear_fun oc lp st fname f params (sp: int) =
  let sp = sp - f.linearfunstksz in
  let regs' = Hashtbl.create 17 in
  match List.iter2 (fun n v -> Hashtbl.replace regs' n v) f.linearfunargs params with
  | exception Invalid_argument _ ->
   Error (Format.sprintf "Linear: Called function %s with %d arguments, expected %d\n" fname
            (List.length params) (List.length f.linearfunargs))
  | _ ->
    let l = f.linearfunbody in
    let regs_save = Hashtbl.copy st.regs in
    let st' = {st with regs = regs' } in
    exec_linear_instrs oc lp fname f st' l sp >>= fun (v,st) ->
    OK(v, {st with regs = regs_save })

and exec_linear_prog oc lp memsize params =
  let st = init_state memsize in
  find_function lp "main" >>= fun f ->
  let n = List.length f.linearfunargs in
  let params = take n params in
  exec_linear_fun oc lp st "main" f params memsize >>= fun (v, st) ->
  OK v


