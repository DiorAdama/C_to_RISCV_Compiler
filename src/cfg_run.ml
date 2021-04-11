open Prog
open Elang
open Elang_run
open Batteries
open BatList
open Cfg
open Utils
open Builtins

let rec eval_cfgexpr (e: expr) st cp oc (sp: int): (int* int Prog.state) res =
  match e with
    | Ebinop(b, e1, e2) ->
        eval_cfgexpr e1 st cp oc sp >>= fun (v1, st) ->
        eval_cfgexpr e2 st cp oc sp >>= fun (v2, st) ->
        let v = eval_binop b v1 v2 in
        OK (v, st)
    | Eunop(u, e) ->
        eval_cfgexpr e st cp oc sp >>= fun (v1, st) ->
        let v = (eval_unop u v1) in
        OK (v, st)
    | Eint i -> OK (i, st)
    | Evar s ->
        begin match Hashtbl.find_option st.env s with
          | Some v -> OK (v, st)
          | None -> Error (Printf.sprintf "Unknown variable %s\n" s)
        end
    | Ecall (fname, fargs) ->
        let f_fold argums expri = (
          argums >>= fun (argums, sti) ->
          eval_cfgexpr expri sti cp oc sp >>= fun (ans_i, sti) -> 
            OK ((argums@[ans_i]), sti)
        ) in
        (List.fold_left f_fold (OK ([],st) ) fargs) >>= fun (arguments, st) ->
            find_function cp fname >>= fun func_def ->
            eval_cfgfun oc st fname func_def arguments cp sp >>= fun (ans, st) -> 
              option_to_res_bind ans  ("Error in cfg_run.eval_cfgexpr Ecall " ^ fname) (fun ans -> OK (ans, st))
      
    | Estk i -> OK (sp+i, st)
    | Eload (e, sz) -> (
        eval_cfgexpr e st cp oc sp >>= fun (addr, st) -> 
        Mem.read_bytes_as_int st.mem addr sz >>= fun ans -> 
          OK (ans, st)
    )


and eval_cfginstr oc st ht (n: int) cp (sp: int): (int * int state) res=
  match Hashtbl.find_option ht n with
    | None -> Error (Printf.sprintf "Invalid node identifier\n")
    | Some node ->
        match node with
          | Cnop succ ->
              eval_cfginstr oc st ht succ cp sp
          | Cassign(v, e, succ) ->
              eval_cfgexpr e st cp oc sp >>= fun (val_assign, st) ->
              Hashtbl.replace st.env v val_assign;
              eval_cfginstr oc st ht succ cp sp
          | Ccmp(cond, i1, i2) -> 
              eval_cfgexpr cond st cp oc sp >>= fun (i,st) ->
              if i = 0 
                then eval_cfginstr oc st ht i2 cp sp
              else 
                eval_cfginstr oc st ht i1 cp sp
          | Creturn(e) ->
              eval_cfgexpr e st cp oc sp >>= fun (e, st) ->
              OK (e, st)
(*
          | Cprint(e, succ) ->
              eval_cfgexpr e st cp oc >>= fun (e, st) ->
              Format.fprintf oc "%d\n" e;
              eval_cfginstr oc st ht succ cp
*)
          | Ccall ("print_int", [c], succ) -> 
              eval_cfgexpr c st cp oc sp >>= fun (c, st) ->
              do_builtin oc st.mem "print_int" [c]  >>= fun ans -> 
                eval_cfginstr oc st ht succ cp sp

          | Ccall ("print", pargs, succ) -> 
              let f_fold argums expri = (
                argums >>= fun (argums, sti) ->
                eval_cfgexpr expri sti cp oc sp >>= fun (ans_i, sti) -> 
                  OK ((argums@[ans_i]), sti)
              ) in
              (List.fold_left f_fold (OK ([],st) ) pargs) >>= fun (arguments, st) ->
              do_builtin oc st.mem "print" arguments >>= fun ans -> 
                eval_cfginstr oc st ht succ cp sp

          | Ccall ("print_char", [c], succ) -> 
              eval_cfgexpr c st cp oc sp >>= fun (c, st) ->
              do_builtin oc st.mem "print_char" [c]  >>= fun ans -> 
                eval_cfginstr oc st ht succ cp sp
          
                
          | Ccall (fname, fargs, succ) -> 
              let f_fold argums expri = (
                argums >>= fun (argums, sti) ->
                eval_cfgexpr expri sti cp oc sp >>= fun (ans_i, sti) -> 
                  OK ((argums@[ans_i]), sti)
              ) in
              (List.fold_left f_fold (OK ([],st) ) fargs) >>= fun (arguments, st) ->
                  find_function cp fname >>= fun func_def ->
                  eval_cfgfun oc st fname func_def arguments cp sp >>= fun _ -> 
                    eval_cfginstr oc st ht succ cp sp

          | Cstore (addr_expr, val_expr, sz, succ) -> 
              eval_cfgexpr addr_expr st cp oc sp >>= fun (addr, st) -> 
              eval_cfgexpr val_expr st cp oc sp >>= fun (v, st) -> 
                  let byte_list = split_bytes sz v in 
                  Mem.write_bytes st.mem addr byte_list >>= fun _ -> 
                      eval_cfginstr oc st ht succ cp sp

          | _ -> Error "Unrecognized Instruction"
          

and eval_cfgfun oc st cfgfunname { cfgfunargs;
                                      cfgfunbody;
                                      cfgentry; cfgfunstksz} vargs cp (sp: int) =
  let sp = sp - cfgfunstksz in
  let st' = { st with env = Hashtbl.create 17 } in
  match List.iter2 (fun a v -> Hashtbl.replace st'.env a v) cfgfunargs vargs with
  | () -> eval_cfginstr oc st' cfgfunbody cfgentry cp sp >>= fun (v, st') ->
    OK (Some v, {st' with env = st.env})
  | exception Invalid_argument _ ->
    Error (Format.sprintf "CFG: Called function %s with %d arguments, expected %d.\n"
             cfgfunname (List.length vargs) (List.length cfgfunargs)
          )

let eval_cfgprog oc cp memsize params =
  let st = init_state memsize in
  find_function cp "main" >>= fun f ->
  let n = List.length f.cfgfunargs in
  let params = take n params in
  eval_cfgfun oc st "main" f params cp memsize >>= fun (v, st) ->
  OK v



  
  
