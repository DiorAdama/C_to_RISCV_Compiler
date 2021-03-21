open Prog
open Elang
open Elang_run
open Batteries
open BatList
open Cfg
open Utils
open Builtins

let rec eval_cfgexpr (e: expr) st cp oc : (int* int Prog.state) res =
  match e with
    | Ebinop(b, e1, e2) ->
        eval_cfgexpr e1 st cp oc >>= fun (v1, st) ->
        eval_cfgexpr e2 st cp oc >>= fun (v2, st) ->
        let v = eval_binop b v1 v2 in
        OK (v, st)
    | Eunop(u, e) ->
        eval_cfgexpr e st cp oc >>= fun (v1, st) ->
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
          eval_cfgexpr expri sti cp oc >>= fun (ans_i, sti) -> 
            OK ((argums@[ans_i]), sti)
        ) in
        (List.fold_left f_fold (OK ([],st) ) fargs) >>= fun (arguments, st) ->
            find_function cp fname >>= fun func_def ->
            eval_cfgfun oc st fname func_def arguments cp >>= fun (ans, st) -> 
              option_to_res_bind ans  ("Error in cfg_run.eval_cfgexpr Ecall " ^ fname) (fun ans -> OK (ans, st))


and eval_cfginstr oc st ht (n: int) cp: (int * int state) res =
  match Hashtbl.find_option ht n with
    | None -> Error (Printf.sprintf "Invalid node identifier\n")
    | Some node ->
        match node with
          | Cnop succ ->
              eval_cfginstr oc st ht succ cp
          | Cassign(v, e, succ) ->
              eval_cfgexpr e st cp oc >>= fun (i, st) ->
              Hashtbl.replace st.env v i;
              eval_cfginstr oc st ht succ cp
          | Ccmp(cond, i1, i2) ->
              eval_cfgexpr cond st cp oc >>= fun (i,st) ->
              if i = 0 
                then eval_cfginstr oc st ht i2 cp
              else 
                eval_cfginstr oc st ht i1 cp
          | Creturn(e) ->
              eval_cfgexpr e st cp oc >>= fun (e, st) ->
              OK (e, st)
          (*
          | Cprint(e, succ) ->
              eval_cfgexpr e st cp oc >>= fun (e, st) ->
              Format.fprintf oc "%d\n" e;
              eval_cfginstr oc st ht succ cp
*)
          | Ccall ("print", pargs, succ) -> 
              let f_fold argums expri = (
                argums >>= fun (argums, sti) ->
                eval_cfgexpr expri sti cp oc >>= fun (ans_i, sti) -> 
                  OK ((argums@[ans_i]), sti)
              ) in
              (List.fold_left f_fold (OK ([],st) ) pargs) >>= fun (arguments, st) ->
              do_builtin oc st.mem "print" arguments >>= fun ans -> 
                option_to_res_bind ans "" (fun x -> OK (x, st))
                
          | Ccall (fname, fargs, succ) -> 
              eval_cfgexpr (Ecall (fname, fargs)) st cp oc >>= fun (ans, st) ->
                OK (ans, st)

          | _ -> Error "Unrecognized Instruction"
          

and eval_cfgfun oc st cfgfunname { cfgfunargs;
                                      cfgfunbody;
                                      cfgentry} vargs cp =
  let st' = { st with env = Hashtbl.create 17 } in
  match List.iter2 (fun a v -> Hashtbl.replace st'.env a v) cfgfunargs vargs with
  | () -> eval_cfginstr oc st' cfgfunbody cfgentry cp >>= fun (v, st') ->
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
  eval_cfgfun oc st "main" f params cp >>= fun (v, st) ->
  OK v



  