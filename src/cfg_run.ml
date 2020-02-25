open Prog
open Elang
open Elang_run
open Batteries
open BatList
open Cfg
open Utils
open Builtins

let rec eval_cfgexpr st : expr -> int res = function
  | Ebinop(b, e1, e2) ->
    eval_cfgexpr st e1 >>= fun v1 ->
    eval_cfgexpr st e2 >>= fun v2 ->
    let v = eval_binop b v1 v2 in
    OK v
  | Eunop(u, e) ->
    eval_cfgexpr st e >>= fun v1 ->
    let v = (eval_unop u v1) in
    OK v
  | Eint i -> OK i
  | Evar s ->
    begin match Hashtbl.find_option st.env s with
      | Some v -> OK v
      | None -> Error (Printf.sprintf "Unknown variable %s\n" s)
    end

let rec eval_cfginstr oc st ht (n: int): (int * int state) res =
  match Hashtbl.find_option ht n with
  | None -> Error (Printf.sprintf "Invalid node identifier\n")
  | Some node ->
    match node with
    | Cnop succ ->
      eval_cfginstr oc st ht succ
    | Cassign(v, e, succ) ->
      eval_cfgexpr st e >>= fun i ->
      Hashtbl.replace st.env v i;
      eval_cfginstr oc st ht succ
    | Ccmp(cond, i1, i2) ->
      eval_cfgexpr st cond >>= fun i ->
      if i = 0 then eval_cfginstr oc st ht i2 else eval_cfginstr oc st ht i1
    | Creturn(e) ->
      eval_cfgexpr st e >>= fun e ->
      OK (e, st)
    | Cprint(e, succ) ->
      eval_cfgexpr st e >>= fun e ->
      Format.fprintf oc "%d\n" e;
      eval_cfginstr oc st ht succ

let eval_cfgfun oc st cfgfunname { cfgfunargs;
                                      cfgfunbody;
                                      cfgentry} vargs =
  let st' = { st with env = Hashtbl.create 17 } in
  match List.iter2 (fun a v -> Hashtbl.replace st'.env a v) cfgfunargs vargs with
  | () -> eval_cfginstr oc st' cfgfunbody cfgentry >>= fun (v, st') ->
    OK (Some v, st')
  | exception Invalid_argument _ ->
    Error (Format.sprintf "CFG: Called function %s with %d arguments, expected %d.\n"
             cfgfunname (List.length vargs) (List.length cfgfunargs)
          )

let eval_cfgprog oc cp memsize params =
  let st = init_state memsize in
  find_function cp "main" >>= fun f ->
  let n = List.length f.cfgfunargs in
  let params = take n params in
  eval_cfgfun oc st "main" f params >>= fun (v, st) ->
  OK v


