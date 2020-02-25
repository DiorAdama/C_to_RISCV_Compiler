open Elang
open Batteries
open BatList
open Prog
open Utils
open Builtins
open Utils

let binop_bool_to_int f x y = if f x y then 1 else 0

(* [eval_binop b x y] évalue l'opération binaire [b] sur les arguments [x]
   et [y]. *)
let eval_binop (b: binop) : int -> int -> int =
  match b with
   | _ -> fun x y -> 0

(* [eval_unop u x] évalue l'opération unaire [u] sur l'argument [x]. *)
let eval_unop (u: unop) : int -> int =
  match u with
   | _ -> fun x -> 0

(* [eval_eexpr st e] évalue l'expression [e] dans l'état [st]. Renvoie une
   erreur si besoin. *)
let rec eval_eexpr st (e : expr) : int res =
   Error "eval_eexpr not implemented yet."

(* [eval_einstr oc st ins] evaluates the instruction [ins] in starting state
   [st].

   The parameter [oc], unused for now, is an output channel in which functions
   like "print" will write their output, when we add them.

   This function returns [(ret, st')] :

   - [ret] is an [int option]. [Some v] should be returned when a return
   instruction is met. [None] means that execution should continue.

   - [st'] is the updated state.
*)
let rec eval_einstr oc (st: int state) (ins: instr) :
  (int option * int state) res =
   Error "eval_einstr not implemented yet."

(* [eval_efun oc st f fname vargs] evaluates function [f] (whose name is
   [fname]) starting in state [st], with arguments given in [vargs].

   This returns a pair (ret, st') with the same meaning as for [eval_einstr].
*)
let eval_efun oc (st: int state) ({ funargs; funbody}: efun)
    (fname: string) (vargs: int list)
  : (int option * int state) res =
  (* A function's environment (mapping from local variables to values) is local
     and a function call should not modify the caller's variables. Hence, we
     save the caller's environment in [env_save], call the function in a clean
     environment with only its arguments set, and restore the caller's
     environment. *)
  let env_save = Hashtbl.copy st.env in
  let env = Hashtbl.create 17 in
  match List.iter2 (fun a v -> Hashtbl.replace env a v) funargs vargs with
  | () ->
    eval_einstr oc { st with env } funbody >>= fun (v, st') ->
    OK (v, { st' with env = env_save })
  | exception Invalid_argument _ ->
    Error (Format.sprintf
             "E: Called function %s with %d arguments, expected %d.\n"
             fname (List.length vargs) (List.length funargs)
          )

(* [eval_eprog oc ep memsize params] evaluates a complete program [ep], with
   arguments [params].

   The [memsize] parameter gives the size of the memory this program will be run
   with. This is not useful for now (our programs do not use memory), but it
   will when we add memory allocation to our programs.

   Returns:
   - [OK (Some v)] when the function evaluation went without problems and
   resulted in integer value [v].
   - [OK None] when the function evaluation finished without returning a value.
   - [Error msg] when an error has occured.
*)
let eval_eprog oc (ep: eprog) (memsize: int) (params: int list)
  : int option res =
  let st = init_state memsize in
  find_function ep "main" >>= fun f ->
  (* trim the parameter list to only take as many as required by the function.
     *)
  let n = List.length f.funargs in
  let params = take n params in
  eval_efun oc st f "main" params >>= fun (v, st) ->
  OK v
