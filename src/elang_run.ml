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
   | Eadd -> fun x y -> x+y
   | Esub -> fun x y -> x-y
   | Emul -> fun x y -> x*y
   | Ediv -> fun x y -> x/y
   | Emod -> fun x y -> x mod y
   | Exor -> fun x y -> x lxor y

   | Eceq -> binop_bool_to_int (fun x y -> x=y)
   | Ecne -> binop_bool_to_int (fun x y -> x<>y)
   | Ecgt -> binop_bool_to_int (fun x y -> x>y)
   | Eclt -> binop_bool_to_int (fun x y -> x<y)
   | Ecge -> binop_bool_to_int (fun x y -> x>=y)
   | Ecle -> binop_bool_to_int (fun x y -> x<=y)

(* [eval_unop u x] évalue l'opération unaire [u] sur l'argument [x]. *)
let eval_unop (u: unop) : int -> int =
  match u with
   | Eneg -> fun x -> -x

(* [eval_eexpr st e] évalue l'expression [e] dans l'état [st]. Renvoie une
   erreur si besoin. *)
let rec eval_eexpr (e : expr) st (ep : eprog) oc: (int * int state) res =
   match e with 
      | Eint i -> OK (i, st)
      | Echar c -> Error "elang run not implemented yet"
      | Evar name -> (
         match Hashtbl.find_option st.env name with 
            | None -> Error ("Unknown variable " ^ name) 
            | Some i -> OK (i, st)
      )
      | Eunop (unary, x) ->(
         eval_eexpr x st ep oc >>= fun (x, st) ->
         OK (eval_unop unary x , st)
      )  
      | Ebinop (binary, x, y) ->(
         eval_eexpr x st ep oc >>= fun (x, st) ->
            eval_eexpr y st ep oc >>= fun (y, st) ->
               OK (eval_binop binary x y, st)
      )
      | Ecall (fname, argms) -> 
         let f_fold argums expri = (
          argums >>= fun (argums, sti) ->
          eval_eexpr expri sti ep oc >>= fun (ans_i, sti) -> 
            OK ((argums@[ans_i]), sti)
        ) in
        (List.fold_left f_fold (OK ([],st) ) argms) >>= fun (arguments, st) ->
            find_function ep fname >>= fun func_def ->
            eval_efun st func_def fname arguments ep oc >>= fun (ans, st) -> 
               option_to_res_bind ans  ("Error in elang_run.eval_eexpr Ecall " ^ fname) (fun ans -> OK (ans, st))

         

(* [eval_einstr oc st ins] évalue l'instrution [ins] en partant de l'état [st].

   Le paramètre [oc] est un "output channel", dans lequel la fonction "print"
   écrit sa sortie, au moyen de l'instruction [Format.fprintf].

   Cette fonction renvoie [(ret, st')] :

   - [ret] est de type [int option]. [Some v] doit être renvoyé lorsqu'une
   instruction [return] est évaluée. [None] signifie qu'aucun [return] n'a eu
   lieu et que l'exécution doit continuer.

   - [st'] est l'état mis à jour. *)
and eval_einstr (ins: instr) (st: int state) (ep : eprog) oc :
  (int option * int state) res =

   match ins with
      | Iassign (var_name, expr) ->(
            eval_eexpr expr st ep oc >>= fun (expr, st) ->
            Hashtbl.replace st.env var_name expr;
            OK (None, st)
      )
      | Iif (ex, i1, i2) ->(
            match eval_eexpr ex st ep oc with 
               | OK (1, new_st) -> eval_einstr i1 new_st ep oc 
               | OK (0, new_st) -> eval_einstr i2 new_st ep oc
               | _ -> Error "Failed to Evaluate if instruction"
      ) 

      | Iwhile (ex, i) ->(
         let rec f_while ret_while state_while = 
            match eval_eexpr ex state_while ep oc with 
            | OK (1, new_st) -> 
               eval_einstr i new_st ep oc >>= fun (next_ret, next_st) -> 
               f_while next_ret next_st
            | OK (0, new_st) -> OK (ret_while, new_st)
            | _ -> Error "Failed to Evaluate while instruction"
         in
         f_while None st
      ) 
      | Iblock instrs -> (
         let f_fold a ii = 
            a >>= fun (ans, new_st) ->
            match ans with 
               | Some first_ret -> OK (ans, new_st)
               | _ -> eval_einstr ii new_st ep oc 
         in
         List.fold_left f_fold (OK (None, st)) instrs
      )
      | Ireturn ex ->(
         eval_eexpr ex st ep oc >>= fun (ex, st) ->
            OK (Some ex, st)
      )
      | Icall ("print", argms) -> 
         let f_fold argums expri = (
          argums >>= fun (argums, sti) ->
          eval_eexpr expri sti ep oc >>= fun (ans_i, sti) -> 
            OK ((argums@[ans_i]), sti)
        ) in
        (List.fold_left f_fold (OK ([],st) ) argms) >>= fun (arguments, st) ->
         do_builtin oc st.mem "print" arguments >>= fun ans -> 
            OK (ans, st)
            
      | Icall (fname, argms) ->
         eval_eexpr (Ecall (fname, argms)) st ep oc >>= fun (ans, new_st) ->
            OK (None, new_st)
      
      | _ -> Error "Unrecognized Instruction"


(* [eval_efun oc st f fname vargs] évalue la fonction [f] (dont le nom est
   [fname]) en partant de l'état [st], avec les arguments [vargs].

   Cette fonction renvoie un couple (ret, st') avec la même signification que
   pour [eval_einstr]. *)
and eval_efun (st: int state) ({ funargs; funbody}: efun)
    (fname: string) (vargs: int list) (ep : eprog) oc
  : (int option * int state) res =
  (* L'environnement d'une fonction (mapping des variables locales vers leurs
     valeurs) est local et un appel de fonction ne devrait pas modifier les
     variables de l'appelant. Donc, on sauvegarde l'environnement de l'appelant
     dans [env_save], on appelle la fonction dans un environnement propre (Avec
     seulement ses arguments), puis on restore l'environnement de l'appelant. *)
  let env_save = Hashtbl.copy st.env in
  let env = Hashtbl.create 17 in
  match List.iter2 (fun a v -> Hashtbl.replace env a v) funargs vargs with
  | () ->
    eval_einstr funbody { st with env = env } ep oc >>= fun (v, st') ->
    OK (v, { st' with env = env_save })
  | exception Invalid_argument _ ->
    Error (Format.sprintf
             "E: Called function %s with %d arguments, expected %d.\n"
             fname (List.length vargs) (List.length funargs)
          )

(* [eval_eprog oc ep memsize params] évalue un programme complet [ep], avec les
   arguments [params].

   Le paramètre [memsize] donne la taille de la mémoire dont ce programme va
   disposer. Ce n'est pas utile tout de suite (nos programmes n'utilisent pas de
   mémoire), mais ça le sera lorsqu'on ajoutera de l'allocation dynamique dans
   nos programmes.

   Renvoie:

   - [OK (Some v)] lorsque l'évaluation de la fonction a lieu sans problèmes et renvoie une valeur [v].

   - [OK None] lorsque l'évaluation de la fonction termine sans renvoyer de valeur.

   - [Error msg] lorsqu'une erreur survient.
   *)


let eval_eprog oc (ep: eprog) (memsize: int) (params: int list)
  : int option res =
  let st = init_state memsize in
  find_function ep "main" >>= fun f ->
  (* ne garde que le nombre nécessaire de paramètres pour la fonction "main". *)
  let n = List.length f.funargs in
  let params = take n params in
  eval_efun st f "main" params ep oc >>= fun (v, st) ->
  OK v

