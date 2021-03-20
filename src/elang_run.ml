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
let rec eval_eexpr st (e : expr) : int res =
   match e with 
      | Eint i -> OK i
      | Evar name -> OK (Hashtbl.find st.env name)
      | Eunop (unary, x) ->(
         eval_eexpr st x >>= fun x ->
         OK (eval_unop unary x)
      )  
      | Ebinop (binary, x, y) ->(
         eval_eexpr st x >>= fun x ->
            eval_eexpr st y >>= fun y ->
               OK (eval_binop binary x y)
      )
      | Ecall (fname, argms) -> Error "Error in Ecall"
      

(* [eval_einstr oc st ins] évalue l'instrution [ins] en partant de l'état [st].

   Le paramètre [oc] est un "output channel", dans lequel la fonction "print"
   écrit sa sortie, au moyen de l'instruction [Format.fprintf].

   Cette fonction renvoie [(ret, st')] :

   - [ret] est de type [int option]. [Some v] doit être renvoyé lorsqu'une
   instruction [return] est évaluée. [None] signifie qu'aucun [return] n'a eu
   lieu et que l'exécution doit continuer.

   - [st'] est l'état mis à jour. *)
let rec eval_einstr oc (st: int state) (ins: instr) :
  (int option * int state) res =

   match ins with
      | Iassign (var_name, expr) ->(
            eval_eexpr st expr >>= fun expr ->
            Hashtbl.replace st.env var_name expr;
            OK (None, st)
      )
      | Iif (ex, i1, i2) ->(
            if eval_eexpr st ex = (OK 1) then
               eval_einstr oc st i1
            else
               eval_einstr oc st i2
      ) 
      | Iwhile (ex, i) ->(
         let rec f_while ret_while state_while = 
            if eval_eexpr st ex = (OK 1) then
               eval_einstr oc state_while i >>= fun (next_ret, next_st) ->
                  f_while next_ret next_st
            else
               OK (ret_while, state_while)
         in
         f_while None st
      ) 
      | Iblock instrs -> (
         let f_fold a ii = 
            a >>= fun a ->
            eval_einstr oc (snd a) ii 
         in
         List.fold_left f_fold (OK (None, st)) instrs
      )
      | Ireturn ex ->(
         eval_eexpr st ex >>= fun ex ->
            OK (Some ex, st)
      )
      | Iprint ex ->(
         eval_eexpr st ex >>= fun ex ->
            Format.fprintf oc "%d\n" ex;
            OK (None, st)
      )
      | Icall (fname, argms) -> Error "Error In Icall"

(* [eval_efun oc st f fname vargs] évalue la fonction [f] (dont le nom est
   [fname]) en partant de l'état [st], avec les arguments [vargs].

   Cette fonction renvoie un couple (ret, st') avec la même signification que
   pour [eval_einstr]. *)
let eval_efun oc (st: int state) ({ funargs; funbody}: efun)
    (fname: string) (vargs: int list)
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
    eval_einstr oc { st with env } funbody >>= fun (v, st') ->
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
  List.iter2 (Hashtbl.replace st.env ) f.funargs params;
  eval_efun oc st f "main" params >>= fun (v, st) ->
  OK v

