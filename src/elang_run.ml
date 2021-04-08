open Elang
open Batteries
open BatList
open Prog
open Utils
open Builtins
open Utils
open Elang_gen
open Elang_print

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
let rec eval_eexpr (e : expr) st (ep) oc (sp: int) 
      ( cur_fun : efun) (fun_typ : (string, typ list * typ) Hashtbl.t )
      (struct_typ: (string, (string * typ) list) Hashtbl.t)
      : (int * int state) res =

   match e with 
      | Eint i -> OK (i, st)
      | Echar c -> OK (Char.code c, st)
      | Evar name -> (
         match Hashtbl.find_option st.env name with 
            | Some i -> OK (i, st)
            | None -> (
               match Hashtbl.find_option cur_fun.funvarinmem name with 
                  | Some offs -> (
                        type_expr e cur_fun.funvartyp fun_typ struct_typ >>= fun t ->
                        match t with 
                           | Tstruct _ -> OK (sp + offs, st)
                           | _ ->   
                              size_of_type struct_typ t >>= fun sz_t ->
                              Mem.read_bytes_as_int st.mem (sp+offs) sz_t >>= fun ans -> OK (ans, st)
                  )
                  | None -> Error ("Unknown variable " ^ name) 
            )
      )
      | Eunop (unary, x) ->(
         eval_eexpr x st ep oc sp cur_fun fun_typ struct_typ >>= fun (x, st) ->
         OK (eval_unop unary x , st)
      )  
      | Ebinop (binary, x, y) ->(
         type_expr x cur_fun.funvartyp fun_typ struct_typ >>= fun x_t -> 
         type_expr y cur_fun.funvartyp fun_typ struct_typ >>= fun y_t ->
            eval_eexpr x st ep oc sp cur_fun fun_typ struct_typ >>= fun (x, st) ->
            eval_eexpr y st ep oc sp cur_fun fun_typ struct_typ >>= fun (y, st) ->
               match x_t, y_t with 
                  | Tptr ty, int_t when List.mem int_t [Tint; Tchar] -> 
                        size_of_type struct_typ ty >>= fun sz_ty ->
                        OK (eval_binop binary x (y*sz_ty), st)
                  | int_t, Tptr ty when List.mem int_t [Tint; Tchar] -> 
                        size_of_type struct_typ ty >>= fun sz_ty ->
                        OK (eval_binop binary (x*sz_ty) y, st)
                           
                  | _, _ ->  OK (eval_binop binary x y, st)
      )
      | Ecall (fname, argms) -> 
         let f_fold argums expri = (
          argums >>= fun (argums, sti) ->
          eval_eexpr expri sti ep oc sp cur_fun fun_typ struct_typ >>= fun (ans_i, sti) -> 
            OK ((argums@[ans_i]), sti)
        ) in
        (List.fold_left f_fold (OK ([],st) ) argms) >>= fun (arguments, st) ->
            find_function ep fname >>= fun func_def ->
            eval_efun st func_def fname arguments ep oc sp fun_typ struct_typ >>= fun (ans, st) -> 
               option_to_res_bind ans  ("Error in elang_run.eval_eexpr Ecall " ^ fname) (fun ans -> OK (ans, st))

      | Eaddrof eexpr -> (
            match eexpr with 
               | Evar var_name -> (
                     match Hashtbl.find_option cur_fun.funvarinmem var_name with 
                        | Some offs -> OK (sp + offs, st)
                        | None -> 
                           match Hashtbl.find_option st.env var_name with
                              | Some i -> OK (i, st)
                              | _ -> Error "@elang_run.eval_eexpr: Variable not found"
               )
               | Eload ptr_expr -> 
                     eval_eexpr ptr_expr st ep oc sp cur_fun fun_typ struct_typ

               | _ -> Error "@elang_run.eval_eexpr : can not get address of Eexpr "
      )

      | Eload eexpr ->(
            type_expr eexpr cur_fun.funvartyp fun_typ struct_typ >>= fun t -> 
            match t with 
               | Tptr ptr_t -> 
                     eval_eexpr eexpr st ep oc sp cur_fun fun_typ struct_typ >>= fun (addr, st) ->
                     size_of_type struct_typ ptr_t >>= fun sz_ptr_t ->
                     Mem.read_bytes_as_int st.mem addr sz_ptr_t >>= fun ans -> OK (ans, st)

               | _ -> Error "@elang_run.eval_eexpr : can not load data from non-pointer variable"
      ) 

      | Egetfield (eexpr, field) -> (
            type_expr eexpr cur_fun.funvartyp fun_typ struct_typ >>= fun t -> 
               match t with 
                  | Tptr (Tstruct str_name) -> 
                     eval_eexpr eexpr st ep oc sp cur_fun fun_typ struct_typ >>= fun (str_pos, st) -> 
                     (*eval_einstr (Icall ("print", [Eint str_pos])) st ep oc sp cur_fun fun_typ struct_typ >>= fun _ ->*)
                     field_offset struct_typ str_name field >>= fun field_offs -> 
                     field_type struct_typ str_name field >>= fun f_typ ->(
                        match f_typ with 
                           | Tstruct _ -> OK ( str_pos + field_offs, st)
                           | _ -> 
                                 size_of_type struct_typ f_typ >>= fun sz_ptr_t ->
                                 Mem.read_bytes_as_int st.mem ( str_pos + field_offs) sz_ptr_t >>= fun ans -> 
                                 OK (ans, st)
                     )
                  | _ -> Error (Format.sprintf "elang_gen.type_expr: Type of expr[%s] is not struct pointer" (dump_eexpr eexpr))
      )

         

(* [eval_einstr oc st ins] évalue l'instrution [ins] en partant de l'état [st].

   Le paramètre [oc] est un "output channel", dans lequel la fonction "print"
   écrit sa sortie, au moyen de l'instruction [Format.fprintf].

   Cette fonction renvoie [(ret, st')] :

   - [ret] est de type [int option]. [Some v] doit être renvoyé lorsqu'une
   instruction [return] est évaluée. [None] signifie qu'aucun [return] n'a eu
   lieu et que l'exécution doit continuer.

   - [st'] est l'état mis à jour. *)
and eval_einstr (ins: instr) (st: int state) (ep) oc (sp: int) 
   ( cur_fun: efun) (fun_typ : (string, typ list * typ) Hashtbl.t ) 
   (struct_typ: (string, (string * typ) list) Hashtbl.t) :
  (int option * int state) res =

   match ins with
      | Iassign (var_name, eexpr) ->(
            eval_eexpr eexpr st ep oc sp cur_fun fun_typ struct_typ >>= fun (expr_val, st) ->(
            match Hashtbl.find_option cur_fun.funvarinmem var_name with 
               | None -> Hashtbl.replace st.env var_name expr_val; OK (None, st)
               | Some offs ->
                     type_expr (Evar var_name) cur_fun.funvartyp fun_typ struct_typ >>= fun t ->
                     size_of_type struct_typ t >>= fun sz_t ->
                     let byte_list = split_bytes sz_t expr_val in 
                     Mem.write_bytes st.mem (sp+offs) byte_list >>= fun _ -> OK (None, st) 
            )
      )
      | Iif (ex, i1, i2) ->(
            match eval_eexpr ex st ep oc sp cur_fun fun_typ struct_typ with 
               | OK (1, new_st) -> eval_einstr i1 new_st ep oc sp cur_fun fun_typ struct_typ
               | OK (0, new_st) -> eval_einstr i2 new_st ep oc sp cur_fun fun_typ struct_typ
               | _ -> Error "Failed to Evaluate if instruction"
      ) 

      | Iwhile (ex, i) ->(
         let rec f_while ret_while state_while = 
            match eval_eexpr ex state_while ep oc sp cur_fun fun_typ struct_typ with 
            | OK (1, new_st) -> 
               eval_einstr i new_st ep oc sp cur_fun fun_typ struct_typ >>= fun (next_ret, next_st) -> 
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
               | _ -> eval_einstr ii new_st ep oc sp cur_fun fun_typ struct_typ
         in
         List.fold_left f_fold (OK (None, st)) instrs
      )
      | Ireturn ex ->(
         eval_eexpr ex st ep oc sp cur_fun fun_typ struct_typ >>= fun (ex, st) ->
            OK (Some ex, st)
      )
      | Icall ("print", argms) -> 
         let f_fold argums expri = (
          argums >>= fun (argums, sti) ->
          eval_eexpr expri sti ep oc sp cur_fun fun_typ struct_typ >>= fun (ans_i, sti) -> 
            OK ((argums@[ans_i]), sti)
        ) in
        (List.fold_left f_fold (OK ([],st) ) argms) >>= fun (arguments, st) ->
         do_builtin oc st.mem "print" arguments >>= fun ans -> 
            OK (ans, st)

      | Icall ("print_char", [arg]) -> 
         eval_eexpr arg st ep oc sp cur_fun fun_typ struct_typ >>= fun (ans, st) -> 
         do_builtin oc st.mem "print_char" [ans] >>= fun ans -> 
            OK (ans, st)
      
      | Icall (fname, argms) ->(
            let f_fold argums expri = (
               argums >>= fun (argums, sti) ->
               eval_eexpr expri sti ep oc sp cur_fun fun_typ struct_typ >>= fun (ans_i, sti) -> 
                 OK ((argums@[ans_i]), sti)
             ) in
             (List.fold_left f_fold (OK ([],st) ) argms) >>= fun (arguments, st) ->
                 find_function ep fname >>= fun func_def ->
                 eval_efun st func_def fname arguments ep oc sp fun_typ struct_typ >>= fun (ans, st) -> 
                  OK (None, st)
      )
      | Istore (e1, e2) -> (
         eval_eexpr e2 st ep oc sp cur_fun fun_typ struct_typ >>= fun (val2, st) ->
         type_expr e1 cur_fun.funvartyp fun_typ struct_typ >>= fun t -> 
            match t with 
               | Tptr ptr_t -> 
                     eval_eexpr e1 st ep oc sp cur_fun fun_typ struct_typ >>= fun (addr, st) ->
                     size_of_type struct_typ ptr_t >>= fun sz_ptr_t ->
                     let byte_list = split_bytes sz_ptr_t val2 in
                     Mem.write_bytes st.mem addr byte_list >>= fun ans -> OK (None, st)
               | _ -> Error ("@elang_run.eval_einstr : can not load data from non-pointer variable " ^ (string_of_typ t))
      ) 

      | Isetfield (e1, field, e2) -> (
         eval_eexpr e2 st ep oc sp cur_fun fun_typ struct_typ >>= fun (val2, st) ->
         type_expr e1 cur_fun.funvartyp fun_typ struct_typ >>= fun t -> 

            match t with 
               | Tptr (Tstruct str_name) -> 
                  eval_eexpr e1 st ep oc sp cur_fun fun_typ struct_typ >>= fun (str_pos, st) -> 
                  field_offset struct_typ str_name field >>= fun field_offs -> 
                     size_of_type struct_typ t >>= fun sz_ptr_t ->
                     let byte_list = split_bytes sz_ptr_t val2 in
                     Mem.write_bytes st.mem ( str_pos + field_offs) byte_list >>= fun ans -> OK (None, st)

               | _ -> Error (Format.sprintf "elang_gen.type_expr: Type of expr[%s] is not struct pointer" (dump_eexpr e1))
      )
      
      | _ -> Error "Unrecognized Instruction"


(* [eval_efun oc st f fname vargs] évalue la fonction [f] (dont le nom est
   [fname]) en partant de l'état [st], avec les arguments [vargs].

   Cette fonction renvoie un couple (ret, st') avec la même signification que
   pour [eval_einstr]. *)
and eval_efun (st: int state) ( (*{ funargs; funbody; funvartyp; funrettyp; funvarinmem; funstksz}*) cur_fun: efun)
    (fname: string) (vargs: int list) (ep) oc (sp: int) (fun_typ : (string, typ list * typ) Hashtbl.t )
    (struct_typ: (string, (string * typ) list) Hashtbl.t)
  : (int option * int state) res =
  (* L'environnement d'une fonction (mapping des variables locales vers leurs
     valeurs) est local et un appel de fonction ne devrait pas modifier les
     variables de l'appelant. Donc, on sauvegarde l'environnement de l'appelant
     dans [env_save], on appelle la fonction dans un environnement propre (Avec
     seulement ses arguments), puis on restore l'environnement de l'appelant. *) 
  let sp = sp - cur_fun.funstksz in
  let env_save = Hashtbl.copy st.env in
  let env = Hashtbl.create 17 in
  match List.iter2 (fun a v -> Hashtbl.replace env (fst a) v) cur_fun.funargs vargs with
  | () ->
    eval_einstr cur_fun.funbody { st with env = env } ep oc sp cur_fun fun_typ struct_typ >>= fun (v, st') ->
    OK (v, { st' with env = env_save }) 
  | exception Invalid_argument _ ->
    Error (Format.sprintf
             "E: Called function %s with %d arguments, expected %d.\n"
             fname (List.length vargs) (List.length cur_fun.funargs)
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

  let struct_type = snd ep in 
  let efuns = fst ep in
  let st = init_state memsize in
  let fun_typ = Hashtbl.create (List.length efuns) in 
      Hashtbl.replace fun_typ "print" ([Tint], Tvoid);
      Hashtbl.replace fun_typ "print_int" ([Tint], Tvoid);
      Hashtbl.replace fun_typ "print_char" ([Tchar], Tvoid);
   List.iter (fun (fname, Gfun ef) -> 
      let arg_types = List.map (fun (key, v) -> v) ef.funargs in
      Hashtbl.replace fun_typ fname (arg_types, ef.funrettyp);
      ) efuns ;
  find_function efuns "main" >>= fun f ->
  (* ne garde que le nombre nécessaire de paramètres pour la fonction "main". *)
  let n = List.length f.funargs in
  let params = take n params in
  eval_efun st f "main" params efuns oc memsize fun_typ struct_type >>= fun (v, st) ->
  OK v

