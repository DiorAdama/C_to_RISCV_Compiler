open Ast
open Elang
open Prog
open Report
open Options
open Batteries
open Elang_print
open Utils

let tag_is_binop =
  function
  | Tadd -> true
  | Tsub -> true
  | Tmul -> true
  | Tdiv -> true
  | Tmod -> true
  | Txor -> true
  | Tcle -> true
  | Tclt -> true
  | Tcge -> true
  | Tcgt -> true
  | Tceq -> true
  | Tne  -> true
  | _    -> false

let binop_of_tag =
  function
  | Tadd -> Eadd
  | Tsub -> Esub
  | Tmul -> Emul
  | Tdiv -> Ediv
  | Tmod -> Emod
  | Txor -> Exor
  | Tcle -> Ecle
  | Tclt -> Eclt
  | Tcge -> Ecge
  | Tcgt -> Ecgt
  | Tceq -> Eceq
  | Tne -> Ecne
  | _ -> assert false



let rec type_expr (e : expr) (var_typ : (string, typ) Hashtbl.t) (fun_typ : (string, typ list * typ) Hashtbl.t) 
   : typ res =
  
  match e with 
    | Eint _ -> OK Tint
    | Echar _ -> OK Tchar
    | Evar var -> (
        match Hashtbl.find_option var_typ var with
          | None -> Error "elang_gen.type_expr: Variable type unfound"
          | Some t -> OK t
    )
    | Ecall (fname, _) -> 
        option_to_res_bind (Hashtbl.find_option fun_typ fname) "elang_gen.type_expr: Variable type unfound" (fun t -> OK (snd t))
    | Eunop (_, child_expr) 
    | Ebinop (_, child_expr, _) -> type_expr child_expr var_typ fun_typ 



(* [make_eexpr_of_ast a] builds an expression corresponding to a tree [a]. If
   the tree is not well-formed, fails with an [Error] message. *)
let rec make_eexpr_of_ast (a: tree) (var_typ : (string, typ) Hashtbl.t) (fun_typ : (string, typ list * typ) Hashtbl.t) 
: expr res =

  let res =
    match a with
      | IntLeaf x -> OK (Eint x)

      | CharLeaf c -> OK (Echar c)

      | StringLeaf s -> 
          type_expr (Evar s) var_typ fun_typ  >>= fun t -> OK (Evar s) 

      | Node(t, [e1; e2]) when tag_is_binop t ->(
          make_eexpr_of_ast e1 var_typ fun_typ >>= fun ex1 -> 
          make_eexpr_of_ast e2 var_typ fun_typ >>= fun ex2 ->
            type_expr ex1 var_typ fun_typ >>= fun t1 -> 
            type_expr ex2 var_typ fun_typ >>= fun t2 ->
              if t1 = t2 then 
                OK (Ebinop (binop_of_tag t, ex1, ex2))
              else
                Error "Operands in binary operation don't have the same data type"    
)
      | Node (Tneg, [e]) ->(
          make_eexpr_of_ast e var_typ fun_typ>>= fun ex -> 
            OK (Eunop (Eneg, ex))
      )

      | Node (Tcall, [(StringLeaf fname); Node(Targs, argmts)]) ->(
        let f_fold argms ast_node = (
          argms >>= fun argums ->
          make_eexpr_of_ast ast_node var_typ fun_typ>>= fun ex -> 
            OK (argums@[ex])
        ) in
        (List.fold_left f_fold (OK []) argmts) >>= fun arguments -> 

          (*This part checks if the expressions given to the function have the right data type*)

          (option_to_res_bind (Hashtbl.find_option fun_typ fname) "Variable type unfound" (fun t -> OK (fst t)))
            >>= fun arg_types -> 
              (List.fold_left (fun rest_arg_types arg_expr -> 
                rest_arg_types >>= fun rest_arg_types ->
                type_expr arg_expr var_typ fun_typ >>= fun t -> 
                  if t = (List.hd rest_arg_types) then 
                    OK (List.tl rest_arg_types)
                  else
                    Error ("Wrong input datatype for function " ^ fname)
              ) (OK arg_types) arguments)
          >>= fun valid_input ->
          OK (Ecall (fname, arguments))
      )

      | _ -> Error (Printf.sprintf "Unacceptable ast in make_eexpr_of_ast %s"
                      (string_of_ast a))
  in
  match res with
  | OK o -> res
  | Error msg -> Error (Format.sprintf "In make_eexpr_of_ast %s:\n%s"
                          (string_of_ast a) msg)
                        

let string_of_varexpr = function 
  | Evar s -> OK s
  | _ -> Error "The given expression is not a variable"

let same_typ e1 e2 var_typ fun_typ = 
  type_expr e1 var_typ fun_typ >>= fun t1 -> 
    type_expr e2 var_typ fun_typ >>= fun t2 ->
  if (t1 = t2) || (t1=Tint && t2=Tchar) || (t1=Tchar && t2=Tint) 
    then OK true
  else
    Error ("Type "^( string_of_typ t1) ^ " and type " ^ ( string_of_typ t2) ^ " are incompatible")

let init_var (var: string) (t : typ) var_typ = 
  if Hashtbl.find_option var_typ var = Some Tvoid 
    then Error "Variable of type void can not be initialized" 
  else if Hashtbl.mem var_typ var 
    then Error ("variable " ^ var ^ " already defined") 
  else(
    Hashtbl.add var_typ var t;
    OK (Evar var)
  ) 

let tag_is_typ = function 
  | Ast.Tint -> true
  | Ast.Tchar -> true
  | Ast.Tvoid -> true
  | _ -> false

let typ_of_tag = function 
  | Ast.Tint -> Prog.Tint
  | Ast.Tchar -> Prog.Tchar 
  | Ast.Tvoid -> Prog.Tvoid
  | _ -> assert false

let rec make_einstr_of_ast (a: tree) (var_typ : (string, typ) Hashtbl.t) (fun_typ : (string, typ list * typ) Hashtbl.t) 
: instr res =

  let res = (
    match a with
      | Node (Tassign, [Node (Tassignvar, [e1; e2] )]) ->( 
        make_eexpr_of_ast e2 var_typ fun_typ >>= fun ex2 -> 
          match e1 with 
            | Node(tag, [StringLeaf s1]) when (tag_is_typ tag) -> 
                init_var s1 (typ_of_tag tag) var_typ >>= fun var1 -> 
                  same_typ var1 ex2 var_typ fun_typ >>= fun b -> 
                    OK (Iassign (s1, ex2))
            
            | _ ->  
              make_eexpr_of_ast e1 var_typ fun_typ >>= fun ex1 ->
              make_eexpr_of_ast e2 var_typ fun_typ >>= fun ex2 -> 
                same_typ ex1 ex2 var_typ fun_typ >>= fun b -> 
                  string_of_varexpr ex1 >>= fun s1 -> 
                    OK (Iassign (s1, ex2))
      )

      | Node (Tif, [expr; instr1; instr2]) ->(
          make_eexpr_of_ast expr var_typ fun_typ >>= fun ex ->
            make_einstr_of_ast instr1 var_typ fun_typ >>= fun i1 ->
              make_einstr_of_ast instr2 var_typ fun_typ >>= fun i2 ->
                OK (Iif (ex, i1, i2))
      )
      | Node (Tif, [expr; instr1]) ->( 
        make_eexpr_of_ast expr var_typ fun_typ >>= fun ex ->
          make_einstr_of_ast instr1 var_typ fun_typ >>= fun i1 -> 
            OK (Iif (ex, i1, Iblock []))
      )
    
      | Node (Twhile, [expr; instr]) ->(
          make_eexpr_of_ast expr var_typ fun_typ>>= fun ex ->
            make_einstr_of_ast instr var_typ fun_typ >>= fun i ->
              OK (Iwhile (ex, i))
      )
      
      | Node (Tblock, instrs) ->( 
          let f_fold a instri = 
            make_einstr_of_ast instri var_typ fun_typ >>= fun i ->
              a >>= fun l ->
                OK (l @ [i])
          in
          List.fold_left f_fold (OK []) instrs >>= fun instr_list ->
          OK (Iblock instr_list)
      )
      | Node (Treturn, [expr]) -> (
        make_eexpr_of_ast expr var_typ fun_typ>>= fun ex ->
          OK (Ireturn ex)
      )
(*
      | Node (Tprint, [expr]) ->(
        make_eexpr_of_ast expr var_typ fun_typ >>= fun ex ->
          OK (Iprint ex)
      ) 
*)      
      | Node (Tcall, [(StringLeaf fname); Node(Targs, argmts)]) ->(
          make_eexpr_of_ast a var_typ fun_typ >>= fun exp ->(
            match exp with 
              | Ecall (fn, argms) -> OK (Icall (fn, argms)) 
              | _ -> Error (Printf.sprintf "Unacceptable ast in make_eexpr_of_ast %s"
                              (string_of_ast a))
          ) 
      )

      | _ -> Error (Printf.sprintf "Unacceptable ast in make_einstr_of_ast %s"
                      (string_of_ast a))
  )
  in
  match res with
  | OK o -> res
  | Error msg -> Error (Format.sprintf "In make_einstr_of_ast %s:\n%s"
                          (string_of_ast a) msg)

                          
let make_ident (a: tree) : (string* Prog.typ) res =
  match a with
  | Node (Targ, [Node (t, [s])]) ->
      OK ( string_of_stringleaf s, typ_of_tag t)
  | a -> Error (Printf.sprintf "make_ident: unexpected AST: %s"
                  (string_of_ast a))

let make_fundef_of_ast (a: tree) (fun_typ : (string, typ list * typ) Hashtbl.t ): (string * efun) res =
  match a with
  | Node (Tfundef, [Node (f_ret_typ, [StringLeaf fname]); Node (Tfunargs, fargs); Node (Tfunbody, [fblock])]) ->
      let var_typ = Hashtbl.create 21 in
      list_map_res make_ident fargs >>= fun fargs ->

        (*adding the inputs of the function to var_typ*)
        List.iter (fun (k, v) -> Hashtbl.replace var_typ k v) fargs;

        (*adding the current function to fun_typ*)
        let arg_types = List.map (fun (key, v) -> v) fargs in
        Hashtbl.replace fun_typ fname (arg_types, (typ_of_tag f_ret_typ));

        make_einstr_of_ast fblock var_typ fun_typ >>= fun fblock ->
          OK (fname, {
            funargs= fargs;
            funbody= fblock;
            funvartyp = var_typ; 
            funrettyp = (typ_of_tag f_ret_typ);
          })
  | _ ->
    Error (Printf.sprintf "make_fundef_of_ast: Expected a Tfundef, got %s."
             (string_of_ast a))

let make_eprog_of_ast (a: tree) : eprog res =
  match a with
  | Node (Tlistglobdef, l) ->
      let fun_typ = Hashtbl.create (List.length l) in 
      Hashtbl.replace fun_typ "print" ([Tint], Tvoid);
      Hashtbl.replace fun_typ "print_int" ([Tint], Tvoid);
      Hashtbl.replace fun_typ "print_char" ([Tchar], Tvoid);
      list_map_res (fun a -> make_fundef_of_ast a fun_typ >>= fun (fname, efun) -> OK (fname, Gfun efun)) l
  | _ ->
    Error (Printf.sprintf "make_fundef_of_ast: Expected a Tlistglobdef, got %s."
             (string_of_ast a))

let pass_elang ast =
  match make_eprog_of_ast ast with
  | Error msg ->
    record_compile_result ~error:(Some msg) "Elang";
    Error msg
  | OK  ep ->
    dump !e_dump dump_e ep (fun file () ->
        add_to_report "e" "E" (Code (file_contents file))); OK ep



