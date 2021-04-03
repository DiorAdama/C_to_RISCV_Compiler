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

    | Eunop (_, child_expr) -> type_expr child_expr var_typ fun_typ

    | Ebinop (_, child_expr1, child_expr2) -> (
        type_expr child_expr1 var_typ fun_typ >>= fun typ1 -> 
          type_expr child_expr2 var_typ fun_typ >>= fun typ2 -> 
            match typ1, typ2 with 
              | Tptr _ , _ -> OK typ1 
              | _ , Tptr _ -> OK typ2 
              | _ -> OK typ1 
    )
    | Eaddrof ex -> type_expr ex var_typ fun_typ >>= fun ex_t -> OK (Tptr ex_t)

    | Eload ex -> type_expr ex var_typ fun_typ >>= fun ex_t -> 
        match ex_t with 
          | Tptr t -> OK t 
          | _ -> Error "elang_gen.type_expr: Can not load value from non pointer variable"



let comp_typ e1 e2 var_typ fun_typ = 
  let int_comp = [Tint; Tchar] in
  type_expr e1 var_typ fun_typ >>= fun t1 -> 
    type_expr e2 var_typ fun_typ >>= fun t2 ->(
      match t1, t2 with 
        | t1, t2 when t1 = t2 -> OK true
        | t1, t2 when (List.mem t1 int_comp && List.mem t2 int_comp) -> OK true 
        | Tptr _, t when List.mem t int_comp  -> OK true
        | t, Tptr _ when List.mem t int_comp -> OK true
        | _ -> Error ("Type "^( string_of_typ t1) ^ " and type " ^ ( string_of_typ t2) ^ " are incompatible")
    )
      
        


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
            comp_typ ex1 ex2 var_typ fun_typ >>= fun b -> 
              OK (Ebinop (binop_of_tag t, ex1, ex2))) 
              
      | Node (Tneg, [e]) ->( 
          make_eexpr_of_ast e var_typ fun_typ >>= fun ex -> 
            OK (Eunop (Eneg, ex)) 
      )

      | Node(Taddrof, [StringLeaf s]) -> (
          make_eexpr_of_ast (StringLeaf s) var_typ fun_typ >>= fun ex -> 
            OK (Eaddrof ex)
      )

      | Node(Tvalueat, [e]) -> (
          make_eexpr_of_ast e var_typ fun_typ >>= fun ex ->( 
            type_expr ex var_typ fun_typ >>= fun ex_t -> 
              match ex_t with  
                | Tptr _ -> OK (Eload ex)
                | _ -> Error "elang_gen.make_eexpr_of_ast: Can not load value from non pointer variable"
            )
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


let rec typ_of_tag = function 
  | Ast.Tint -> Prog.Tint
  | Ast.Tchar -> Prog.Tchar 
  | Ast.Tvoid -> Prog.Tvoid
  | _ -> assert false

let tag_is_typ = function 
  | Ast.Tint -> true
  | Ast.Tchar -> true
  | Ast.Tvoid -> true
  | Ast.Tptr -> true
  | _ -> false

let rec make_typ_of_ast (a : Ast.tree) = 
  match a with 
    | Node(ttag, [StringLeaf s]) when (tag_is_typ ttag && ttag <> Ast.Tptr) -> OK (s, typ_of_tag ttag)
    | Node(Tptr, [tl]) -> 
        make_typ_of_ast tl >>= fun (s, t) ->
          OK (s, Tptr t)
    | _ -> Error " elang_gen.make_typ_of_ast: Could not recognize variable type from ast"


let init_var (var: string) (t : typ) var_typ = 
  if Hashtbl.find_option var_typ var = Some Tvoid 
    then Error "Variable of type void can not be initialized" 
  else if Hashtbl.mem var_typ var 
    then Error ("variable " ^ var ^ " already defined") 
  else(
    Hashtbl.add var_typ var t;
    OK (Evar var)
  ) 



let init_expr_of_tag = function 
  | Ast.Tint -> Eint 0
  | Ast.Tchar -> Echar '0'
  | Ast.Tvoid -> Eint 0
  | Ast.Tptr -> Eint 0
  | _ -> assert false

let rec make_einstr_of_ast (a: tree) (var_typ : (string, typ) Hashtbl.t) (fun_typ : (string, typ list * typ) Hashtbl.t) 
: instr res =

  let res = (
    match a with

      | Node (ttag, [_]) when (tag_is_typ ttag) -> (
          make_typ_of_ast a >>= fun (s, typ_s) -> 
          init_var s typ_s var_typ >>= fun _ ->
          match ttag with 
            | Tvoid -> OK (Iblock [])
            | _ ->  OK (Iassign (s, init_expr_of_tag ttag)) 
      )

      | Node (Tassign, [Node (Tassignvar, [e1; e2] )]) ->( 
        make_eexpr_of_ast e2 var_typ fun_typ >>= fun ex2 -> 
          match e1 with 
            | Node(ttag, [_]) when (tag_is_typ ttag) -> 
                make_typ_of_ast e1 >>= fun (s1, typ_s1) -> 
                init_var s1 typ_s1 var_typ >>= fun var1 -> 
                  comp_typ var1 ex2 var_typ fun_typ >>= fun b -> 
                    OK (Iassign (s1, ex2))
            
            | _ ->  
              make_eexpr_of_ast e1 var_typ fun_typ >>= fun ex1 ->
              make_eexpr_of_ast e2 var_typ fun_typ >>= fun ex2 -> 
                comp_typ ex1 ex2 var_typ fun_typ >>= fun b ->
                  match ex1 with 
                    | Eload _ -> OK (Istore (ex1, ex2))
                    | _ -> string_of_varexpr ex1 >>= fun s1 -> OK (Iassign (s1, ex2))
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
  | Node (Targ, [ast_decl] )->
      make_typ_of_ast ast_decl >>= fun (s, typ_s) ->
      OK ( s, typ_s)
  | a -> Error (Printf.sprintf "make_ident: unexpected AST: %s"
                  (string_of_ast a))

let make_fundef_of_ast (a: tree) (fun_typ : (string, typ list * typ) Hashtbl.t ): (string * efun) res =
  match a with
  | Node (Tfundef, [Node (f_ret_typ, [StringLeaf fname]); Node (Tfunargs, fargs); Node (Tfunbody, [fblock])]) ->
      let var_typ = Hashtbl.create 21 in
      let funvarmem = Hashtbl.create 5 in
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
            funvarinmem = funvarmem;
            funstksz = 0;
          })

  | Node (Tfundef, [Node (f_ret_typ, [StringLeaf fname]); Node (Tfunargs, fargs); Node(Tfunbody, [])]) ->
      let var_typ = Hashtbl.create 21 in
      let funvarmem = Hashtbl.create 5 in
      list_map_res make_ident fargs >>= fun fargs ->

      (*adding the inputs of the function to var_typ*)
      List.iter (fun (k, v) -> Hashtbl.replace var_typ k v) fargs;

        (*adding the current function to fun_typ*)
        let arg_types = List.map (fun (key, v) -> v) fargs in
        Hashtbl.replace fun_typ fname (arg_types, (typ_of_tag f_ret_typ));
          OK (fname, {
            funargs= fargs;
            funbody= (Iblock []);
            funvartyp = var_typ; 
            funrettyp = (typ_of_tag f_ret_typ);
            funvarinmem = funvarmem;
            funstksz = 0;
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



