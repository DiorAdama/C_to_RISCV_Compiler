open Batteries
open Elang
open Cfg
open Utils
open Prog
open Report
open Cfg_print
open Options
open Elang_gen

(* [cfg_expr_of_eexpr e] converts an [Elang.expr] into a [expr res]. This should
   always succeed and be straightforward.

   In later versions of this compiler, you will add more things to [Elang.expr]
   but not to [Cfg.expr], hence the distinction.
*)
let rec cfg_expr_of_eexpr (e: Elang.expr) (cur_efun: efun) (fun_typ : (string, typ list * typ) Hashtbl.t)
        (struct_typ: (string, (string * typ) list) Hashtbl.t) : expr res =

  match e with
  | Elang.Ebinop (b, e1, e2) ->(
      type_expr e1 cur_efun.funvartyp fun_typ struct_typ >>= fun t_e1 -> 
      type_expr e2 cur_efun.funvartyp fun_typ struct_typ >>= fun t_e2 ->
        cfg_expr_of_eexpr e1 cur_efun fun_typ struct_typ >>= fun cfg_e1 ->
        cfg_expr_of_eexpr e2 cur_efun fun_typ struct_typ >>= fun cfg_e2 ->
          match t_e1, t_e2 with 
            | Tptr (Tstruct _), _ 
            | _ , Tptr (Tstruct _) -> OK (Ebinop (b, cfg_e1, cfg_e2))
            | Tptr ty, int_t when List.mem int_t [Tint; Tchar] -> 
                  size_of_type struct_typ ty >>= fun sz_ty ->
                  OK ( Ebinop (b, cfg_e1, Ebinop (Emul, cfg_e2, Eint sz_ty)))
                  
            | int_t, Tptr ty when List.mem int_t [Tint; Tchar] -> 
                  size_of_type struct_typ ty >>= fun sz_ty ->
                  OK (Ebinop (b, Ebinop (Emul, cfg_e1, Eint sz_ty), cfg_e2))
            
            | Ttab (ty,_) , int_t when List.mem int_t [Tint; Tchar] -> 
                  size_of_type struct_typ ty >>= fun sz_ty ->
                  OK ( Ebinop (b, cfg_e1, Ebinop (Emul, cfg_e2, Eint sz_ty)))
                    
            | int_t, Ttab(ty,_) when List.mem int_t [Tint; Tchar] -> 
                  size_of_type struct_typ ty >>= fun sz_ty ->
                  OK (Ebinop (b, Ebinop (Emul, cfg_e1, Eint sz_ty), cfg_e2))
                      
            | _, _ ->  OK (Ebinop (b, cfg_e1, cfg_e2))
  )
  | Elang.Eunop (u, e) ->
      cfg_expr_of_eexpr e cur_efun fun_typ struct_typ >>= fun ee ->
      OK (Eunop (u, ee))

  | Elang.Eint i -> OK (Eint i)

  | Elang.Echar c -> OK (Eint (Char.code c))

  | Elang.Evar v -> (
      match Hashtbl.find_option cur_efun.funvarinmem v with 
        | None -> OK (Evar v)
        | Some offs -> 
            type_expr e cur_efun.funvartyp fun_typ struct_typ >>= fun t ->(
              match t with 
                | Tstruct _ 
                | Ttab _ -> OK (Estk offs)
                | _ ->   
                    type_expr e cur_efun.funvartyp fun_typ struct_typ >>= fun t ->
                    size_of_type struct_typ t >>= fun sz_t ->
                    OK (Eload (Estk offs, sz_t))
            )
  )

  | Elang.Ecall (fname, fargs) -> 
      let f_fold a argi = 
        a >>= fun a -> 
        cfg_expr_of_eexpr argi cur_efun fun_typ struct_typ >>= fun cfg_expri ->
          OK (a @ [cfg_expri])
      in
      List.fold_left f_fold (OK []) fargs >>= fun cfg_args -> 
        OK (Ecall (fname, cfg_args))

  | Elang.Eaddrof eexpr -> ( 
      match eexpr with 
        | Evar var_name -> (
              match Hashtbl.find_option cur_efun.funvarinmem var_name with 
                | Some offs -> OK (Estk offs) 
                | None -> Error "@cfg_gen.cfg_expr_of_eexpr: Variable not found"    
        )
        | Eload ptr_expr -> 
              cfg_expr_of_eexpr ptr_expr cur_efun fun_typ struct_typ

        | _ -> Error "@cfg_gen.cfg_expr_of_eexpr : can not get address of Eexpr "
  )

  | Elang.Eload eexpr -> (
      type_expr eexpr cur_efun.funvartyp fun_typ struct_typ >>= fun t -> 
      match t with 
        | Tptr ptr_t -> 
              cfg_expr_of_eexpr eexpr cur_efun fun_typ struct_typ >>= fun cfg_expr ->
              size_of_type struct_typ ptr_t >>= fun sz_ptr_t ->
              OK (Eload (cfg_expr, sz_ptr_t))

        | _ -> Error "@cfg_gen.cfg_expr_of_eexpr : can not load data from non-pointer variable"
  ) 

  | Elang.Egetfield (eexpr, field) -> (
      type_expr eexpr cur_efun.funvartyp fun_typ struct_typ >>= fun t -> 
        match t with 
          | Tptr (Tstruct str_name) -> (
              match eexpr with  
                | Eaddrof (Evar v) -> 
                    field_offset struct_typ str_name field >>= fun field_offs -> 
                    field_type struct_typ str_name field >>= fun f_typ ->
                    size_of_type struct_typ f_typ >>= fun sz_typ ->(
                      match Hashtbl.find_option cur_efun.funvarinmem v with 
                        | Some str_pos -> (
                            match f_typ with 
                              | Tstruct _  
                              | Ttab _ -> OK (Estk (str_pos + field_offs))
                              | _ -> OK (Eload (Estk (str_pos + field_offs), sz_typ)))
                        | None ->  
                            match f_typ with 
                              | Tstruct _ 
                              | Ttab _ -> OK (Ebinop (Eadd, Evar v, Eint field_offs))
                              | _ -> OK (Eload (Ebinop (Eadd, Evar v, Eint field_offs), sz_typ))
                      )  
                        
                | _ -> Error "@cfg_gen.cfg_expr_of_eexpr: Can not get field from a non struct pointer variable"
              )
          | _ -> Error "@cfg_gen.cfg_expr_of_eexpr: Can not get field from a non struct pointer variable"
    )
      (*

              cfg_expr_of_eexpr eexpr cur_efun fun_typ struct_typ >>= fun cfg_expr ->
                match cfg_expr with 
                  | Estk str_pos ->
                      field_offset struct_typ str_name field >>= fun field_offs -> 
                      field_type struct_typ str_name field >>= fun f_typ ->
                      size_of_type struct_typ f_typ >>= fun sz_typ -> (
                          match f_typ with 
                            | Tstruct _ -> OK (Estk (str_pos + field_offs))
                            | _ -> OK (Eload (Estk (str_pos + field_offs), sz_typ)))
                  | _ -> Error "@cfg_gen.cfg_expr_of_eexpr: Can not get field from a non struct pointer variable"
          )
          | _ -> Error "@cfg_gen.cfg_expr_of_eexpr: Can not get field from a non struct pointer variable"
  )
*)

(* [cfg_node_of_einstr next cfg succ i] builds the CFG node(s) that correspond
   to the E instruction [i].

   [cfg] is the current state of the control-flow graph.

   [succ] is the successor of this node in the CFG, i.e. where to go after this
   instruction.

   [next] is the next available CFG node identifier.

   This function returns a pair (n, next) where [n] is the identifer of the
   node generated, and [next] is the new next available CFG node identifier.

   Hint: several nodes may be generated for a single E instruction.*)

let rec cfg_node_of_einstr (next: int) (cfg : (int, cfg_node) Hashtbl.t)
    (succ: int) (i: instr) (cur_efun: efun) (fun_typ : (string, typ list * typ) Hashtbl.t)
    (struct_typ: (string, (string * typ) list) Hashtbl.t) : (int * int) res =
  match i with
  | Elang.Iassign (v, e) ->(
      cfg_expr_of_eexpr e cur_efun fun_typ struct_typ >>= fun val_assign ->
      (match Hashtbl.find_option cur_efun.funvarinmem v with 
        | None -> 
            Hashtbl.replace cfg next (Cassign(v, val_assign, succ));
            OK (next, next + 1)
        | Some offs -> 
            type_expr (Evar v) cur_efun.funvartyp fun_typ struct_typ >>= fun t -> 
            size_of_type struct_typ t >>= fun sz ->
            Hashtbl.replace cfg next (Cstore(Estk offs, val_assign, sz, succ));
            OK (next, next + 1)
      )   
  )
  | Elang.Iif (c, ithen, ielse) ->
      cfg_expr_of_eexpr c cur_efun fun_typ struct_typ >>= fun c ->
      cfg_node_of_einstr next cfg succ ithen cur_efun fun_typ struct_typ >>= fun (nthen, next) ->
      cfg_node_of_einstr next cfg succ ielse cur_efun fun_typ struct_typ >>= fun (nelse, next) ->
      Hashtbl.replace cfg next (Ccmp(c, nthen, nelse)); OK (next, next + 1)
  | Elang.Iwhile (c, i) ->
      cfg_expr_of_eexpr c cur_efun fun_typ struct_typ >>= fun c ->
      let (cmp, next) = (next, next+1) in
      cfg_node_of_einstr next cfg cmp i cur_efun fun_typ struct_typ >>= fun (nthen, next) ->
      Hashtbl.replace cfg cmp (Ccmp(c, nthen, succ)); OK (cmp, next + 1)
  | Elang.Iblock il ->  (
      if (List.is_empty il) 
        then 
          (Hashtbl.replace cfg next (Cnop succ); 
          OK (next, next+1))
      else 
        List.fold_right (fun i acc ->
            acc >>= fun (succ, next) ->
            cfg_node_of_einstr next cfg succ i cur_efun fun_typ struct_typ
          ) il (OK (succ, next))
  )
  | Elang.Ireturn e ->
      cfg_expr_of_eexpr e cur_efun fun_typ struct_typ >>= fun e ->
      Hashtbl.replace cfg next (Creturn e); OK (next, next + 1)

  | Elang.Iprint e ->
      cfg_expr_of_eexpr e cur_efun fun_typ struct_typ >>= fun e ->
      Hashtbl.replace cfg next (Cprint (e,succ));
      OK (next, next + 1)

  | Elang.Icall (fname, argms) ->
      let f_fold a argi = 
        a >>= fun a -> 
        cfg_expr_of_eexpr argi cur_efun fun_typ struct_typ >>= fun cfg_expri ->
          OK (a @ [cfg_expri])
      in
      List.fold_left f_fold (OK []) argms >>= fun cfg_args -> 
        Hashtbl.replace cfg next (Ccall (fname, cfg_args, succ));
        OK (next, next+1)

  | Elang.Istore (e1, e2) ->( 
      cfg_expr_of_eexpr e2 cur_efun fun_typ struct_typ >>= fun e2_cfg ->
      type_expr e1 cur_efun.funvartyp fun_typ struct_typ >>= fun t -> 
        match t with 
            | Tptr ptr_t -> 
                cfg_expr_of_eexpr e1 cur_efun fun_typ struct_typ >>= fun e1_cfg ->
                size_of_type struct_typ ptr_t >>= fun sz_ptr_t ->
                Hashtbl.replace cfg next (Cstore (e1_cfg, e2_cfg, sz_ptr_t, succ));
                OK (next, next+1)
            | _ -> Error ("@cfg_gen.cfg_node_of_einstr : can not load data from non-pointer variable " ^ (string_of_typ t))
  ) 

  | Elang.Isetfield (e1, field, e2) -> (
      cfg_expr_of_eexpr e2 cur_efun fun_typ struct_typ >>= fun e2_cfg ->
      type_expr e1 cur_efun.funvartyp fun_typ struct_typ >>= fun t -> 
      match t with 
        | Tptr (Tstruct str_name) -> 
            cfg_expr_of_eexpr e1 cur_efun fun_typ struct_typ >>= fun e1_cfg ->(
            match e1_cfg with 
              | Estk str_pos -> 
                  field_offset struct_typ str_name field >>= fun field_offs -> 
                  field_type struct_typ str_name field >>= fun f_typ ->
                  size_of_type struct_typ f_typ >>= fun sz_f_typ ->
                  Hashtbl.replace cfg next (Cstore (Estk (str_pos + field_offs), e2_cfg, sz_f_typ, succ));
                  OK (next, next+1)
              | _ -> Error "@cfg_gen.cfg_expr_of_eexpr: Can not get field from a non struct pointer variable"
            )
        | _ -> Error "@cfg_gen.cfg_expr_of_eexpr: Can not get field from a non struct pointer variable"
  )

(* Some nodes may be unreachable after the CFG is entirely generated. The
   [reachable_nodes n cfg] constructs the set of node identifiers that are
   reachable from the entry node [n]. *)
let rec reachable_nodes n (cfg: (int,cfg_node) Hashtbl.t) =
  let rec reachable_aux n reach =
    if Set.mem n reach then reach
    else let reach = Set.add n reach in
      match Hashtbl.find_option cfg n with
      | None -> reach
      | Some (Cnop succ)
      | Some (Cprint (_, succ))
      | Some (Ccall (_,_,succ))
      | Some (Cstore (_,_,_,succ))
      | Some (Cassign (_, _, succ)) -> reachable_aux succ reach
      | Some (Creturn _) -> reach
      | Some (Ccmp (_, s1, s2)) ->
        reachable_aux s1 (reachable_aux s2 reach)
  in reachable_aux n Set.empty

(* [cfg_fun_of_efun f] builds the CFG for E function [f]. *)
let cfg_fun_of_efun (*{ funargs; funbody;funvartyp; funrettyp }*) efunc (fun_typ : (string, typ list * typ) Hashtbl.t)
    (struct_typ: (string, (string * typ) list) Hashtbl.t)=
  let cfg = Hashtbl.create 17 in
  Hashtbl.replace cfg 0 (Creturn (Eint 0));
  cfg_node_of_einstr 1 cfg 0 efunc.funbody efunc fun_typ struct_typ >>= fun (node, _) ->
  (* remove unreachable nodes *)
  let r = reachable_nodes node cfg in
  Hashtbl.filteri_inplace (fun k _ -> Set.mem k r) cfg;
  let arg_names = List.map (fun (key, v) -> key) efunc.funargs in
  OK { cfgfunargs = arg_names;
       cfgfunbody = cfg;
       cfgentry = node;
       cfgfunstksz = efunc.funstksz;
     }

let cfg_gdef_of_edef fun_typ struct_typ gd =
  match gd with
    Gfun f -> cfg_fun_of_efun f fun_typ struct_typ >>= fun f -> OK (Gfun f)

let cfg_prog_of_eprog (ep: eprog) : cfg_fun prog res =
  (*building funtyp*)
  let struct_typ = snd ep in 
  let efuns = fst ep in
  let fun_typ = Hashtbl.create (List.length efuns) in 
    Hashtbl.replace fun_typ "print" ([Tint], Tvoid);
    Hashtbl.replace fun_typ "print_int" ([Tint], Tvoid);
    Hashtbl.replace fun_typ "print_char" ([Tchar], Tvoid);
  List.iter (fun (fname, Gfun ef) -> 
    let arg_types = List.map (fun (key, v) -> v) ef.funargs in
    Hashtbl.replace fun_typ fname (arg_types, ef.funrettyp);
    ) efuns ;

  assoc_map_res (fun fname -> cfg_gdef_of_edef fun_typ struct_typ) efuns

let pass_cfg_gen ep =
  match cfg_prog_of_eprog ep with
  | Error msg ->
    record_compile_result ~error:(Some msg) "CFG"; Error msg
  | OK cfg ->
    record_compile_result "CFG";
    dump !cfg_dump dump_cfg_prog cfg (call_dot "cfg" "CFG");
    OK cfg


