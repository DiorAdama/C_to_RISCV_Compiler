open Batteries
open Elang
open Cfg
open Rtl
open Prog
open Utils
open Report
open Rtl_print
open Options

(* Une partie de la génération de RTL consiste à allouer les variables dans des
   pseudo-registres RTL.

   Ces registres sont en nombre illimité donc ce problème est facile.

   Étant donnés :
   - [next_reg], le premier numéro de registre disponible (pas encore alloué à
   une variable)
   - [var2reg], une liste d'associations dont les clés sont des variables et les
   valeurs des numéros de registres
   - [v] un nom de variable (de type [string]),

   [find_var (next_reg, var2reg) v] renvoie un triplet [(r, next_reg, var2reg)]:

   - [r] est le registre RTL associé à la variable [v]
   - [next_reg] est le nouveau premier registre disponible
   - [var2reg] est la nouvelle association nom de variable/registre.

*)
let find_var (next_reg, var2reg) v =
  match List.assoc_opt v var2reg with
    | Some r -> (r, next_reg, var2reg)
    | None -> (next_reg, next_reg + 1, assoc_set var2reg v next_reg)

(* [rtl_instrs_of_cfg_expr (next_reg, var2reg) e] construit une liste
   d'instructions RTL correspondant à l'évaluation d'une expression E.

   Le retour de cette fonction est un quadruplet [(r,l,next_reg,var2reg)], où :
   - [r] est le registre RTL dans lequel le résultat de l'évaluation de [e] aura
     été stocké
   - [l] est une liste d'instructions RTL.
   - [next_reg] est le nouveau premier registre disponible
   - [var2reg] est la nouvelle association nom de variable/registre.
*)
let rec rtl_instrs_of_cfg_expr (next_reg, var2reg) (e: expr) =
    match  e with
      | Evar st -> 
          let r, next_reg, var2reg = find_var (next_reg, var2reg) st in
            (r, [], next_reg, var2reg)
      
      | Eint i -> 
          let l = [Rconst (next_reg, i)] in
            (next_reg, l, next_reg + 1, var2reg)

      | Eunop (unar, ex) ->
          let r, l, next_reg, var2reg = rtl_instrs_of_cfg_expr (next_reg, var2reg) ex in
            let instr = Runop (unar, next_reg, r) in 
              (next_reg, l @ [instr], next_reg+1, var2reg)   
              
      | Ebinop (binar, ex1, ex2) ->
          let r1, l1, next_reg, var2reg = rtl_instrs_of_cfg_expr (next_reg, var2reg) ex1 in
            let r2, l2, next_reg, var2reg = rtl_instrs_of_cfg_expr (next_reg, var2reg) ex2 in
              let instr = Rbinop (binar, next_reg, r1, r2) in
                (next_reg, l1 @ l2 @ [instr], next_reg+1, var2reg)

      | Ecall (fname, cfg_expr_list) -> 
          
          let f_fold (regs, instrs, n_reg, varToReg) cfg_expri = 
            let r, l, n_reg, varToreg = rtl_instrs_of_cfg_expr (n_reg, varToReg) cfg_expri in
              (regs @ [r], instrs @ l, n_reg, varToReg)
          in 
          let (regs, instrs, next_reg, var2reg) = List.fold_left f_fold ([], [], next_reg, var2reg) cfg_expr_list in 
          let call_instr = Rcall (Some next_reg, fname, regs) in 
          (next_reg, instrs @ [call_instr], next_reg+1, var2reg)

      | Estk _ -> (next_reg, [], next_reg, var2reg)
      | Eload _ -> (next_reg, [], next_reg, var2reg)

    

    


let is_cmp_op =
  function Eclt -> Some Rclt
         | Ecle -> Some Rcle
         | Ecgt -> Some Rcgt
         | Ecge -> Some Rcge
         | Eceq -> Some Rceq
         | Ecne -> Some Rcne
         | _ -> None

let rtl_cmp_of_cfg_expr (e: expr) =
  match e with
  | Ebinop (b, e1, e2) ->
    (match is_cmp_op b with
     | None -> (Rcne, e, Eint 0)
     | Some rop -> (rop, e1, e2))
  | _ -> (Rcne, e, Eint 0)


let rtl_instrs_of_cfg_node ((next_reg:int), (var2reg: (string*int) list)) (c: cfg_node) =
    match c with
      | Cassign (var, ex, i) -> 
          let rs, l, next_reg, var2reg = rtl_instrs_of_cfg_expr (next_reg, var2reg) ex in 
            let rd, next_reg, var2reg = find_var (next_reg, var2reg) var in
              let instr = Rmov (rd, rs) in 
                let jmp_instr = Rjmp i in
                  (l @ [instr; jmp_instr], next_reg, var2reg)

      | Creturn ex -> 
          let r, l, next_reg, var2reg = rtl_instrs_of_cfg_expr (next_reg, var2reg) ex in 
            let instr = Rret r in 
          (l @ [instr], next_reg, var2reg)

      | Cprint (ex, i) ->
          let r, l, next_reg, var2reg = rtl_instrs_of_cfg_expr (next_reg, var2reg) ex in 
            let instr = Rprint r in 
              let jmp_instr = Rjmp i in
          (l @ [instr; jmp_instr], next_reg, var2reg) 
              
      | Cnop i -> ([Rjmp i], next_reg, var2reg) 
 
      | Ccmp (ex, i1, i2) -> 
          let rop, e1, e2 = rtl_cmp_of_cfg_expr ex in 
            let r1, l1, next_reg, var2reg = rtl_instrs_of_cfg_expr (next_reg, var2reg) e1 in 
              let r2, l2, next_reg, var2reg = rtl_instrs_of_cfg_expr (next_reg, var2reg) e2 in 
                let jmp_instr2 = Rjmp i2 in
                  let branch_instr = Rbranch (rop, r1, r2, i1) in 
                        
          ( l1 @ l2 @ [ branch_instr; jmp_instr2], next_reg, var2reg )

      | Ccall (fname, cfg_expr_list, succ) -> 
          let (r, l, next_reg, var2reg) = rtl_instrs_of_cfg_expr (next_reg, var2reg) (Ecall (fname, cfg_expr_list)) in 
          let jmp_instr = Rjmp succ in 
          ( l @ [jmp_instr], next_reg, var2reg )

      | Cstore _ -> ([], next_reg, var2reg)
    

let rtl_instrs_of_cfg_fun cfgfunname ({ cfgfunargs; cfgfunbody; cfgentry }: cfg_fun) =
  let (rargs, next_reg, var2reg) =
    List.fold_left (fun (rargs, next_reg, var2reg) a ->
        let (r, next_reg, var2reg) = find_var (next_reg, var2reg) a in
        (rargs @ [r], next_reg, var2reg)
      )
      ([], 0, []) cfgfunargs
  in
  let rtlfunbody = Hashtbl.create 17 in
  let (next_reg, var2reg) = Hashtbl.fold (fun n node (next_reg, var2reg)->
      let (l, next_reg, var2reg) = rtl_instrs_of_cfg_node (next_reg, var2reg) node in
      Hashtbl.replace rtlfunbody n l;
      (next_reg, var2reg)
    ) cfgfunbody (next_reg, var2reg) in
  {
    rtlfunargs = rargs;
    rtlfunentry = cfgentry;
    rtlfunbody;
    rtlfuninfo = var2reg;
  }

let rtl_of_gdef funname = function
    Gfun f -> Gfun (rtl_instrs_of_cfg_fun funname f)

let rtl_of_cfg cp = List.map (fun (s, gd) -> (s, rtl_of_gdef s gd)) cp

let pass_rtl_gen cfg =
  let rtl = rtl_of_cfg cfg in
  dump !rtl_dump dump_rtl_prog rtl
    (fun file () -> add_to_report "rtl" "RTL" (Code (file_contents file)));
  OK rtl

  
