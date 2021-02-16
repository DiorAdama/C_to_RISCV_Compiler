open Batteries
open BatList
open Elang
open Cfg
open Utils
open Prog

type reg = int

type rtl_cmp = Rclt | Rcle | Rcgt | Rcge | Rceq | Rcne

type rtl_instr = Rbinop of binop * reg * reg * reg
               | Runop of unop * reg * reg
               | Rconst of reg * int
               | Rbranch of rtl_cmp * reg * reg * int
               | Rjmp of int
               | Rmov of reg * reg
               | Rret of reg
               | Rlabel of int
               | Rprint of reg

type rtl_fun = { rtlfunargs: reg list;
                 rtlfunbody: (int, rtl_instr list) Hashtbl.t;
                 rtlfunentry: int;
                 rtlfuninfo: (string*reg) list
               }

let written_rtl_regs_instr (i: rtl_instr) =
  match i with
  | Rbinop (_, rd, _, _)
  | Runop (_, rd, _)
  | Rconst (rd, _)
  | Rmov (rd, _) -> Set.singleton rd
  | Rprint _
  | Rret _
  | Rlabel _
  | Rbranch (_, _, _, _)
  | Rjmp _ -> Set.empty

let written_rtl_regs (l: rtl_instr list) =
  List.fold_left (fun acc i -> Set.union acc (written_rtl_regs_instr i))
    Set.empty l
