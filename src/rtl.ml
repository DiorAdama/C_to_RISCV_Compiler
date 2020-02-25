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
