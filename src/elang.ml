open Ast
open Batteries
open Prog
open Utils

type binop = Eadd | Emul | Emod | Exor | Ediv | Esub (* binary operations *)
           | Eclt | Ecle | Ecgt | Ecge | Eceq | Ecne (* comparisons *)
type unop = Eneg

type expr =
  | Ecall of string*(expr list) 
  | Ebinop of binop * expr * expr
  | Eunop of unop * expr
  | Eint of int
  | Evar of string
  | Echar of char

type instr =
  | Iassign of string * expr
  | Icall of string * (expr list)
  | Iif of expr * instr * instr
  | Iwhile of expr * instr
  | Iblock of instr list
  | Ireturn of expr
  | Iprint of expr

type efun = {
  funargs: ( string*typ ) list;
  funbody: instr;
  funvartyp: (string, typ) Hashtbl.t;
  funrettyp: typ;
}

type eprog = efun prog



