open Elang
open Prog
open Utils
open Batteries
open BatList

type expr =
    Ebinop of binop * expr * expr
  | Eunop of unop * expr
  | Eint of int
  | Evar of string

type cfg_node =
  | Cassign of string * expr * int
  | Creturn of expr
  | Cprint of expr * int
  | Ccmp of expr * int * int
  | Cnop of int

type cfg_fun = {
  cfgfunargs: string list;
  cfgfunbody: (int, cfg_node) Hashtbl.t;
  cfgentry: int;
}

type cprog = cfg_fun prog


let size_binop b e1 e2 =
  1 + e1 + e2

let size_unop u e =
  1 + e

let rec size_expr (e: expr) : int =
  match e with
  | Ebinop (b, e1, e2) -> size_binop b (size_expr e1) (size_expr e2)
  | Eunop (u, e) -> size_unop u (size_expr e)
  | Eint _ -> 1
  | Evar v -> 1

let rec size_instr (i: cfg_node) : int =
  match (i : cfg_node) with
  | Cassign (v, e, s) -> 1 + size_expr e
  | Creturn e -> 1 + (size_expr e)
  | Cprint (e, s) -> 1 + (size_expr e)
  | Ccmp (e, s1, s2) -> 1 + size_expr e
  | Cnop s -> 1

let size_fun f =
  Hashtbl.fold (fun k v acc -> acc + size_instr v) f 0
