open Batteries
open BatList
open Rtl
open Prog
open Utils

type linear_fun = {
  linearfunargs: reg list;
  linearfunbody: rtl_instr list;
  linearfuninfo: (string*reg) list;
}
