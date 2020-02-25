open Batteries
open BatList
open Elang
open Rtl
open Ltl
open Ltl_print
open Utils
open Prog
open Options

(* This file performs the translation from LTL programs to RISC-V assembly
   programs. The languages are basically the same, so the only thing to do here
   is to print LTL instructions in RISC-V assembly syntax.

   Another thing to do is to deal with command-line arguments. These are given
   as strings and need to be converted into strings. That is the purpose of the
   [riscv_load_args] function in this file.

   Finally, [riscv_prelude] gives an entry point in the program : a global
   "_start" symbol is created, which points to startup code, whose job is the
   following:
   - initializes the global pointer (for "heap" allocation),
   - sets up the stack,
   - retrieves arguments,
   - calls the "main" function of our program,
   - prints the return value to the screen,
   - and finally exits the program by issuing the appropriate system call.

*)

let riscv_of_cmp (cmp: rtl_cmp) =
  match cmp with
  | Rtl.Rclt -> "blt"
  | Rtl.Rcle -> "ble"
  | Rtl.Rcgt -> "bgt"
  | Rtl.Rcge -> "bge"
  | Rtl.Rceq -> "beq"
  | Rtl.Rcne -> "bne"

let print_binop (b: binop) =
  match b with
  | Elang.Eadd -> "add"
  | Elang.Emul -> "mul"
  | Elang.Emod -> "remu"
  | Elang.Exor -> "xor"
  | Elang.Ediv -> "div"
  | Elang.Esub -> "sub"
  | Elang.Eclt -> "slt"
  | Elang.Ecle -> "sle"
  | Elang.Ecgt -> "sgt"
  | Elang.Ecge -> "sge"
  | Elang.Eceq -> "seq"
  | Elang.Ecne -> "sne"

let print_unop (u: unop) =
  match u with
  | Elang.Eneg -> "neg"

let instrsuffix_of_size sz =
  match !Archi.archi, sz with
  | _, 1 -> 'b'
  | _, 4 -> 'w'
  | A64, 8 -> 'd'
  | _, _ ->
    failwith (Format.sprintf "Impossible write size (%d) in archi (%d)"
                sz !Archi.nbits)

let dump_riscv_instr oc (i: ltl_instr) =
  match i with
  | LAddi(rd, rs, i) ->
    Format.fprintf oc "addi %s, %s, %d\n" (print_reg rd) (print_reg rs) i
  | LSubi(rd, rs, i) ->
    Format.fprintf oc "addi %s, %s, %d\n" (print_reg rd) (print_reg rs) (-i)
  | LBinop(b, rd, rs1, rs2) ->
     (* TODO *)
     Format.fprintf oc "%s %s, %s, %s\n"
               (print_binop b) (print_reg rd) (print_reg rs1) (print_reg rs2)
  | LUnop(u, rd, rs) ->
    Format.fprintf oc "%s %s, %s\n"
      (print_unop u) (print_reg rd) (print_reg rs)
  | LStore(rt, i, rs, sz) ->
    Format.fprintf oc "s%c %s, %d(%s)\n"
      (instrsuffix_of_size sz) (print_reg rs) i (print_reg rt)
  | LLoad(rd, rt, i, sz) ->
    Format.fprintf oc "l%c %s, %d(%s)\n"
      (instrsuffix_of_size sz) (print_reg rd) i (print_reg rt)
  | LMov(rd, rs) ->
    Format.fprintf oc "mv %s, %s\n" (print_reg rd) (print_reg rs)
  | LLabel l ->
    Format.fprintf oc "%s:\n" l
  | LJmp l -> Format.fprintf oc "j %s\n" l
  | LJmpr r -> Format.fprintf oc "jr %s\n" (print_reg r)
  | LConst (rd, i) -> Format.fprintf oc "li %s, %d\n\n" (print_reg rd) i
  | LComment l -> Format.fprintf oc "# %s\n" l
  | LBranch(cmp, rs1, rs2, s) ->
    Format.fprintf oc "%s %s, %s, %s\n"
      (riscv_of_cmp cmp) (print_reg rs1) (print_reg rs2) s
  | LCall fname ->
    Format.fprintf oc "jal ra, %s\n" fname
  | LHalt -> Format.fprintf oc "halt\n"

let dump_riscv_fun oc (fname , lf) =
  Format.fprintf oc "%s:\n" fname;
  List.iter (dump_riscv_instr oc) lf.ltlfunbody

let riscv_load_args oc =
  let nargs = [1;2;3;4;5;6;7;8] in
  (* for each arg in [1..8]:
       a0 <- arg
       call load_int_arg
       call atoi
       sd a0, -8*arg(fp)
  *)
  let l1 = nargs |>
           List.map (fun i ->
               [LConst(reg_a0, i);
                LCall("load_int_arg");
                LCall("atoi");
                LStore(reg_fp, - !Archi.wordsize*i,
                       reg_a0, !Archi.wordsize)
               ]) in
  (* for each arg in [1..8]
     ld a{arg-1}, -8*arg(fp)
  *)
  let l2 = nargs |>
           List.map (fun i ->
               [LLoad(starting_arg_register + i - 1, reg_fp,
                      - !Archi.wordsize*i, !Archi.wordsize)]) in
  (l1 @ l2) |> List.concat |> List.iter (fun i -> dump_riscv_instr oc i)


let riscv_fun_load_arg oc () =
  ("load_int_arg",{
      ltlfunargs = 0;
      (*
         t0 <- Archi.wordsize (in this example 8)
         mul a0, a0, t0
         add t0, fp, a0
         ld a0, 8(t0)
         jmpr ra
      *)
      ltlfunbody = [LConst(reg_t0, !Archi.wordsize);
                    LBinop(Emul, reg_ret, reg_ret, reg_t0);
                    LBinop(Eadd, reg_t0, reg_fp, reg_ret);
                    LLoad(reg_ret, reg_t0, !Archi.wordsize, !Archi.wordsize);
                    LJmpr reg_ra
                   ];
      ltlfuninfo = [];
      ltlregalloc = []
    }) |> dump_riscv_fun oc

let rv_store () =
  Format.sprintf "s%c" !Archi.instrsuffix
let rv_load () =
  Format.sprintf "l%c" !Archi.instrsuffix

let riscv_prelude oc =
  Format.fprintf oc ".globl _start\n";
  Format.fprintf oc "_start:\n";
  Format.fprintf oc "  lui gp, %%hi(_heap_start)\n";
  Format.fprintf oc "  addi gp, gp, %%lo(_heap_start)\n";
  Format.fprintf oc "  addi t0, gp, 8\n";
  Format.fprintf oc "  %s t0, 0(gp)\n" (rv_store ());
  Format.fprintf oc "  mv s0, sp\n";
  Format.fprintf oc "  add sp, sp, -72\n";
  riscv_load_args oc;
  Format.fprintf oc "jal ra, main\n";
  Format.fprintf oc "mv s0, a0\n";
  Format.fprintf oc "jal ra, println\n";
  Format.fprintf oc "mv a0, s0\n";
  Format.fprintf oc "jal ra, print_int\n";
  Format.fprintf oc "jal ra, println\n";
  Format.fprintf oc "addi a7, zero, 93\n";
  Format.fprintf oc "ecall\n"

let dump_riscv_prog oc lp =
  if !nostart then () else riscv_prelude oc;
  Format.fprintf oc ".global main\n";
  List.iter (function
        (fname, Gfun f) -> dump_riscv_fun oc (fname,f)
    ) lp;
  riscv_fun_load_arg oc ()
