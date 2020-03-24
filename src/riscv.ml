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
  | _, 1 -> OK 'b'
  | _, 4 -> OK 'w'
  | A64, 8 -> OK 'd'
  | _, _ ->
    Error (Format.sprintf "Impossible write size (%d) in archi (%d)"
                sz !Archi.nbits)

let dump_riscv_instr oc (i: ltl_instr) =
  match i with
  | LAddi(rd, rs, i) ->
    Format.fprintf oc "addi %s, %s, %d\n" (print_reg rd) (print_reg rs) i;
    OK ()
  | LSubi(rd, rs, i) ->
    Format.fprintf oc "addi %s, %s, %d\n" (print_reg rd) (print_reg rs) (-i);
    OK ()
  | LBinop(b, rd, rs1, rs2) ->
    begin match b with
      | Elang.Eclt ->
        Format.fprintf oc "slt %s, %s, %s\n"
          (print_reg rd) (print_reg rs1) (print_reg rs2);
        OK ()
      | Elang.Ecgt ->
        Format.fprintf oc "slt %s, %s, %s\n"
          (print_reg rd) (print_reg rs2) (print_reg rs1);
        OK ()
      | Elang.Ecle ->
        (* 'rd <- rs1 <= rs2' == 'rd <- rs2 < rs1; rd <- not rd' *)
        Format.fprintf oc "slt %s, %s, %s\n"
          (print_reg rd) (print_reg rs2) (print_reg rs1);
        Format.fprintf oc "not %s, %s\n"
          (print_reg rd) (print_reg rd);
        OK ()
      | Elang.Ecge ->
        Format.fprintf oc "slt %s, %s, %s\n"
          (print_reg rd) (print_reg rs1) (print_reg rs2);
        Format.fprintf oc "not %s, %s\n"
          (print_reg rd) (print_reg rd);
        OK ()
      | Elang.Eceq ->
        Format.fprintf oc "sub %s, %s, %s\n"
          (print_reg rd) (print_reg rs1) (print_reg rs2);
        Format.fprintf oc "seqz %s, %s\n"
          (print_reg rd) (print_reg rd);
        OK ()
      | Elang.Ecne ->
        Format.fprintf oc "sub %s, %s, %s\n"
          (print_reg rd) (print_reg rs1) (print_reg rs2);
        Format.fprintf oc "snez %s, %s\n"
          (print_reg rd) (print_reg rd);
        OK ()
      | _ -> Format.fprintf oc "%s %s, %s, %s\n"
               (print_binop b) (print_reg rd) (print_reg rs1) (print_reg rs2);
        OK ()
    end
  | LUnop(u, rd, rs) ->
    Format.fprintf oc "%s %s, %s\n"
          (print_unop u) (print_reg rd) (print_reg rs); OK ()
  | LStore(rt, i, rs, sz) ->
    (instrsuffix_of_size sz) >>= fun sz ->
    OK (Format.fprintf oc "s%c %s, %d(%s)\n"
          sz (print_reg rs) i (print_reg rt))
  | LLoad(rd, rt, i, sz) ->
    (instrsuffix_of_size sz) >>= fun sz ->
    Format.fprintf oc "l%c %s, %d(%s)\n"
      sz (print_reg rd) i (print_reg rt); OK ()
  | LMov(rd, rs) ->
    Format.fprintf oc "mv %s, %s\n" (print_reg rd) (print_reg rs);
    OK ()
  | LLabel l ->
    Format.fprintf oc "%s:\n" l;
    OK ()
  | LJmp l -> Format.fprintf oc "j %s\n" l;
    OK ()
  | LJmpr r -> Format.fprintf oc "jr %s\n" (print_reg r);
    OK ()
  | LConst (rd, i) -> Format.fprintf oc "li %s, %d\n\n" (print_reg rd) i;
    OK ()
  | LComment l -> Format.fprintf oc "# %s\n" l;
    OK ()
  | LBranch(cmp, rs1, rs2, s) ->
    Format.fprintf oc "%s %s, %s, %s\n"
      (riscv_of_cmp cmp) (print_reg rs1) (print_reg rs2) s;
    OK ()
  | LCall fname ->
    Format.fprintf oc "jal ra, %s\n" fname;
    OK ()
  | LHalt -> Format.fprintf oc "halt\n";
    OK ()

let dump_riscv_fun oc (fname , lf) =
  Format.fprintf oc "%s:\n" fname;
  list_iter_res (dump_riscv_instr oc) lf.ltlfunbody

let riscv_load_args oc : unit res =
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
                LBranch(Rceq, reg_a0, reg_zero, Printf.sprintf "riscv_load_arg_%d" i);
                LCall("atoi");
                LStore(reg_fp, - !Archi.wordsize*i,
                       reg_a0, !Archi.wordsize);
                LLabel(Printf.sprintf "riscv_load_arg_%d" i);
               ]) in
  (* for each arg in [1..8]
     ld a{arg-1}, -8*arg(fp)
  *)
  let l2 = nargs |>
           List.map (fun i ->
               [LLoad(starting_arg_register + i - 1, reg_fp,
                      - !Archi.wordsize*i, !Archi.wordsize)]) in
  (l1 @ l2) |> List.concat |> list_iter_res (fun i -> dump_riscv_instr oc i)


let riscv_fun_load_arg oc () =
  ("load_int_arg",{
      ltlfunargs = 0;
      (*

         *( fp + a0 * wordsize + 8)

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
  riscv_load_args oc >>= fun _ -> 
  Format.fprintf oc "jal ra, main\n";
  Format.fprintf oc "mv s0, a0\n";
  Format.fprintf oc "jal ra, println\n";
  Format.fprintf oc "mv a0, s0\n";
  Format.fprintf oc "jal ra, print_int\n";
  Format.fprintf oc "jal ra, println\n";
  Format.fprintf oc "addi a7, zero, 93\n";
  Format.fprintf oc "ecall\n";
  OK ()

let dump_riscv_prog oc lp : unit res =
  if !nostart then OK () else riscv_prelude oc >>= fun _ ->
  Format.fprintf oc ".global main\n";
  list_iter_res (function
        (fname, Gfun f) -> dump_riscv_fun oc (fname,f)
    ) lp >>= fun _ ->
  riscv_fun_load_arg oc ()
