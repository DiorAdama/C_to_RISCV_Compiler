open Batteries
open BatList
open Batteries
open BatList
open Prog
open Elang
open Cfg
open Elang_run
open Cfg_run
open Rtl
open Rtl_print
open Rtl_run
open Linear
open Ltl
open Utils
open Builtins

(* To execute LTL functions, we need to place instructions in memory, in
   particular because the [Jmpr] instruction jumps to an address contained in a
   register.

   The [fun_location] type records for a given function:
   - its starting and ending position in the code : [funstart] and [funend];
   - [funinfo] : a mapping from source variable names to RTL registers
   - [funregalloc] : a mapping from RTL registers to LTL locations.

   The [ltl_state] record gives the state of a program being executed:
   - [code] gives the LTL instruction at every position in the code;
   - [funs] gives a mapping from function names to [fun_location]s;
   - [regs] gives a mapping from LTL registers to their values;
   - [mem] is a memory state;
   - [numstep] is the number of steps taken in the current execution (useful for
     debugging).
*)

type fun_location = {
  funstart: int;
  funend: int;
  funinfo : (string * int) list;
  funregalloc: (int * Regalloc.loc) list
}

type ltl_state = {
  code: (int, ltl_instr) Hashtbl.t;
  funs: (string, fun_location) Hashtbl.t;
  regs: (ltl_reg, int) Hashtbl.t;
  mem: Mem.t;
  numstep: int ref;
}

(* Finds the position of a label in the code. *)
let find_label (code: (int, ltl_instr) Hashtbl.t) (l: string) =
  Hashtbl.fold (fun k v acc ->
      match acc with
      | OK x -> acc
      | Error _ -> if v = LLabel l then OK k else acc
    ) code (Error (Format.sprintf "Label %s not found." l))

(* For most instructions, the next instruction to execute is the one at [ip +
   1]. *)
let next ip = OK (Some (ip + 1))

(* Helper function to get value of register [r] in state [st]. *)
let get_reg st r =
  Hashtbl.find_option st.regs r >>
  (Format.sprintf "uninitialized register %s\n" (print_reg r))

(* Execution of one LTL instruction.

   - [oc] : the output channel where to print output of the program
   - [ip] : the instruction pointer : which instruction should we execute ?
   - [st] : an [ltl_state]

   Returns :
   - [OK (Some ip)] : execution should continue at ip [ip].
   - [OK None] : execution is finished
   - [Error msg] : something wrong happened.
*)

let exec_ltl_instr oc ip st : (int option) res =
  let open Utils in
  match Hashtbl.find_option st.code ip with
  | None -> Error (Format.sprintf
                     "Could not find next instruction to execute at ip=%d \
                      [in exec_ltl_instr]" ip)
  | Some i ->
    match i with
    | LAddi(rd, rs, i) ->
      get_reg st rs $ fun vs ->
        Hashtbl.replace st.regs rd (vs + i);
        next ip
    | LSubi(rd, rs, i) ->
      get_reg st rs $ fun vs ->
        Hashtbl.replace st.regs rd (vs - i);
        next ip
    | LBinop(b, rd, rs1, rs2) ->
      get_reg st rs1 $ fun vs1 ->
        get_reg st rs2 $ fun vs2 ->
          Hashtbl.replace st.regs rd (eval_binop b vs1 vs2);
          next ip
    | LUnop(u, rd, rs) ->
      get_reg st rs $ fun vs ->
        Hashtbl.replace st.regs rd (eval_unop u vs);
        next ip
    | LStore(rt, i, rs, sz) ->
      get_reg st rt $ fun vt ->
        get_reg st rs $ fun vs ->
          Mem.write_bytes st.mem (vt + i)  (split_bytes sz vs) >>= fun _ ->
          next ip
    | LLoad(rd, rt, i, sz) ->
      get_reg st rt $ fun vt ->
        Mem.read_bytes_as_int st.mem (vt + i) sz >>= fun (v) ->
        Hashtbl.replace st.regs rd v;
        next ip
    | LMov(rd, rs) ->
      get_reg st rs $ fun vs ->
        Hashtbl.replace st.regs rd vs;
        next ip
    | LLabel l -> next ip
    | LJmp l -> find_label st.code l >>= fun n -> OK (Some n)
    | LJmpr r ->
      get_reg st reg_ra $ fun ra -> OK (Some ra)
    | LConst (rd, i) -> Hashtbl.replace st.regs rd i; next ip
    | LComment _ -> next ip
    | LBranch(cmp, rs1, rs2, s) ->
      get_reg st rs1 $ fun vs1 ->
        get_reg st rs2 $ fun vs2 ->
          let b = eval_rtl_cmp cmp vs1 vs2 in
          if b
          then find_label st.code s >>= fun n -> OK (Some n)
          else next ip
    | LCall callee_name ->
      begin match Hashtbl.find_option st.funs callee_name with
          Some {funstart} ->
          Hashtbl.replace st.regs reg_ra (ip+1);
          OK (Some funstart)
        | None ->
          do_builtin oc st.mem callee_name
            (list_ints_desc number_of_arguments_passed_in_registers |> List.rev |>
             List.map (fun i -> i + starting_arg_register) |>
             List.map (fun i -> Hashtbl.find_default st.regs i 0)) >>=
          fun v ->
          begin match v with
            | None -> ()
            | Some v -> Hashtbl.replace st.regs reg_ret v
          end;
          next ip
      end
    | LHalt -> OK None

(* Initialize regs [0,n[ *)
let rec init_regs n =
  let regs = Hashtbl.create n in
  range n |> List.iter (fun n -> Hashtbl.replace regs n 0);
  regs

let init_state memsize lp params =
  let code : (int, ltl_instr) Hashtbl.t = Hashtbl.create 17 in
  let funs : (string, fun_location) Hashtbl.t = Hashtbl.create 17 in
  let mem = Mem.init memsize in
  let regs = init_regs 32 in
  let sp = memsize - !Archi.wordsize in
  Hashtbl.replace regs reg_sp sp;
  Hashtbl.replace regs reg_fp sp;
  Hashtbl.replace regs reg_ra 0;

  let codesize = List.fold_left (fun ofs (name, def) ->
      match def with
      | Gfun f ->
        let funstart = ofs in
        let funend = List.fold_left (fun ofs ins ->
            Hashtbl.replace code ofs ins;
            (* write dummy instruction in memory. *)
            Mem.write_char mem ofs 0x90 >>! fun _ ->
            ofs + 1
          ) ofs f.ltlfunbody in
        Hashtbl.replace funs name {
          funstart ; funend;
          funinfo = f.ltlfuninfo;
          funregalloc = f.ltlregalloc
        };
        funend
    ) 0 (("__halt", Gfun {ltlfunargs = 0;
                          ltlfunbody = [LHalt];
                          ltlfuninfo = [];
                          ltlregalloc = [];
                         })::lp)
  in

  let codesize = (codesize / 8 + 1) * 8 in
  Hashtbl.replace regs reg_gp codesize;

  (* write arguments, relative to sp *)
  List.iteri (fun i p ->
      if i >= number_of_arguments_passed_in_registers
      then begin
        let sp = Hashtbl.find regs reg_sp - !Archi.wordsize in
        Hashtbl.replace regs reg_sp sp;
        Mem.write_bytes mem sp (split_bytes !Archi.wordsize p) >>!
        ignore
      end else
        begin
          Hashtbl.replace regs (starting_arg_register + i) p
        end
    ) params;
  let mem_next = ref (codesize + 8) in
  Mem.write_bytes mem codesize (split_bytes !Archi.wordsize !mem_next) >>!
  fun _ -> { code; funs; mem ; regs ; numstep = ref 0}


let rec exec_ltl_at oc ip st =
  match exec_ltl_instr oc ip st with
    OK (Some ip) -> exec_ltl_at oc ip st
  | OK None -> OK st
  | Error msg -> Error msg

let exec_ltl_prog oc lp memsize params : int option res =
  let st = init_state memsize lp params in
  match Hashtbl.find_option st.funs "main" with
  | None -> Error (Format.sprintf "Could not find function main.")
  | Some {funstart} ->
    exec_ltl_at oc funstart st >>= fun st ->
    OK (Hashtbl.find_option st.regs reg_ret)

