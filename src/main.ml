open Batteries
open BatList
open Symbols
open Parser
open Ast
open Elang
open Elang_run
open Elang_print
open Elang_gen
open Cfg
open Cfg_run
open Cfg_print
open Cfg_gen
open Cfg_constprop
open Cfg_dead_assign
open Cfg_nop_elim
open Rtl
open Rtl_run
open Rtl_print
open Rtl_gen
open Linear
open Linear_run
open Linear_print
open Linear_gen
open Linear_liveness
open Linear_dse
open Ltl
open Ltl_run
open Ltl_print
open Ltl_gen
open Ltl_debug
open Riscv
open Utils
open Archi
open Report
open Options
open Lexer_generator
open Tokenize

let speclist =
  [
    ("-show-tokens", Arg.String (fun s -> show_tokens := Some s), "Output the list of tokens recognized by the lexer.");
    ("-ast-tree", Arg.String (fun s -> ast_tree := Some s), "Output DOT file for dumping the tree.");
    ("-ast-dump", Arg.Set ast_dump, "Dumps the tree in textual form.");
    ("-e-dump", Arg.String (fun s -> e_dump := Some s), "Output Elang file.");
    ("-e-run", Arg.Set e_run, "Run Elang program.");
    ("-cfg-dump", Arg.String (fun s -> cfg_dump := Some s), "Output CFG file.");
    ("-cfg-run", Arg.Set cfg_run, "Run CFG program.");
    ("-cfg-run-after-cp", Arg.Set cfg_run_after_cp, "Run CFG program after constant propagation.");
    ("-cfg-run-after-dae", Arg.Set cfg_run_after_dae, "Run CFG program after dead assign elimination.");
    ("-cfg-run-after-ne", Arg.Set cfg_run_after_ne, "Run CFG program after nop elimination.");
    ("-rtl-dump", Arg.String (fun s -> rtl_dump := Some s), "Output RTL file.");
    ("-rtl-run", Arg.Set rtl_run, "Run RTL program.");
    ("-linear-dump", Arg.String (fun s -> linear_dump := Some s), "Output Linear file.");
    ("-linear-run", Arg.Set linear_run, "Run Linear program.");
    ("-linear-run-after-dse", Arg.Set linear_run_after_dse, "Run Linear program after dead store elimination.");
    ("-ltl-dump", Arg.String (fun s -> ltl_dump := Some s), "Output LTL file.");
    ("-ltl-run", Arg.Set ltl_run, "Run LTL program.");
    ("-ltl-debug", Arg.Set ltl_debug, "Debug LTL program.");
    ("-riscv-dump", Arg.String (fun s -> riscv_dump := Some s), "Output RISC-V file.");
    ("-riscv-run", Arg.Set riscv_run, "Run RISC-V program.");
    ("-no-dump", Arg.Set no_dump, "Do not dump anything but the .s file");
    ("-no-dot", Arg.Set no_dot, "Do not call dot on CFG dumps (default false)");
    ("-clever-regalloc", Arg.Unit (fun () -> naive_regalloc := false), "Use the graph coloring algorithm for register allocation.");
    ("-naive-regalloc", Arg.Unit (fun () -> naive_regalloc := true),
     "Use the naive algorithm for register allocation (all pseudo-registers go on the stack).");
    ("-no-cfg-constprop", Arg.Set no_cfg_constprop, "Disable CFG constprop");
    ("-no-cfg-dae", Arg.Set no_cfg_dae, "Disable CFG Dead Assign Elimination");
    ("-no-cfg-ne", Arg.Set no_cfg_ne, "Disable CFG Nop Elimination");
    ("-no-linear-dse", Arg.Set no_linear_dse, "Disable Linear Dead Store Elimination");
    ("-rig-dump", Arg.String (fun s -> rig_dump := Some s),
    "Path to output the register interference graph");
    ("-all-run", Arg.Unit (fun () ->
         e_run := true;
         cfg_run := true;
         cfg_run_after_cp := true;
         cfg_run_after_dae := true;
         cfg_run_after_ne := true;
         rtl_run := true;
         linear_run := true;
         linear_run_after_dse := true;
         ltl_run := true;
         riscv_run := true;
       ), "Run all intermediate languages");
    ("-heap", Arg.Set_int heapsize, "Heap size");
    ("-show", Arg.Set show, "Show Results");
    ("-m32", Arg.Unit (fun _ -> Archi.archi := A32), "32bit mode");
    ("-f", Arg.String (fun s -> input_file := Some s), "file to compile");
    ("-alloc-order-ts", Arg.Unit (fun _ -> Options.alloc_order_st := false), "Allocate t regs before s regs");
    ("-json", Arg.String (fun s -> output_json := s), "Output JSON summary");
    ("-nostart", Arg.Set nostart, "Don't output _start code.");
    ("-nostats", Arg.Set nostats, "Don't output stats.");
    ("-nomul", Arg.Unit (fun _ -> has_mul := false), "Target architecture without mul instruction.");
    ("-linux", Arg.Unit (fun _ -> target := Linux), "emit linux syscalls");
    ("-xv6", Arg.Unit (fun _ -> target := Xv6), "emit xv6 syscalls");
    ("--", Arg.Rest (fun p -> params := int_of_string p::!params), "Run parameters.")
  ]

let set_default r v suff =
  match !r with
    None -> r := Some (v ^ suff)
  | _ -> ()

let compile_rv basename asmfile () =
  if not !Options.nostart then begin
    let obj_file_prog = Filename.temp_file ~temp_dir:"/tmp" "" ".o" in
    let cmdas_prog = Format.sprintf "%s -I%s -o %s %s"
        (Archi.assembler ())
        (Archi.runtime_lib_include_path ())
        obj_file_prog asmfile in
    let obj_file_lib = Filename.temp_file ~temp_dir:"/tmp" "" ".o" in
    let cmdas_lib = Format.sprintf "%s -I%s -o %s %s"
        (Archi.assembler ())
        (Archi.runtime_lib_include_path ())
        obj_file_lib (Archi.runtime_lib_path ()) in
    let cmdld = Format.sprintf "%s -T %s/link.ld %s %s -o %s.exe"
        (Archi.linker ())
        Config.runtime_dir
        obj_file_prog obj_file_lib
        basename in
    Printf.printf "AS: %s\n" cmdas_prog;
    Printf.printf "AS: %s\n" cmdas_lib;
    Printf.printf "LD: %s\n" cmdld;
    let out_as_prog = cmd_to_list cmdas_prog in
    let out_as_lib = cmd_to_list cmdas_lib in
    let out_ld = cmd_to_list cmdld in
    let out = out_as_prog @ out_as_lib @ out_ld in
    match out with
      [] -> None
    | _ -> Some (String.concat "\n" out)
  end
  else None

let exec_rv_prog ltl basename oc rvp heapsize params =
  let rvp =
    match rvp with
      Some rvp -> rvp
    | None ->
      let f = Filename.temp_file ~temp_dir:"/tmp" basename ".s" in
      f
  in
  let error = ref None in
  dump (Some rvp) (dump_riscv_prog !Archi.target) ltl (fun file () ->
      error := compile_rv basename file ());
  match !error with
  | Some e -> Error ("RiscV generation error:\n" ^e)
  | None ->
    let l = cmd_to_list (Format.sprintf "%s \"%s.exe\" %s" (Archi.qemu ())  basename
                           (params |> List.map string_of_int |> String.concat " " )) in
    try
      let all_but_last = l |> List.rev |> List.tl |> List.rev in
      all_but_last |> print_list (fun oc -> Format.fprintf oc "%s") "" "\n" "" oc;
      let ret = l |> List.last |> int_of_string in
      OK (Some ret)
    with _ -> OK None


let _ =
  Arg.parse speclist (fun s -> ()) "Usage";
  Archi.archi := !archi;
  match !input_file with
  | None -> failwith "No input file specified.\n"
  | Some input ->
    add_to_report "Source" "Source" (Code (file_contents input));

    match Filename.chop_suffix_opt ".e" input with
      None -> failwith
                (Format.sprintf "File (%s) should end in .e" input)
    | Some basename ->
      params := List.rev !params;
      set_default riscv_dump basename ".s";
      if not !no_dump then begin
        set_default show_tokens basename ".lex";
        set_default ast_tree basename ".ast";
        set_default e_dump basename ".e.dump";
        set_default cfg_dump basename ".cfg";
        set_default rtl_dump basename ".rtl";
        set_default linear_dump basename ".linear";
        set_default rig_dump basename ".rig";
        set_default ltl_dump basename ".ltl";
      end;

      let compiler_res =
        pass_tokenize input >>= fun tokens ->
        pass_parse tokens >>= fun (ast, _) ->
        pass_elang ast >>= fun ep ->
        run "Elang" !e_run eval_eprog ep;
        pass_cfg_gen ep >>= fun cfg ->
        run "CFG" !cfg_run eval_cfgprog cfg;
        pass_constant_propagation cfg >>= fun cfg ->
        run "CFG after constant_propagation" !cfg_run_after_cp eval_cfgprog cfg;
        pass_dead_assign_elimination cfg >>= fun cfg ->
        run "CFG after dead_assign_elimination" !cfg_run_after_dae eval_cfgprog cfg;
        pass_nop_elimination cfg >>= fun cfg ->
        run "CFG after nop_elimination" !cfg_run_after_ne eval_cfgprog cfg;
        pass_rtl_gen cfg >>= fun rtl ->
        run "RTL" !rtl_run exec_rtl_prog rtl;
        pass_linearize rtl >>= fun (linear, lives) ->
        run "Linear" !linear_run exec_linear_prog linear;
        pass_linear_dse linear lives >>= fun linear ->
        run "Linear after DSE" !linear_run_after_dse exec_linear_prog linear;
        pass_ltl_gen linear
      in
      begin
        match compiler_res with
        | Error msg -> ()
        | OK ltl ->
          run "LTL" !ltl_run (exec_ltl_prog) ltl;
          (if !ltl_debug then debug_ltl_prog input ltl !heapsize !params);
          dump !riscv_dump (dump_riscv_prog !Archi.target) ltl (fun file () ->
              add_to_report "riscv" "RISC-V" (Code (file_contents file));
              ignore (compile_rv basename file ()));
          if not !Options.nostart then begin
            run "Risc-V" !riscv_run (exec_rv_prog ltl basename) !riscv_dump
          end
      end;
      dump (Some !output_json) (fun oc p ->
          Format.fprintf oc "%s\n" p
        ) (json_output_string ()) (fun _ () -> ());
      make_report input report ()
