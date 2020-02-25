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

let tokenize file =
  Lexer_generator.tokenize_file file >>= fun tokens ->
  OK (List.map (fun tok -> (tok, None)) tokens)

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
    ("-json", Arg.Set output_json, "Output JSON summary");
    ("-nostart", Arg.Set nostart, "Don't output _start code.");
    ("-nostats", Arg.Set nostats, "Don't output stats.");
    ("-nomul", Arg.Unit (fun _ -> has_mul := false), "Target architecture without mul instruction.");
    ("--", Arg.Rest (fun p -> params := int_of_string p::!params), "Run parameters.")
  ]



type run_result = {
  step: string;
  retval: int option;
  output: string;
  error: string option;
}

type compile_result = {
  step: string;
  error: string option;
  data: Yojson.t
}

type result = RunRes of run_result
            | CompRes of compile_result


let results = ref []

let run step flag eval p =
  if flag then begin
    begin match eval Format.str_formatter p !heapsize !params with
      | OK v ->
        let output = Format.flush_str_formatter () in
        results := !results @ [RunRes { step ; retval = v; output; error = None}];
        add_to_report step ("Run " ^ step) (
          Paragraph 
            (
              Printf.sprintf "With parameters : [%s]<br>\n" (String.concat"," (List.map string_of_int !params))
              ^ Printf.sprintf "Mem size : %d bytes<br>\n" !heapsize
              ^ Printf.sprintf "Return value : %s<br>\n" (match v with | Some v -> string_of_int v | _ -> "none")
              ^ Printf.sprintf "Output : <pre style=\"padding: 1em; background-color: #ccc;\">%s</pre>\n" output
            )
        )
      | Error msg ->
        let output  = Format.flush_str_formatter () in
        results := !results @ [RunRes { step ; retval = None; output; error = Some msg}];
        add_to_report step ("Run " ^ step) (
          Paragraph 
            (
              Printf.sprintf "With parameters : [%s]<br>\n" (String.concat"," (List.map string_of_int !params))
              ^ Printf.sprintf "Mem size : %d bytes<br>\n" !heapsize
              ^ Printf.sprintf "Return value : none<br>\n"
              ^ Printf.sprintf "Output : <pre style=\"padding: 1em; background-color: #ccc;\">%s</pre>\n" output
              ^ Printf.sprintf "Error : <pre style=\"padding: 1em; background-color: #fcc;\">\n%s</pre>\n" msg
            )
        )

    end
  end

let record_compile_result ?error:(error=None) ?data:(data=[]) step =
  let data = if not !Options.nostats then `List data else `Null in
  results := !results @ [CompRes { step; error; data}]

let dump file dumpf p additional_command =
  begin match file with
    | None -> ()
    | Some file ->
      let oc, close = 
        if file = "-"
        then (Format.std_formatter, fun _ -> ())
        else
          let oc = open_out file in
          (Format.formatter_of_out_channel oc, fun () -> close_out oc)
      in
      dumpf oc p; close ();
      additional_command file ()
  end



let process_output_to_list2 = fun command ->
  let chan = Unix.open_process_in command in
  let res = ref ([] : string list) in
  let rec process_otl_aux () =
    let e = input_line chan in
    res := e::!res;
    process_otl_aux() in
  try process_otl_aux ()
  with End_of_file ->
    let stat = Unix.close_process_in chan in (List.rev !res,stat)

let cmd_to_list command =
  let (l,_) = process_output_to_list2 command in l

let file_contents file =
  match
    let ic = open_in file in
    let rec aux s () =
      try
        let line = input_line ic in  (* read line from in_channel and discard \n *)
        aux (s ^ line ^ "\n") ()   (* close the input channel *)
      with e ->                      (* some unexpected exception occurs *)
        close_in_noerr ic;           (* emergency closing *)
        s in
    aux "" ()
  with
  | exception Sys_error _ -> failwith (Printf.sprintf "Could not open file %s\n" file)
  | x -> x

let set_default r v suff =
  match !r with
    None -> r := Some (v ^ suff)
  | _ -> ()

let compile_rv basename asmfile () =
  if not !Options.nostart then begin
    let out, _ =
      Format.sprintf
        "%s -nostdlib -nostartfiles -T %s/link.ld -o \"%s.exe\" \"%s\" %s/lib%d.s %s/mul%d.S 2>&1"
        !Archi.assembler Config.runtime_dir basename asmfile 
        Config.runtime_dir !Archi.nbits
        Config.runtime_dir !Archi.nbits
      |> process_output_to_list2 in
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
  dump (Some rvp) dump_riscv_prog ltl (fun file () -> error := compile_rv basename file ());
  match !error with
  | Some e -> Error ("RiscV generation error:\n" ^e)
  | None ->
    let l = cmd_to_list (Format.sprintf "%s%d-static \"%s.exe\" %s" Config.qemu_path !Archi.nbits basename
                           (params |> List.map string_of_int |> String.concat " " )) in
    try
      let all_but_last = l |> List.rev |> List.tl |> List.rev in
      all_but_last |> print_list (fun oc -> Format.fprintf oc "%s") "" "\n" "" oc;
      let ret = l |> List.last |> int_of_string in
      OK (Some ret)
    with _ -> OK None

let call_dot report_sectid report_secttitle file () : unit =
  if not !Options.no_dot
  then begin
    let r = Sys.command (Format.sprintf "dot -Tsvg %s -o %s.svg" file file) in
    add_to_report report_sectid report_secttitle (Img (Filename.basename file^".svg"));
    ignore r
  end

let _ =
  Arg.parse speclist (fun s -> ()) "Usage";
  init_archi !archi ();
  match !input_file with
  | None -> failwith "No input file specified.\n"
  | Some input ->
    add_to_report "Source" "Source" (Code (file_contents input));

    match Filename.chop_suffix_opt ".e" input with
      None -> failwith
                (Format.sprintf "File (%s) should end in .e" input)
    | Some basename ->
      begin
        params := List.rev !params;
        set_default riscv_dump basename ".s";
        if not !no_dump then begin
          set_default show_tokens basename ".lex";
          set_default ast_tree basename ".ast";
          set_default e_dump basename ".e.dump";
          set_default cfg_dump basename ".cfg";
          set_default rtl_dump basename ".rtl";
          set_default linear_dump basename ".linear";
          set_default ltl_dump basename ".ltl";
        end;

        tokenize input >>* (fun msg ->
            record_compile_result ~error:(Some msg) "Lexing";
          ) $ fun tokens ->
            record_compile_result "Lexing";
            dump !show_tokens (fun oc tokens ->
            List.iter (fun (tok,_) ->
                Format.fprintf oc "%s\n" (string_of_symbol tok)
                  ) tokens) tokens (fun f () -> add_to_report "lexer" "Lexer" (Code (file_contents f)));
        parse tokens () >>* (fun msg ->
            record_compile_result ~error:(Some msg) "Parsing";
          ) $ fun (ast, tokens) ->
            record_compile_result "Parsing";
            dump !ast_tree draw_ast_tree ast (call_dot "ast" "AST");
            if !ast_dump then Format.printf "%s\n" (string_of_ast ast) else ();

            match make_eprog_of_ast ast with
            | Error msg -> record_compile_result ~error:(Some msg) "Elang"
            | OK  ep ->
            dump !e_dump dump_e ep (fun file () ->
                add_to_report "e" "E" (Code (file_contents file)));
            run "Elang" !e_run eval_eprog ep;

            cfg_prog_of_eprog ep >>! fun cfg ->
            dump !cfg_dump dump_cfg_prog cfg (call_dot "cfg" "CFG");
            run "CFG" !cfg_run eval_cfgprog cfg;

            let cfg = constant_propagation cfg in
            record_compile_result "ConstProp";
            dump (!cfg_dump >*> fun s -> s ^ "0") dump_cfg_prog cfg
              (call_dot "cfg-after-cstprop" "CFG after Constant Propagation");
            run "CFG after constant_propagation" !cfg_run_after_cp eval_cfgprog cfg;

            let cfg = dead_assign_elimination cfg in
            record_compile_result "DeadAssign";
            dump (!cfg_dump >*> fun s -> s ^ "1") dump_cfg_prog cfg
              (call_dot "cfg-after-dae" "CFG after DAE");
            run "CFG after dead_assign_elimination" !cfg_run_after_dae eval_cfgprog cfg;

            let cfg = nop_elimination cfg in
            record_compile_result "NopElim";
            dump (!cfg_dump >*> fun s -> s ^ "2") dump_cfg_prog cfg
              (call_dot "cfg-after-nop" "CFG after NOP elim");
            run "CFG after nop_elimination" !cfg_run_after_ne eval_cfgprog cfg;

            let rtl = rtl_of_cfg cfg in
            dump !rtl_dump dump_rtl_prog rtl
              (fun file () -> add_to_report "rtl" "RTL" (Code (file_contents file)));
            run "RTL" !rtl_run exec_rtl_prog rtl;

            let linear = linear_of_rtl rtl in
            let lives = liveness_linear_prog linear in
            dump !linear_dump (fun oc -> dump_linear_prog oc (Some lives)) linear
              (fun file () -> add_to_report "linear" "Linear" (Code (file_contents file)));
            run "Linear" !linear_run exec_linear_prog linear;

            let linear = dse_prog linear lives in
            record_compile_result "DSE";
            dump (!linear_dump >*> fun s -> s ^ "1")
              (fun oc -> dump_linear_prog oc (Some lives)) linear
              (fun file () -> add_to_report "linear-after-dse" "Linear after DSE"
                  (Code (file_contents file)));
            run "Linear after DSE" !linear_run_after_dse exec_linear_prog linear;

            let ltl = ltl_prog_of_linear linear () in
            dump !ltl_dump dump_ltl_prog ltl
              (fun file () -> add_to_report "ltl" "LTL" (Code (file_contents file)));
            run "LTL" !ltl_run (exec_ltl_prog) ltl;
            (if !ltl_debug then debug_ltl_prog input ltl !heapsize !params);

            dump !riscv_dump dump_riscv_prog ltl (fun file () -> ignore (compile_rv basename file ()));
            if not !Options.nostart then begin
              run "Risc-V" !riscv_run (exec_rv_prog ltl basename) !riscv_dump
            end;

      end;


      if !output_json
      then begin
        let open Yojson in
        let jstring_of_ostring o =
          match o with
          | None -> `Null
          | Some s -> `String s
        in
        let j = `List (List.map (function
            | RunRes { step; retval; output; error; } ->
              `Assoc [("runstep",`String step);
                      ("retval", match retval with Some r -> `Int r | None -> `Null);
                      ("output", `String output);
                      ("error", jstring_of_ostring error);
                     ]
            | CompRes { step; error; data } ->
              `Assoc [("compstep",`String step);
                      ("error", jstring_of_ostring error);
                      ("data", data)
                     ]
          ) !results) in
        Format.printf "%s\n" (Yojson.pretty_to_string j);
      end;
      make_report input report ()
