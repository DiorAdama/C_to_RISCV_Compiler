open Batteries
open BatList
open Batteries
open BatList
open BatBuffer
open Prog
open Elang
open Cfg
open Elang_run
open Cfg_run
open Rtl
open Rtl_run
open Linear
open Ltl
open Ltl_print
open Utils
open Builtins
open Lwt
open Yojson
open Ltl_run



type dbg_cmd =
  | Next
  | NextBp
  | Break of int
  | RmBreak of int
  | Init of int * int list
  | Quit

let get_typed_attribute json attr toX =
  let open Yojson.Basic.Util in
  try
    OK (json |> member attr |> toX)
  with e -> Error (
      Yojson.Basic.to_string json |> fun s ->
      Format.sprintf "In get_typed_attribute\n json=%s\nattr=%s\n%s"
        s
        attr
        (Printexc.to_string e))

let parse_breakpoint j =
  let open Yojson.Basic.Util in
  let open Utils in
  get_typed_attribute j "ip" to_int >>= fun ip ->
  OK ip

let parse_init j =
  let open Yojson.Basic.Util in
  let open Utils in
  get_typed_attribute j "memsize" to_int >>= fun memsize ->
  get_typed_attribute j "params" (fun x -> x |> to_list |> filter_int) >>= fun params ->
  OK (Init (memsize, params))

let parse_dbg_cmd j =
  let open Yojson.Basic.Util in
  let open Utils in
  get_typed_attribute j "cmd" to_string >>= fun cmd ->
  begin match cmd with
    | "next" -> OK Next
    | "next_bp" -> OK NextBp
    | "break" -> parse_breakpoint j >>= fun ip -> OK (Break ip)
    | "rmbreak" -> parse_breakpoint j >>= fun ip -> OK (RmBreak ip)
    | "init" -> parse_init j
    | "quit" -> OK Quit
    | _ -> Error (Format.sprintf "Unknown command '%s'" cmd)
  end

let rec json_summary j =
  match j with
    `Assoc l ->
    let ks = ["regs"; "memwrite"; "memread"; "start"; "end"; "params"] in
    let l = List.filter_map (fun (k,v) -> if List.mem k ks then None
                              else if k = "code" then Some (k, `List [])
                              else Some (k, json_summary v)) l in
    `Assoc l
  | `List l -> `List (List.map json_summary l)
  | _ -> j
let trace_regs st =
  Hashtbl.fold (fun r v acc ->
      (string_of_reg r, `Int v) :: acc
    ) st.regs []
  |> fun l -> `Assoc l

let make_trace ip (st: ltl_state) out () =
  let m = Mem.write_log st.mem () in
  let mread = Mem.read_log st.mem () in
  let out = if out = "" then [] else [("output", `String (String.escaped out))] in
  `Assoc ([
      ("step", `Int !(st.numstep));
      ("ip", `Int ip);
      ("regs", trace_regs st);
      ("memwrite", `Assoc (List.map (fun (addr,v) -> (string_of_int addr, `Int v)) m));
      ("memread", `List (List.map (fun addr -> `Int addr) mread))
    ] @ out)

let step_until state st send_json cond =
  let rec aux () =
    match !state with
    | Some  ip ->
      begin match exec_ltl_instr Format.str_formatter ip st with
          OK r ->
          st.numstep := !(st.numstep) + 1;
          state := r;
          begin match r with
              Some ip ->
              let o = Format.flush_str_formatter () in
              make_trace ip st o () |> send_json >>= fun _ ->
              if cond ip
              then `Assoc [("currentstep", `Int !(st.numstep))] |> send_json
              else aux ()
            | None ->
              `Assoc [("currentstep", `Int (!(st.numstep) - 1))] |> send_json >>= fun _ ->
              `Assoc [("finished", `Bool true)] |> send_json
          end
        | Error msg ->
          let s = String.escaped (string_of_error msg) in
          `Assoc [("error", `String s)] |>
          send_json
      end
    | None -> return ()
  in aux ()

open Websocket
open Websocket_lwt_unix

let json_loc (l: Regalloc.loc) : Yojson.t =
  match l with
  | Regalloc.Reg r -> `Assoc [("reg", `String(string_of_reg r))]
  | Regalloc.Stk o -> `Assoc [("stk", `Int(o))]

let debugger_message progname breaks state st prog rstop client : unit Lwt.t =
  let open Frame in
  let send msg = Connected_client.send client (Frame.create ~opcode:Opcode.Text ~content:msg ())in
  let send_json j =
    Logs_lwt.info
      (fun m -> m "Sending %s\n" (json_summary j |> to_string)) >>= fun _ ->
    let s = to_string j ^ "@" in send s
  in
  let rec loop () =
    let react fr =
      Lwt_log.debug_f "<- %s" (Frame.show fr) >>= fun () ->
      match fr.opcode with
      | Opcode.Text ->
        begin
          let msg = fr.content in
          Logs.info (fun m -> m "received command '%s'\n" msg);
          let open Yojson.Basic in
          let j = from_string msg in
          match parse_dbg_cmd j with
          | OK Next ->
            step_until state !st send_json (fun ip -> true)
          | OK NextBp ->
            step_until state !st send_json
              (fun ip -> List.mem ip !breaks)
          | OK (Break ip) ->
            breaks := ip :: !breaks; Lwt.return_unit
          | OK (RmBreak ip) ->
            breaks := List.remove_all !breaks ip; Lwt.return_unit
          | OK (Init (memsize, params)) ->
            st := init_state memsize prog params;
            breaks := [];
            begin match Hashtbl.find_option !st.funs "main" with
              | Some floc ->
                let ip = floc.funstart in
                state := Some ip;
                Hashtbl.fold (fun fname { funstart; funend; funinfo; funregalloc}
                               (* (fstart,fend,var2reg, alloc) *) acc ->
                    (`Assoc [("fname",`String fname);
                             ("start",`Int funstart);
                             ("end", `Int funend);
                             ("vars", `Assoc (List.filter_map (fun (k,v) ->
                                  match List.assoc_opt v funregalloc with
                                  | None -> None
                                  | Some l -> Some (k,json_loc l)
                                ) funinfo))
                            ]) :: acc
                  ) !st.funs [] |> fun funboundaries ->
                Hashtbl.fold (fun ip ins acc ->
                    (Format.fprintf Format.str_formatter "%a" dump_ltl_instr ins);
                    (string_of_int ip, `String (Format.flush_str_formatter ())) :: acc
                  ) !st.code [] |> fun code ->
                `List ( [`Assoc [("progname", `String progname);
                                ("params", `List (List.map (fun x -> `Int x) params))];
                         `Assoc [("funboundaries", `List funboundaries)];
                         `Assoc [("code", `Assoc code)];
                         make_trace ip !st "" ();
                         `Assoc [("currentstep", `Int !(!st.numstep))]])
                |> send_json
              | None -> fail (failwith (Format.sprintf "Could not find 'main' entry point..."))
            end
          | OK Quit ->
            Lwt.wakeup rstop (); Lwt.return_unit
          | Error errmsg ->
            `Assoc [("error", `String (Format.sprintf "Error while parsing %s" msg));
                   ("errorMsg", `String (string_of_error errmsg))
                  ] |>
            send_json
        end
      | _ -> Lwt.return_unit
    in
    Websocket_lwt_unix.Connected_client.recv client >>= react >>= loop
  in
  Lwt.catch
    loop
    (fun exn ->
       Lwt_log.info_f  "Connection to client lost" >>= fun () ->
       Lwt.fail exn)

let debug_ltl_prog progname lp memsize params : unit=
  let () = Logs.set_reporter (Logs.format_reporter ()) in
  let () = Logs.set_level (Some Logs.Info) in
  let st = ref (init_state memsize lp params) in
  let state = ref (None) in
  let breaks = ref [] in
  let ctx = Conduit_lwt_unix.default_ctx in
  let uri = "http://localhost:8080" in
  let (pstop, rstop) = Lwt.task () in
  let server () =
    Resolver_lwt.resolve_uri (Uri.of_string uri) Resolver_lwt_unix.system >>= fun endp ->
    Conduit_lwt_unix.endp_to_server ~ctx endp >>= fun server ->
    Websocket_lwt_unix.establish_server
      ~ctx ~mode:server
      ~check_request:(fun req -> true)
      ~on_exn:(fun exn ->
          match exn with
          End_of_file -> Logs.info (fun m -> m "Client disconnected\n")
          | _ -> Logs.err (fun m -> m "Received exception : %s" (Printexc.to_string exn))
        )
      (debugger_message progname breaks state st lp rstop) in
  Lwt_main.run (Lwt.pick [server () ; pstop])
