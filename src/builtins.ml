open Batteries
open BatList
open Utils

let dump_int oc i =
  Format.fprintf oc "%d, " i

let dump_list pp oc l =
  List.iter (pp oc) l

let rec read_chars_rec mem addr (chars_read) =
  Mem.read_char mem addr  >>= fun (v) ->
  if v = 0 then
    (OK (rev chars_read))
  else read_chars_rec mem (addr + 1) (v::chars_read)

let read_string mem addr =
  read_chars_rec mem addr ([]) >>= fun (s) ->
  let s = s |> List.map char_of_int |> String.of_list in
  OK (s)

let dump_mem oc mem arg =
    Format.fprintf oc "Address accessed : %d\n" arg;
    List.iter (fun i ->
      Mem.read_char mem (arg + i) >>! fun (v) ->
      Format.fprintf oc "%d: %c (%d)" (arg + i) (char_of_int v) v;
      if (i+1) mod 8 == 0 then Format.fprintf oc "\n" else Format.fprintf oc "\t";
    ) (list_ints_inc 200);
  Format.fprintf oc "MEM :-)\n"; OK (None, [])


let rec do_builtin oc mem fname vargs =
  match fname, vargs with
  | "print", arg::_ ->
    Format.fprintf oc "%d\n" arg;
    OK(None)
  | "print_int", arg::_ ->
    Format.fprintf oc "%d" arg;
    OK(None)
  | "print_char", arg::_ ->
    Format.fprintf oc "%c" (char_of_int arg);
    OK(None)
  | "print_string", arg::_ ->
    read_string mem arg >>= fun s ->
    Format.fprintf oc "%s" s;
    OK (None)
  | "atoi", arg :: _ ->
    read_string mem arg >>= fun (s) ->
    begin
      let i = try int_of_string s with _ -> -1 in
      OK (Some i)
    end
  | "dump_mem", arg :: _ ->
    Format.fprintf oc "Address accessed : %d\n" arg;
    List.iter (fun i ->
      Mem.read_char mem (arg + i) >>! fun v ->
      Format.fprintf oc "%d: %c (%d)" (arg + i) (char_of_int v) v;
      if (i+1) mod 8 == 0 then Format.fprintf oc "\n" else Format.fprintf oc "\t";
      ) (list_ints_inc 200);
    Format.fprintf oc "MEM :-)\n"; OK (None)
  | "__muldi3", arg1 :: arg2 :: _ ->
    OK(Some (arg1*arg2))
  | "__udivdi3", arg1 :: arg2 :: _ ->
    OK(Some (arg1/arg2))
  | "__umoddi3", arg1 :: arg2 :: _ ->
    OK(Some (arg1 mod arg2))
  | "error", _ ->
    do_builtin oc mem "print_string" vargs >>= fun _ ->
    Error (Format.sprintf "exited")
  | _ -> Error (Format.sprintf "Unknown function %s\n" fname)
