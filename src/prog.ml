open Batteries
open Utils

type typ = 
  | Tint 
  | Tchar 
  | Tvoid
  | Tptr of typ
  | Tstruct of string

let rec string_of_typ = function 
  | Tint -> "int"
  | Tchar -> "char"
  | Tvoid -> "void"
  | Tptr ty -> (string_of_typ ty) ^ "*"
  | Tstruct s -> "struct " ^ s

let rec size_of_type (struct_defs: (string, (string * typ) list) Hashtbl.t) = function 
  | Tint -> OK 8
  | Tchar -> OK 1
  | Tptr _ -> OK 8
  | Tvoid -> Error "Void does not have a size"
  | Tstruct s -> 
      match Hashtbl.find_option struct_defs s with
        | Some var_typ_list -> 
            List.fold_left (
              fun a (var_i, typ_i) -> 
                a >>= fun a ->
                size_of_type struct_defs typ_i >>= fun sz_i ->
                OK (a+sz_i)
              ) (OK 0) var_typ_list

        | None -> Error ("@prog.size_of_type : Couldn't find struct " ^ s ^ " in struct_defs")
  
let field_offset (struct_defs: (string, (string * typ) list) Hashtbl.t) (s: string) (f: string): int res =
  match Hashtbl.find_option struct_defs s with 
    | Some var_typ_list -> (
        List.fold_left (
          fun a (var_i, typ_i) -> 
            a >>= fun (offs, pursue) ->
            if pursue && var_i <> f then
              size_of_type struct_defs typ_i >>= fun sz_i ->
              OK (offs+sz_i, pursue)
            else 
              OK (offs, false)
          ) (OK (0,true)) var_typ_list >>= fun (ans, pursue) -> 
        match pursue with 
          | false -> OK ans
          | true -> Error ("No such field '" ^ f ^ "' in struct '"^ s ^ "'")
    )
    | None -> Error ("@prog.size_of_type : Couldn't find struct " ^ s ^ " in struct_defs")

let field_type (struct_defs: (string, (string * typ) list) Hashtbl.t) (s: string) (f: string): typ res =
  match Hashtbl.find_option struct_defs s with 
    | Some var_typ_list -> (
        List.fold_left (
          fun a (var_i, typ_i) -> 
            a >>= fun (typ_ans, pursue) ->
            if var_i = f then
              OK (typ_i, false)
            else
              OK (typ_ans, pursue)
          ) (OK (Tvoid, true)) var_typ_list >>= fun (ans, pursue) -> 
        match pursue with 
          | false -> OK ans
          | true -> Error ("No such field '" ^ f ^ "' in struct '"^ s ^ "'")
    )
    | None -> Error ("@prog.size_of_type : Couldn't find struct " ^ s ^ " in struct_defs")

    
type mem_access_size =
  | MAS1
  | MAS4
  | MAS8

let string_of_mem_access_size mas =
  match mas with
  | MAS1 -> "{1}"
  | MAS4 -> "{4}"
  | MAS8 -> "{8}"

let mas_of_size n =
  match n with
  | 1 -> OK MAS1
  | 4 -> OK MAS4
  | 8 -> OK MAS8
  | _ -> Error (Printf.sprintf "Unknown memory access size for size = %d" n)


let size_of_mas mas =
  match mas with
  | MAS1 -> 1
  | MAS4 -> 4
  | MAS8 -> 8

let archi_mas () =
  match !Archi.archi with
  | A64 -> MAS8
  | A32 -> MAS4


type 'a gdef = Gfun of 'a

type 'a prog = (string * 'a gdef) list


let dump_gdef dump_fun oc gd =
  match gd with
  | (fname, Gfun f) ->
    dump_fun oc fname f;
    Format.fprintf oc "\n"

let dump_prog dump_fun oc =
  List.iter (dump_gdef dump_fun oc)

type 'a state = {
  env: (string, 'a) Hashtbl.t;
  mem: Mem.t
}

let init_state memsize =
  {
    mem = Mem.init memsize;
    env = Hashtbl.create 17;
  }

let set_val env v i =
  Hashtbl.replace env v i

let get_val env v =
  Hashtbl.find_option env v

let find_function (ep: 'a prog) fname : 'a res =
  let f_fold ans (k, Gfun v) = 
    if k = fname 
      then OK v 
    else 
      ans
  in
  List.fold f_fold (Error (Format.sprintf "Unknown function %s\n" fname)) ep
  
(*
  match List.assoc_opt fname ep with
  | Some (Gfun f) -> OK f
  | _ -> Error (Format.sprintf "Unknown function %s\n" fname)
*)
  
