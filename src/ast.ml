open Batteries
open BatPrintf

type tag = Tassign | Tif | Twhile | Tblock | Treturn | Tprint
         | Tint
         | Tadd | Tmul | Tdiv | Tmod | Txor | Tsub
         | Tclt | Tcgt | Tcle | Tcge | Tceq | Tne
         | Tneg
         | Tlistglobdef
         | Tfundef | Tfunname | Tfunargs | Tfunbody
         | Tassignvar
         | Targ | Targs

type tree = | Node of tag * tree list
            | StringLeaf of string
            | IntLeaf of int
            | NullLeaf
            | CharLeaf of char

let string_of_stringleaf = function
  | StringLeaf s -> s
  | _ -> failwith "string_of_stringleaf called on non-stringleaf nodes."

type astfun = (string list * tree)
type ast = (string * astfun) list

let string_of_tag = function
  | Tassign -> "Tassign"
  | Tif -> "Tif"
  | Twhile -> "Twhile"
  | Tblock -> "Tblock"
  | Treturn -> "Treturn"
  | Tprint -> "Tprint"
  | Tint -> "Tint"
  | Tadd -> "Tadd"
  | Tmul -> "Tmul"
  | Tdiv -> "Tdiv"
  | Tmod -> "Tmod"
  | Txor -> "Txor"
  | Tsub -> "Tsub"
  | Tclt -> "Tclt"
  | Tcgt -> "Tcgt"
  | Tcle -> "Tcle"
  | Tcge -> "Tcge"
  | Tceq -> "Tceq"
  | Tne -> "Tne"
  | Tneg -> "Tneg"
  | Tlistglobdef -> "Tlistglobdef"
  | Tfundef -> "Tfundef"
  | Tfunname -> "Tfunname"
  | Tfunargs -> "Tfunargs"
  | Tfunbody -> "Tfunbody"
  | Tassignvar -> "Tassignvar"
  | Targ -> "Targ"
  | Targs -> "Targs"

(* return (node, nextnode, dotcode) *)
let rec draw_ast a next =
  match a with
  | Node (t, l) ->

    let (code, nodes, next) =
      List.fold_left (fun (code, nodes, nextnode) n ->
          let (node, next, ncode) = draw_ast n nextnode in
          (code @ ncode, node::nodes, next)
        ) ([], [], next)
        l in
    (next, next+1, code @ [
         Format.sprintf "n%d [label=\"%s\"]\n" next (string_of_tag t)
       ] @ List.map (fun n ->
        Format.sprintf "n%d -> n%d\n" next n
      )nodes)

  | StringLeaf s ->
    (next, next+1, [         Format.sprintf "n%d [label=\"%s\"]\n" next s])
  | IntLeaf i ->
    (next, next+1, [         Format.sprintf "n%d [label=\"%d\"]\n" next i])
  | NullLeaf ->
    (next, next+1, [         Format.sprintf "n%d [label=\"null\"]\n" next])
  | CharLeaf i ->
    (next, next+1, [         Format.sprintf "n%d [label=\"%c\"]\n" next i])

let draw_ast_tree oc ast =
  let (_, _, s) = draw_ast ast 1 in
  let s = String.concat "" s in
  Format.fprintf oc "digraph G{\n%s\n}\n" s

let rec string_of_ast a =
  match a with
  | Node (t, l) ->
    Format.sprintf "Node(%s,%s)" (string_of_tag t)
      (String.concat ", " (List.map string_of_ast l))
  | StringLeaf s -> Format.sprintf "\"%s\"" s
  | IntLeaf i -> Format.sprintf "%d" i
  | CharLeaf i -> Format.sprintf "%c" i
  | NullLeaf -> "null"
