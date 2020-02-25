open Batteries
open List_utils
open List
open Grammar


let iter_until_fixpoint (f : unit -> bool) name =
  let rec aux i =
    if f ()
    then aux (i+1)
    else Printf.printf "Finished %s after %d steps.\n" name i in
  aux 1

let id x = x

let apply_on_all f l = List.exists id (List.map f l)

let nullt : (string, bool) Hashtbl.t = Hashtbl.create 20
let firstt : (string, string Set.t) Hashtbl.t = Hashtbl.create 20
let followt : (string, string Set.t) Hashtbl.t = Hashtbl.create 20

let nullable k = hashget_def nullt k false
let first n = hashget_def firstt n Set.empty
let follow n = hashget_def followt n Set.empty

let null_nt (toks,nts,rules) (n: string)  : bool =
  if hashget_def nullt n false then false
  else
    begin
      let r = List.filter (fun {rule_nt = x; _} -> x = n) rules in
      let r = List.map (fun {rule_prods = x} -> x) r in
      let can_be_null = List.exists (List.for_all nullable) r in
      Hashtbl.replace nullt n can_be_null; can_be_null
    end

let null_all_nt (toks,nts,rules) () : bool = apply_on_all (null_nt (toks,nts,rules)) nts

let iter_nullnt (toks,nts,rules) () = iter_until_fixpoint (null_all_nt (toks,nts,rules)) "nullnt"

let rec first_prod (toks,nts,rules) (p: string list) : string Set.t =
  match p with
  | [] -> Set.empty
  | s::r ->
    if List.mem s toks then Set.singleton s
    else begin Set.union (first s) (if nullable s then first_prod (toks,nts,rules) r else Set.empty) end

let first_nt (toks,nts,rules) (n: string) : bool =
  let old = first n in
  let r = List.filter (fun {rule_nt = x} -> x = n) rules in
  let r = List.map (fun {rule_prods = x} -> x) r in
  let f = List.fold_left Set.union Set.empty (List.map (first_prod (toks,nts,rules)) r) in
  Hashtbl.replace firstt n f; not (Set.equal old f)

let iter_first (toks,nts,rules) () =
  iter_until_fixpoint (fun () -> apply_on_all (first_nt (toks,nts,rules)) nts) "firstt"

let rec cut_prod (x: string) (l: string list) : string list =
  match l with
    [] -> []
  | a::r -> if a = x then r else cut_prod x r

let null_prodlist (toks,nts,rules)(pl: string list) =
  List.for_all (fun p ->
      if List.mem p toks then false
      else nullable p) pl

let follow_nt (toks,nts,rules) (n: string) : bool =
  let old = follow n in
  let l = List.filter (fun { rule_prods = x} -> List.mem n x) rules in
  let l = List.map (fun {rule_nt = fx; rule_prods = sx} -> (fx, cut_prod n sx)) l in
  let l = List.map (fun x -> Set.union (if null_prodlist (toks,nts,rules) (snd x)
                              then follow (fst x)
                              else Set.empty) (first_prod (toks,nts,rules) (snd x))) l in
  let l = List.fold_left Set.union Set.empty l in
  Hashtbl.add followt n l; old <> l

let follow_all_nt (toks,nts,rules) () = apply_on_all (follow_nt (toks,nts,rules)) nts

let iter_follownt (toks,nts,rules) () =
  iter_until_fixpoint (follow_all_nt (toks,nts,rules)) "follownt"

type lltype = First of int | Follow of int

let lltable : (string * string, lltype list) Hashtbl.t = Hashtbl.create 20

let add_into_table x t p =
  Hashtbl.add lltable (x,t) (cleardup (hashget_def lltable (x,t) [] @ [p]))


let fill_lltable (toks,nts,rules) () =
  List.iteri (fun i {rule_nt = x; rule_prods = gamma} ->
      List.iter (fun t -> add_into_table x t (First (i+1))) (Set.to_list (first_prod (toks,nts,rules) gamma));
      if null_prodlist (toks,nts,rules) gamma
      then
        List.iter (fun t -> add_into_table x t (Follow (i+1))) (Set.to_list (follow x))
    ) rules

let string_of_lltype = function
  | First i -> Printf.sprintf "<a style='color:blue;' href=\"#rule-%d\">%d</a>" i i
  | Follow i -> Printf.sprintf "<a style='color:red;' href=\"#rule-%d\">%d</a>" i i


let print_table (toks,nts,rules) oc () =
  Format.fprintf oc "<!DOCTYPE html>" ;
  Format.fprintf oc "<html><head><link rel='stylesheet' type='text/css' href='style.css'/></head>\n";
  Format.fprintf oc "<body><table><tr><th></th>" ;
  List.iter (fun t ->
      Format.fprintf oc " <th class='bigth'><div class='verticalText'>%s</div></th> " t
    ) toks;
  Format.fprintf oc "</tr>\n";
  List.iteri
    (fun i x ->
       Format.fprintf oc "<tr><td style='text-align:center;'>%s</td> " x;
       List.iter (fun t ->
           let rs = (hashget_def lltable (x,t) []) in
           Format.fprintf oc " <td style='text-align:center;%s'>%s</td> "
             (if List.length rs > 1 then "background: rgba(255, 99, 71, 0.5);" else "")
             (print_seq string_of_lltype rs)
         ) toks;
    )
    nts;
  Format.fprintf oc "</table></body></html>\n"

let str_replace s s1 s2 =
  Str.global_replace (Str.regexp (Str.quote s1)) s2 s


let print_grammar (toks,nts,rules) oc () =
  let counter = ref 1 in
  Format.fprintf oc "<table>" ;
  List.iter (fun nt  ->
      let rules : (string list * string) list =
        filter_map (fun {rule_nt; rule_prods; rule_action} ->
            if rule_nt = nt
            then Some (rule_prods, match rule_action with Some a -> a | None -> "")
            else None) rules
      in
      let first_rule = ref true in
      List.iter (fun (rule,act) ->
          Format.fprintf oc " <tr id='rule-%d' style='%s;'><td class='left'>(%d)</td> <td class='left'> %s </td class='left'> <td class='left'> -> </td> <td class='left'> %s</td><td class='left'><pre>%s</pre></td></tr>\n"
            !counter
            (if !first_rule then "border-top: solid 1px" else "border: none")
            !counter
            (if !first_rule then Format.sprintf "<span id='nt-%s'>%s</span>" nt nt else "")
            (if rule = [] then "&epsilon;" else print_seq (fun x -> if List.mem x toks then x else Format.sprintf "<a href='#nt-%s' style='color: black;'>%s</a>" x x) rule)
            act ;
          first_rule := false;
          counter:=!counter+1
        ) rules ;
      Format.fprintf oc "\n"

  ) nts;
  Format.fprintf oc "</table>\n"


let print_null (toks,nts,rules) oc () =
  Format.fprintf oc "<br><table><tr><th>Non-terminal</th><th>Nullable</th></tr>\n" ;
  List.iteri (fun i x  ->
      Format.fprintf oc "<tr><td> %s</td><td>  %s</td></tr> \n"
        x
        (if nullable x then "true" else "false")
    ) nts;
  Format.fprintf oc "</table>\n<br>"


let print_first (toks,nts,rules) oc () =
  Format.fprintf oc "<br><table><tr><th>Non-terminal</th><th>First</th></tr>\n" ;
  List.iteri (fun i x  ->
      Format.fprintf oc " <tr><td>%s</td><td>%s</td></tr> \n"
        x
        (print_seq (fun y -> y) (Set.to_list (first x)))
    ) nts;
  Format.fprintf oc "</table>\n"


let print_follow (toks,nts,rules) oc () =
  Format.fprintf oc "<table><tr><th> Non-terminal</th><th> Follow</th></tr>\n" ;
  List.iteri (fun i x  ->
      Format.fprintf oc "<tr><td> %s </td><td> %s</td></tr> \n"
        x
        (print_seq (fun y -> y) (Set.to_list (follow x)))
    ) nts;
  Format.fprintf oc "</table>\n"




let print_html (toks,nts,rules) oc () =
  Format.fprintf oc "<!DOCTYPE html>" ;
  Format.fprintf oc "<html><head>\
 <style type=\"text/css\">\
 .bigth\
 {\
     text-align: center;\
     vertical-align: bottom;\
     height: 250px;\
     padding-bottom: 3px;\
     padding-left: 5px;\
     padding-right: 5px;\
 }\
 table {\
     display: inline-table;\
     margin: 1em;\
     border: solid 1px;\
     border-width: 1px;\
     border-collapse: collapse;\
 }\
 table th, table td {\
     border: solid 1px;\
     border-collapse: collapse;\
 }\
 .verticalText\
 {\
     text-align: center;\
     vertical-align: middle;\
     width: 20px;\
     margin: 0px;\
     padding: 0px;\
     padding-left: 3px;\
     padding-right: 3px;\
     padding-top: 10px;\
     white-space: nowrap;\
     -webkit-transform: rotate(-90deg);\
     -moz-transform: rotate(-90deg);\
 }\
 .left{\
     text-align: left;\
     border:none;\
 }\
     </style>\
 </head>\n";
  Format.fprintf oc "<body>\n" ;
  Format.fprintf oc "<h1>Grammaire</h1>\n";
  print_grammar (toks,nts,rules) oc ();
  Format.fprintf oc "<h1>Table Null</h1>\n";
  print_null (toks,nts,rules) oc ();
  Format.fprintf oc "<h1>Table First</h1>\n";
  print_first (toks,nts,rules) oc ();
  Format.fprintf oc "<h1>Table Follow</h1>\n";
  print_follow (toks,nts,rules) oc ();
  Format.fprintf oc "<h1>Table LL</h1>\n";
  print_table (toks, nts, rules) oc ();
  Format.fprintf oc "</body>\n</html>\n"


