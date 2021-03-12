open Batteries
open Cfg
open Prog
open Utils

(* Analyse de vivacité *)


(* [vars_in_expr e] renvoie l'ensemble des variables qui apparaissent dans [e]. *)
let rec vars_in_expr (e: expr) =
   match e with
      | Evar s -> Set.singleton s
      | Ebinop (_, e1, e2) -> Set.union (vars_in_expr e1) (vars_in_expr e2)
      | Eunop (_, e1) -> vars_in_expr e1
      | Eint i -> Set.empty

(* [live_cfg_node node live_after] renvoie l'ensemble des variables vivantes
   avant un nœud [node], étant donné l'ensemble [live_after] des variables
   vivantes après ce nœud. *)
let live_cfg_node (node: cfg_node) (live_after: string Set.t) = 
   match node with
      | Cprint (e, i) -> Set.union (vars_in_expr e) live_after
      | Creturn e -> Set.union (vars_in_expr e) live_after
      | Ccmp (e, i1, i2) -> Set.union (vars_in_expr e) live_after
      | Cassign (var, e, i) -> Set.union (vars_in_expr e) (Set.remove var live_after)
      | Cnop i -> live_after
   

(* [live_after_node cfg n] renvoie l'ensemble des variables vivantes après le
   nœud [n] dans un CFG [cfg]. [lives] est l'état courant de l'analyse,
   c'est-à-dire une table dont les clés sont des identifiants de nœuds du CFG et
   les valeurs sont les ensembles de variables vivantes avant chaque nœud. *)
let live_after_node cfg n (lives: (int, string Set.t) Hashtbl.t) : string Set.t =
   let next_nodes = succs cfg n in
      Set.fold (fun bi a -> Set.union a (Hashtbl.find lives bi)) next_nodes Set.empty

(* [live_cfg_nodes cfg lives] effectue une itération du calcul de point fixe.

   Cette fonction met à jour l'état de l'analyse [lives] et renvoie un booléen
   qui indique si le calcul a progressé durant cette itération (i.e. s'il existe
   au moins un nœud n pour lequel l'ensemble des variables vivantes avant ce
   nœud a changé). 
   vrai s'il n'ya pas eu de changement 
   faux sinon 
   *) 
let live_cfg_nodes cfg (lives : (int, string Set.t) Hashtbl.t) =
   let f_fold nodei cfg_nodei a = 
      let live_in = Hashtbl.find lives nodei in 
      let new_live_in = live_cfg_node cfg_nodei (live_after_node cfg nodei lives) in 
      Hashtbl.replace lives nodei new_live_in;  
      if ( Set.equal new_live_in live_in) then 
         a  
      else 
         false  
   in 
   Hashtbl.fold f_fold cfg true 

(* [live_cfg_fun f] calcule l'ensemble des variables vivantes avant chaque nœud
   du CFG en itérant [live_cfg_nodes] jusqu'à ce qu'un point fixe soit atteint.
   *)
let live_cfg_fun (f: cfg_fun) : (int, string Set.t) Hashtbl.t =
   let lives = Hashtbl.create 17 in
   Hashtbl.iter (fun a b -> Hashtbl.replace lives a Set.empty) f.cfgfunbody;
   let rec looper tbl = 
      if (live_cfg_nodes f.cfgfunbody tbl) 
         then tbl
      else
         looper tbl 
   in
   looper lives 

