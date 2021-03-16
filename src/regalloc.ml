open Batteries
open BatList
open BatEnum
open Prog
open Linear
open Rtl
open Linear_liveness
open Utils
open Report
open Options



(* Allocation de registres *)

(* Nous allons procéder à l'allocation de registres, par coloration de graphe
   d'interférences.

   Le but de l'allocateur est d'associer à chaque pseudo-registre utilisé dans
   une fonction Linear, un emplacement (type [loc]). *)

type loc = Reg of int | Stk of int

(* Un emplacement (location en anglais) est soit un registre machine (identifié
   par son numéro [r] entre 0 et 31 inclus) : [Reg r], soit un emplacement sur
   la pile [Stk o] signifiant un décalage de [o] octets par rapport au pointeur
   de trame présent dans le registre [s0] (aussi appelé [fp] pour frame
   pointer). *)

(* Nous vous fournissons, ci-dessous, une implémentation naïve qui évince tous
   les pseudo-registres sur la pile. *)

let regs_in_instr i =
  Set.union (gen_live i) (kill_live i)

let regs_in_instr_list (l: rtl_instr list) : reg Set.t =
  List.fold_left
    (fun acc i -> Set.union acc (regs_in_instr i))
    Set.empty l

let regalloc_on_stack_fun (f: linear_fun) : ((reg, loc) Hashtbl.t * int)=
  let allocation = Hashtbl.create 10 in
  let regs = regs_in_instr_list f.linearfunbody in
  let regs = Set.diff regs (Set.of_list f.linearfunargs) in
  let next_stack_slot =
    List.fold_left (fun next_stack_slot r ->
        Hashtbl.replace allocation r (Stk (next_stack_slot));
        next_stack_slot - 1
      ) (-1) (Set.to_list regs) in
  (allocation, next_stack_slot)


(* Nous allons maintenant construire un graphe d'interférence de registres
   (register interference graph, ou rig). Le type d'un rig est donné par le type
   OCaml [(reg, reg Set.t) Hashtbl.t], i.e. une table dont les clés sont des
   registres et les valeurs sont des ensembles de registres qui "interfèrent"
   avec le registre-clé. Cela correspond à la relation d'adjacence dans le
   graphe d'interférence. *)

(* La fonction [add_to_interf rig x y] ajoute [y] à la liste des registres qui
   interfèrent avec [x] dans le graphe [rig].

   On pourra utiliser la fonction [Hashtbl.modify_def] qui permet de modifier la
   valeur associée à une clé.

   Par exemple, l'appel [Hashtbl.modify_def def k f rig] modifie la valeur
   associée à la clé [k] dans le graphe [rig].

   [f] est une fonction qui prend en entrée l'ancienne valeur, et qui retourne
   la nouvelle valeur (type ['b -> 'b], si [rig] est de type [('a,'b)
   Hashtbl.t], i.e. ['b] est le type des valeurs).

   [def] est la valeur par défaut donnée à [f] s'il n'existe pas d'ancienne
   valeur pour la clé [k].

   Attention, les interférences doivent exister dans les deux sens, i.e. si [x]
   est dans la liste d'interférence de [y], alors [y] doit être dans la liste
   d'interférence de [x].

*)

let add_interf (rig : (reg, reg Set.t) Hashtbl.t) (x: reg) (y: reg) : unit =
    Hashtbl.replace rig x (Set.add y (Hashtbl.find rig x));
    Hashtbl.replace rig y (Set.add x (Hashtbl.find rig y))
    


(* [make_interf_live rig live] ajoute des arcs dans le graphe d'interférence
   pour chaque paire de registres vivants en même temps à un point de programme.
   *)
let make_interf_live
    (rig: (reg, reg Set.t) Hashtbl.t)
    (live : (int, reg Set.t) Hashtbl.t) : unit =
    
    let f_iter instr_id alive = 
      Set.iter (fun x ->(
        Set.iter (fun y ->
            if x=y then ()
            else add_interf rig x y
          ) alive)
        ) alive
    in 
    Hashtbl.iter f_iter live

(* [build_interference_graph live_out] construit, en utilisant les fonctions que
   vous avez écrites, le graphe d'interférence en fonction de la vivacité des
   variables à la sortie des nœuds donné par [live_out].

   Offert par la maison !
*)
let build_interference_graph (live_out : (int, reg Set.t) Hashtbl.t) code : (reg, reg Set.t) Hashtbl.t  =
  let interf = Hashtbl.create 17 in
  (* On ajoute un sommet pour chaque variable qui apparaît dans le programme. *)
  Hashtbl.iter (fun _ s ->
      Set.iter (fun v -> Hashtbl.replace interf v Set.empty) s
    ) live_out;
  make_interf_live interf live_out;
(* Les registres dans lesquels on écrit mais qui ne sont jamais vivants doivent être considérés comme en interférence avec tous les autres. *)
  let written_regs = written_rtl_regs code in
  let written_regs_never_live =
    Hashtbl.fold (fun _ regset_live_together acc -> Set.diff acc regset_live_together) live_out
      written_regs in
  let other_regs = Hashtbl.keys interf |> Set.of_enum in
  Set.iter (fun r ->
      Set.iter (fun r_other ->
          add_interf interf r r_other
        ) other_regs
    ) written_regs_never_live;
  interf

(* [remove_from_rig rig v] supprime le sommet [v] du graphe d'interférences
   [rig]. *)
let remove_from_rig (rig : (reg, reg Set.t) Hashtbl.t)  (v: reg) : unit =
  Hashtbl.remove rig v;
  let f_iter r interfer = 
    Hashtbl.replace rig r (Set.remove v interfer)
  in
  Hashtbl.iter f_iter rig;
   


(* Type représentant les différentes décisions qui peuvent être prises par
   l'allocateur de registres.

   - [Spill r] signifie que le pseudo-registre [r] sera évincé (spillé) sur la pile.
    
   - [NoSpill r] signifie que le pseudo-registre [r] sera alloué dans un vrai
   registre physique.
*)
type regalloc_decision =
    Spill of reg
  | NoSpill of reg

(* Rappel de l'algorithme d'empilement des registres *)

(* Une fois le graphe d'interférences construit, il nous faut parcourir ce
   graphe afin de le colorer, avec [n] couleurs. On construit une pile de
   [regalloc_decision].

   Tant que le graphe n'est pas vide:

   - choisir un sommet [s] avec strictement moins de [n] voisins (ce sera le
   travail de la fonction [pick_node_with_fewer_than_n_neighbors]), empiler la
   décision [NoSpill s] et retirer [s] du graphe.

   - si aucun tel sommet n'existe dans le graphe, choisir un sommet [s]
   correspondant à un registre que l'on évincera (ce sera le travail de la
   fonction [pick_spilling_candidate]). Empiler la décision [Spill s] et retirer
   [s] du graphe.

*)

(* [pick_node_with_fewer_than_n_neighbors rig n] choisit un nœud du graphe [rig]
   possédant strictement moins de [n] voisins. Retourne [None] si aucun sommet
   ne satisfait cette condition. *)
let pick_node_with_fewer_than_n_neighbors (rig : (reg, reg Set.t) Hashtbl.t) (n: int) : reg option =
  let f_fold r interfer ans= 
    match ans with 
      | Some re -> Some re
      | None -> 
          if (Set.cardinal interfer <= n)
            then Some r
          else
            None
  in
  Hashtbl.fold f_fold rig None

(* Lorsque la fonction précédente échoue (i.e. aucun sommet n'a moins de [n]
   voisins), on choisit un pseudo-registre à évincer.

   Une heuristique possible consiste à évincer le pseudo-registre qui a le plus
   de voisins dans le graphe [rig].

   [pick_spilling_candidate rig] retourne donc le pseudo-registre [r] qui a le
   plus de voisins dans [rig], ou [None] si [rig] est vide. *)
let pick_spilling_candidate (rig : (reg, reg Set.t) Hashtbl.t)  : reg option =
  let f_fold r interfer ans = 
    match ans with 
      | None -> Some r 
      | Some r_ans ->(
        if (Set.cardinal (Hashtbl.find rig r_ans) < Set.cardinal interfer)
          then Some r 
        else 
          Some r_ans
      ) 
  in
  if Hashtbl.is_empty rig  
    then None
  else
    Hashtbl.fold f_fold rig None



(* [make_stack rig stack ncolors] construit la pile, selon l'algorithme vu en
   cours (slides 60 à 63 du cours "Allocation de registres - Autres slides"
   présent sur Edunao.) *)
let rec make_stack (rig : (reg, reg Set.t) Hashtbl.t)  (stack : regalloc_decision list) (ncolors: int) : regalloc_decision list =
  
  match pick_node_with_fewer_than_n_neighbors rig ncolors with 
    | Some r -> 
        remove_from_rig rig r;
        make_stack rig ((NoSpill r)::stack) ncolors
    | None -> (
        match pick_spilling_candidate rig with 
          | None -> stack 
          | Some r_spill -> 
              remove_from_rig rig r_spill;
              make_stack rig ((Spill r_spill)::stack) ncolors
    )


(* Maintenant que nous avons une pile de [regalloc_decision], il est temps de
   colorer notre graphe, i.e. associer une couleur (un numéro de registre
   physique) à chaque pseudo-registre. Nous allons parcourir la pile et pour
   chaque décision :

   -  [Spill r] : associer un emplacement sur la pile au pseudo-registre [r]. On
   choisira l'emplacement [next_stack_slot].

   - [NoSpill r] : associer une couleur (un registre) physique au
   pseudo-registre [r]. On choisira une couleur qui n'est pas déjà associée à un
   voisin de [r] dans [rig].

   Cette fonction prend en entrée :

   - [allocation] : l'allocation courante, que l'on mettra à jour, et qui
   permettra de trouver les couleurs qui ne sont pas déjà associées à des
   voisins.

   - [rig] : le graphe d'interférence, qui permettra de connaître les voisins
   d'un registre.

   - [all_colors] : l'ensemble des couleurs que l'on peut allouer.

   - [next_stack_slot] : le prochain emplacement disponible sur la pile. Cela
   représentera des offsets négatifs par rapport à fp, on le mettra donc à jour
   en décrémentant cette valeur de 1.

   - [decision] : une décision parmi celles empilées.

   Cette fonction met à jour [allocation] et renvoie la nouvelle valeur de
   [next_stack_slot].

*)
let allocate (allocation: (reg, loc) Hashtbl.t) (rig: (reg, reg Set.t) Hashtbl.t)
    (all_colors: int Set.t)
    (next_stack_slot: int) (decision: regalloc_decision)
  : int =
  
  match decision with 
    | Spill r -> 
        Hashtbl.replace allocation r (Stk next_stack_slot); 
        next_stack_slot-1
    | NoSpill r ->
        let interfer = Hashtbl.find rig r in 
          let f_fold neighb_reg neighb_loc ans = 
            match neighb_loc with 
              | Stk i -> ans 
              | Reg neighb_color -> 
                  if (Set.mem neighb_reg interfer)
                      then Set.add neighb_color ans 
                  else
                    ans
          in
          let chosen_clrs = Hashtbl.fold f_fold allocation Set.empty in 
          
          let r_color = Set.any (Set.diff all_colors chosen_clrs) in 
            Hashtbl.replace allocation r (Reg r_color); 
            next_stack_slot

  

(* [regalloc_fun f live_out all_colors] effectue l'allocation de registres pour
   la fonction [f].

   - [live_out] est un mapping des numéros d'instructions dans la fonction
   Linear vers l'ensemble des registres vivants après cette instruction.

   - [all_colors] est l'ensemble des registres que l'on pourra utiliser.

   Cette fonction renvoie un triplet [(rig, allocation, next_stack_slot)] :

   - [rig] est le graphe d'interférences (simplement pour l'affichage)

   - [allocation] est l'allocation de registre que vous aurez construit

   - [next_stack_slot] est le prochain emplacement disponible sur la pile
   (utilisé dans [ltl_gen], qui vous est fourni.)
*)
let regalloc_fun (f: linear_fun)
    (live_out: (int, reg Set.t) Hashtbl.t)
    (all_colors: int Set.t) :
  (reg, reg Set.t) Hashtbl.t      (* the RIG *)
  * (reg, loc) Hashtbl.t          (* the allocation *)
  * int                         (* the next stack slot *)
  =
  let rig = build_interference_graph live_out f.linearfunbody in

  let allocation = Hashtbl.create 17 in
  (* Les pseudo-registres qui contiennent les arguments sont traités séparément
     dans [ltl_gen.ml]. On les enlève donc du graphe. *)
  List.iter (fun p -> remove_from_rig rig p) f.linearfunargs;
  (* On effectue une copie [g] du graphe d'interférence [rig]. En effet, comme
     on va supprimer des sommets du graphe, on perd l'information
     d'interférence, dont on aura besoin pour effectuer la coloration. *)
  let g = Hashtbl.copy rig in
  let stack = make_stack g [] (Set.cardinal all_colors) in
  let next_stack_slot =
    List.fold_left (fun next_stack_slot decision ->
        allocate allocation rig all_colors next_stack_slot decision
      ) (-1) stack in
  (rig, allocation, next_stack_slot)


(* [dump_interf_graph fname rig] affiche les interférences associées à chaque
   registre. Peut être utile pour le débogage ! Pas besoin d'inspecter cette
   fonction, à moins qu'elle soit buggée... :-) *)
let dump_interf_graph oc (fname, rig, allocation) =
  let colors = Array.of_list [
      "blue"; "red"; "orange"; "pink"; "green"; "purple";
      "brown"; "turquoise"; "gray"; "gold"; "darkorchid"; "bisque";
      "darkseagreen"; "cornsilk"; "burlywood"; "dodgerblue"; "antiquewhite"; "firebrick";
      "deepskyblue"; "darkolivegreen"; "hotpink"; "lightsalmon"; "magenta"; "lawngreen";
    ] in
  let color_of_allocation r =
    match Hashtbl.find_option allocation r with
    | Some (Reg r) ->
      Array.get colors (r mod Array.length colors)
    | _ -> "white"
  in
  Format.fprintf oc "subgraph cluster_%s{\n" fname;
  Format.fprintf oc "label=\"%s\";\n" fname;
  Hashtbl.keys rig |> Enum.iter (fun r ->
      Format.fprintf oc "%s_r%d [label=\"r%d\",style=filled,fillcolor=\"%s\"];\n" fname r r (color_of_allocation r)
    );
  Hashtbl.iter
    (fun i s ->
       Set.iter (fun x ->
           Format.fprintf oc "%s_r%d -> %s_r%d;\n" fname i fname x
         ) s;)
    rig;
  Format.fprintf oc "}\n"

let dump_interf_graphs oc allocations =
  Format.fprintf oc "digraph RIGS {\n";
  Hashtbl.iter (fun fname (rig, allocation, next_stack_slot) ->
      dump_interf_graph oc (fname, rig, allocation)
    ) allocations;
  Format.fprintf oc "}\n"

(* On applique l'allocation de registres à tout le programme Linear, et on
   affiche tout ça dans le rapport (la page HTML de chaque fichier). *)
let regalloc lp lives all_colors =
  let allocations = Hashtbl.create 17 in
  List.iter (function (fname,Gfun f) ->
      begin match Hashtbl.find_option lives fname with
      | Some (live_in, live_out) ->
        let (rig, allocation, curstackslot) =
          if !Options.naive_regalloc
          then let (al, nss) = regalloc_on_stack_fun f in
            (Hashtbl.create 0, al, nss)
          else regalloc_fun f live_out all_colors
        in
        Hashtbl.replace allocations fname (rig, allocation, curstackslot)
      | None -> ()
      end
    ) lp;
  dump !Options.rig_dump dump_interf_graphs allocations
    (call_dot "regalloc" "Register Allocation");
  allocations
