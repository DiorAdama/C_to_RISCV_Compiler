open Batteries
open BatPrintf
open BatBuffer
open BatList

type 'a res = OK of 'a | Error of string
type ('a , 'b) sum = Inl of 'a | Inr of 'b
type ('a , 'b, 'c) trisum = Tri1 of 'a | Tri2 of 'b | Tri3 of 'c

let string_of_error (msg: string) : string =
  msg

let sprintf msg =
  let s = BatBuffer.create 17 in
  msg (output_buffer s);
  BatBuffer.contents s



let option_to_res_bind r m f = match r with
  | Some r -> f r
  | None -> Error m

let (>>) r m f = option_to_res_bind r m f

let option_bind o f  = match o with
  | None -> None
  | Some x -> Some (f x)

let (>*>) o f = match o with
  | None -> None
  | Some x -> Some (f x)

let error_bind r f = match r with
  | OK r -> f r
  | Error msg -> Error msg

let (>>=) r f = error_bind r f

let error_fail r f  = match r with
  | OK r -> f r
  | Error msg -> failwith msg

let (>>!) r f  = error_fail r f

let (>>*) r g f = match r with
  | OK r -> f r
  | Error msg -> g msg

let ($) f a = f a

let rec assoc_set (l: ('a * 'b) list) (a: 'a) (b: 'b) =
  match l with
  | [] -> [(a,b)]
  | (k,v)::r -> if k = a then (a,b)::r
    else (k,v)::(assoc_set r a b)

let safe_array_get a i =
  try Some (Array.get a i)
  with Not_found -> None

(* split_bytes n i splits an integer i into n bytes, starting with LSB *)
let rec split_bytes n i =
  if n <= 0 then []
  else i mod 256 :: split_bytes (n-1) (i / 256)


let write_mem_bytes mem addr bl =
  bl |>
  List.fold_lefti (fun acc i b ->
      acc >>= fun l ->
      let ofs = addr+i in let v = b mod 256 in
      try mem.(ofs) <- v; OK ((ofs,v)::l)
      with _ -> Error (Format.sprintf "Problem when writing mem at address %d\n" ofs)
    ) (OK [])

(* let write_mem_int mem addr v =
 *   split_bytes !Archi.wordsize v |> rev |> write_mem_bytes mem addr *)

let write_mem_char mem addr c = write_mem_bytes mem addr [c]

let int_of_bytes l =
  List.fold_left (fun acc b -> acc * 256 + b) 0 l

(* list [0,...,n-1] *)
let rec list_ints_inc n =
  if n <= 0 then []
  else list_ints_inc (n-1) @ [n-1]

(* list [n,...0] *)
let rec list_ints_desc n =
  if n <= 0 then [0]
  else n :: list_ints_desc (n-1)

let range ?desc:(desc=false) ?start:(start=0) len =
  let rec list_ints n acc =
    if n <= 0 then acc else list_ints (n-1) ((n-1) :: acc)
  in
  let l = List.map ((+) start) (list_ints len []) in
  if not desc then l else List.rev l


let read_mem_bytes mem addr n =
  List.fold_left (fun acc i ->
      acc >>= fun (lv, laddr) ->
      try let v = mem.(addr+i) in OK (lv@[v], (addr+i)::laddr)
      with _ -> Error (Format.sprintf "Problem when reading from mem at address %d\n" (addr+i))
    ) (OK ([],[])) (list_ints_inc n)

let read_mem_bytes_as_int mem addr n =
  read_mem_bytes mem addr n >>= fun (bl, read_list) ->
  OK (int_of_bytes (rev bl), read_list)


let read_mem_int mem addr =
  read_mem_bytes_as_int mem addr !Archi.wordsize

let read_mem_char mem addr =
  read_mem_bytes mem addr 1 >>= fun bl ->
  match bl with
  | [a],addrl -> OK (a,addrl)
  | _ -> Error (Format.sprintf "unable to read from mem at addr %d" addr)

module Mem : sig
  type t
  val init : int -> t
  val write_bytes : t -> int -> int list -> unit res
  val write_char : t -> int -> int -> unit res
  val read_bytes : t -> int -> int -> int list res
  val read_bytes_as_int : t -> int -> int -> int res
  val read_char : t -> int -> int res
  val read_log : t -> unit -> int list
  val write_log : t -> unit -> (int * int) list
end = struct
  type t = int array * int list ref * (int * int) list ref
  let write_bytes (m,rl,wl) addr bytes =
    write_mem_bytes m addr bytes >>= fun w -> wl := !wl @ w; OK ()
  let write_char (m,rl,wl) addr c =
    write_mem_char m addr c >>= fun w -> wl := !wl @ w; OK ()
  let read_bytes (m,rl,wl) addr len =
    read_mem_bytes m addr len >>= fun (vl,addrl) ->
    rl := !rl @ addrl; OK vl
  let read_bytes_as_int (m,rl,wl) addr len =
    read_mem_bytes_as_int m addr len >>= fun (v,addrl) ->
    rl := !rl @ addrl; OK v
  let read_char (m,rl,wl) addr =
    read_mem_char m addr >>= fun (v,addrl) ->
    rl := !rl @ addrl; OK v
  let init n = Array.init n (fun _ -> 0), ref [], ref []
  let read_log (_,rl,_) () = let r = !rl in rl := []; r
  let write_log (_,_,wl) () = let w = !wl in wl := []; w
end

let assoc_opti k l =
  let rec aux l n =
    match l with
    | [] -> None
    | (a,v)::l when a = k -> Some (n, v)
    | _::l -> aux l (n+1)
  in
  aux l 0

let assoc_map f =
  List.map (fun (k,v) -> (k, f v))

let assoc_map_res f l =
  List.fold_left (fun acc (k,v) ->
      acc >>= fun acc ->
      f k v >>= fun v ->
      OK (acc@[(k,v)])
    ) (OK []) l

let rec assoc_split fl fr l =
  let rec aux l (accl, accr) =
  match l with
  | [] -> (accl, accr)
  | (s, Inl x)::r -> aux r ((s, fl x)::accl, accr)
  | (s, Inr x)::r -> aux r (accl, (s, fr x)::accr)
  in aux l ([],[]) |>
     fun (a,b) -> (List.rev a, List.rev b)

type string_env = int ref * (int, string) Hashtbl.t

let lookup_string_env (senv: string_env) i =
  Hashtbl.find_option (snd senv) i

let add_string_env (senv: string_env) s =
  let cur = !(fst senv) in
  Hashtbl.replace (snd senv) cur s;
  fst senv := cur + 1;
  cur

let init_string_env () =
  let i = ref 0 in
  let t = Hashtbl.create 17 in
  (i, t)



let print_list f beg sep fin oc l =
  Format.fprintf oc "%s" beg;
  List.iteri (fun i x ->
      if i <> 0 then Format.fprintf oc "%s" sep;
      f oc x
    ) l;
  Format.fprintf oc "%s" fin


let print_listi f beg sep fin oc l =
  Format.fprintf oc "%s" beg;
  List.iteri (fun i x ->
      if i <> 0 then Format.fprintf oc "%s" sep;
      f i oc x
    ) l;
  Format.fprintf oc "%s" fin

let print_option f = function
  | None ->  "None"
  | Some o -> Format.sprintf "Some(%s)" (f o)

let pp_list f oc l =
  Format.pp_print_list ~pp_sep:(fun oc () -> Format.fprintf oc ", ") f oc l

let pp_pair f1 f2 oc (p1,p2) =
  Format.fprintf oc "(%a,%a)" f1 p1 f2 p2

let dump_option f oc = function
  | None -> Format.fprintf oc "None"
  | Some o -> Format.fprintf oc "Some(%a)" f o


let dump_string oc s = Format.fprintf oc "%s" s


let print_optint oc = function
  | None -> Format.fprintf oc "None"
  | Some v -> Format.fprintf oc "Some(%d)" v

let set_concat sl =
  List.fold_left (fun acc e -> Set.union acc e) Set.empty sl

let list_map_res f l =
  List.fold_left (fun acc e ->
      acc >>= fun acc ->
      f e >>= fun e ->
      OK (acc@[e])
    ) (OK []) l

let assoc_err ?word:(word="item") k l =
  match List.assoc_opt k l with
  | Some v -> OK v
  | None -> Error (Format.sprintf "%s %s not found." word k)


let remove_dups l : 'a list =
  List.fold_left (fun acc elt -> if List.mem elt acc then acc else elt::acc) [] l

let rec take n l =
  if n = 0 then []
  else match l with
    | [] -> []
    | a::r -> a::take (n-1) r

let char_list_of_string l : char list =
  String.to_list l

let string_of_char_list cl =
  String.of_list cl

let string_of_char_set s =
  string_of_char_list (Set.to_list s)

let string_of_int_list l =
  Printf.sprintf "%s" (String.concat "_" (List.map string_of_int l))

let string_of_int_set s =
  string_of_int_list (Set.to_list s)


