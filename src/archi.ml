open Config

type archi = A64 | A32

let archi = ref A64
let nbits () =
  match !archi with
  | A64 -> 64
  | A32 -> 32

let wordsize () = nbits () / 8

let assembler () =
  let opts =
    match !archi with
    | A64 -> "-march=rv64imafdc -mabi=lp64d"
    | A32 -> "-march=rv32imafdc -mabi=ilp32"
  in
  Format.sprintf "%s %s" Config.rv_as opts

let linker () =
  let opts =
    match !archi with
      A64 -> "-melf64lriscv"
    | A32 -> "-melf32lriscv"
  in
  Format.sprintf "%s %s" Config.rv_ld opts

let instrsuffix () =
  match !archi with
  | A64 -> 'd'
  | A32 -> 'w'

let qemu () =
  match !archi with
  | A64 -> Config.qemu64
  | A32 -> Config.qemu32

let heapstart = ref 8

type target_system =
  | Linux
  | Xv6

let target = ref Linux

let lib_syscall () =
  match !target with
  | Linux -> "linux"
  | Xv6 -> "xv6"

let target_data_segment t =
  match t with
  | Linux -> "8000000"
  | Xv6 -> "2000"

let runtime_lib_include_path () =
  Format.sprintf "%s/%s" Config.runtime_dir (lib_syscall ())

let runtime_lib_path () =
  Format.sprintf "%s/lib%d.s" Config.runtime_dir (nbits ())
