type archi = A64 | A32

let wordsize = ref 8
let assembler = ref "riscv64-unknown-elf-gcc"
let instrsuffix = ref 'd'
let archi = ref A64
let nbits = ref 64

let heapstart = ref 8

let init_archi a () =
  begin match a with
    | A64 ->
      begin
        wordsize := 8;
        assembler := "riscv64-unknown-elf-gcc";
        instrsuffix := 'd';
        archi := A64;
      end
    | A32 ->
      begin
        wordsize := 4;
        assembler := "riscv32-unknown-elf-gcc";
        instrsuffix := 'w';
        archi := A32;
      end
  end
  ;
  nbits := !wordsize *8;
