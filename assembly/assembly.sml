signature ASSEMBLY_INTERPRETER =
sig
    val runFile : string -> unit
end

structure Interpreter : ASSEMBLY_INTERPRETER =
struct

type memory = (int * int) list
type registers = int list

datatype action = 
    LOAD of int * int
  | STOREV of int * int
  | STORER of int * int
  | STOREM of int * int
  | ADDRR of int * int * int | ADDRV of int * int * int | ADDRM of int * int * int
  | ADDVR of int * int * int | ADDVV of int * int * int | ADDVM of int * int * int
  | ADDMR of int * int * int | ADDMV of int * int * int | ADDMM of int * int * int
  | SUBRR of int * int * int | SUBRV of int * int * int | SUBRM of int * int * int
  | SUBVR of int * int * int | SUBVV of int * int * int | SUBVM of int * int * int
  | SUBMR of int * int * int | SUBMV of int * int * int | SUBMM of int * int * int
  | JMP of string
  | JZ of int * string
  | JNZ of int * string
  | PRINT of int
  | LABEL of string
  | NOP

type line = int * action
type program = line list
exception assemblyError of string

fun execute program initialMemory initialRegisters =
let
  fun storeMemory [] address value = [(address, value)]
    | storeMemory ((currA, currV)::rest) address value =
        if currA = address then (address, value) :: rest
        else if currA > address then (address, value) :: (currA, currV) :: rest
        else (currA, currV) :: (storeMemory rest address value)

  fun loadMemory [] _ = 0
    | loadMemory ((currA, currV)::rest) address =
        if currA = address then currV
        else if currA > address then 0
        else loadMemory rest address

  fun storeRegister [] _ _ = []
    | storeRegister (x::xs) num value =
        if num = 1 then value :: xs else x :: (storeRegister xs (num - 1) value)

  fun loadRegister [] _ = raise assemblyError("Register not found")
    | loadRegister (x::xs) num =
        if num = 1 then x else loadRegister xs (num - 1)

  fun createLabelList [] = []
    | createLabelList ((m, LABEL str)::xs) = (str, m) :: createLabelList xs
    | createLabelList (_::xs) = createLabelList xs

  fun findPC [] label = raise assemblyError("Unknown label: " ^ label)
    | findPC ((l, pc)::xs) label = if l = label then pc else findPC xs label

  fun getNextLine [] _ = []
    | getNextLine ((n, act)::rest) target =
        if n = target then (n, act)::rest else getNextLine rest target

  fun loop _ [] _ _ _ = ()
    | loop programCode ((line, instr)::rest) mem regs labels =
      case instr of
          LOAD (addr, r)   => loop programCode rest mem (storeRegister regs r (loadMemory mem addr)) labels
        | STOREV (addr, v) => loop programCode rest (storeMemory mem addr v) regs labels
        | STORER (addr, r) => loop programCode rest (storeMemory mem addr (loadRegister regs r)) regs labels
        | STOREM (a1, a2)  => loop programCode rest (storeMemory mem a1 (loadMemory mem a2)) regs labels
        
        | ADDRR (d, r1, r2) => loop programCode rest mem (storeRegister regs d (loadRegister regs r1 + loadRegister regs r2)) labels
        | ADDRV (d, r1, v2) => loop programCode rest mem (storeRegister regs d (loadRegister regs r1 + v2)) labels
        | ADDRM (d, r1, a2) => loop programCode rest mem (storeRegister regs d (loadRegister regs r1 + loadMemory mem a2)) labels
        | ADDVR (d, v1, r2) => loop programCode rest mem (storeRegister regs d (v1 + loadRegister regs r2)) labels
        | ADDVV (d, v1, v2) => loop programCode rest mem (storeRegister regs d (v1 + v2)) labels
        | ADDVM (d, v1, a2) => loop programCode rest mem (storeRegister regs d (v1 + loadMemory mem a2)) labels
        | ADDMR (d, a1, r2) => loop programCode rest mem (storeRegister regs d (loadMemory mem a1 + loadRegister regs r2)) labels
        | ADDMV (d, a1, v2) => loop programCode rest mem (storeRegister regs d (loadMemory mem a1 + v2)) labels
        | ADDMM (d, a1, a2) => loop programCode rest mem (storeRegister regs d (loadMemory mem a1 + loadMemory mem a2)) labels

        | SUBRR (d, r1, r2) => loop programCode rest mem (storeRegister regs d (loadRegister regs r1 - loadRegister regs r2)) labels
        | SUBRV (d, r1, v2) => loop programCode rest mem (storeRegister regs d (loadRegister regs r1 - v2)) labels
        | SUBRM (d, r1, a2) => loop programCode rest mem (storeRegister regs d (loadRegister regs r1 - loadMemory mem a2)) labels
        | SUBVR (d, v1, r2) => loop programCode rest mem (storeRegister regs d (v1 - loadRegister regs r2)) labels
        | SUBVV (d, v1, v2) => loop programCode rest mem (storeRegister regs d (v1 - v2)) labels
        | SUBVM (d, v1, a2) => loop programCode rest mem (storeRegister regs d (v1 - loadMemory mem a2)) labels
        | SUBMR (d, a1, r2) => loop programCode rest mem (storeRegister regs d (loadMemory mem a1 - loadRegister regs r2)) labels
        | SUBMV (d, a1, v2) => loop programCode rest mem (storeRegister regs d (loadMemory mem a1 - v2)) labels
        | SUBMM (d, a1, a2) => loop programCode rest mem (storeRegister regs d (loadMemory mem a1 - loadMemory mem a2)) labels

        | JMP l => loop programCode (getNextLine programCode (findPC labels l)) mem regs labels
        | JZ (r, l) => 
            if (loadRegister regs r) = 0 
            then loop programCode (getNextLine programCode (findPC labels l)) mem regs labels
            else loop programCode rest mem regs labels
        | JNZ (r, l) => 
            if (loadRegister regs r) <> 0 
            then loop programCode (getNextLine programCode (findPC labels l)) mem regs labels
            else loop programCode rest mem regs labels

        | PRINT r => (print (Int.toString (loadRegister regs r) ^ "\n"); loop programCode rest mem regs labels)
        | _ => loop programCode rest mem regs labels
in
  loop program program initialMemory initialRegisters (createLabelList program)
end;

structure Parser =
struct
    fun stripPrefix s =
        let val chars = String.explode s
        in
            if List.null chars then 0
            else case List.hd chars of
                (#"r" | #"m" | #"R" | #"M") => valOf (Int.fromString (String.implode (List.tl chars)))
              | _ => valOf (Int.fromString s)
        end handle _ => raise assemblyError ("Invalid token: " ^ s)

    fun parseLine [i] = if i = "NOP" then NOP else raise assemblyError "Bad NOP"
      | parseLine [i, a] = (case i of "PRINT" => PRINT (stripPrefix a) | "JMP" => JMP a | "LABEL" => LABEL a | _ => NOP)
      | parseLine [i, a, b] = (case i of 
            "LOAD" => LOAD(stripPrefix a, stripPrefix b) | "STOREV" => STOREV(stripPrefix a, stripPrefix b)
          | "STORER" => STORER(stripPrefix a, stripPrefix b) | "STOREM" => STOREM(stripPrefix a, stripPrefix b)
          | "JZ" => JZ(stripPrefix a, b) | "JNZ" => JNZ(stripPrefix a, b) | _ => NOP)
      | parseLine [i, d, a, b] =
            let val (id, i1, i2) = (stripPrefix d, stripPrefix a, stripPrefix b) in
                case i of
                  "ADDRR" => ADDRR(id,i1,i2) | "ADDRV" => ADDRV(id,i1,i2) | "ADDRM" => ADDRM(id,i1,i2)
                | "ADDVR" => ADDVR(id,i1,i2) | "ADDVV" => ADDVV(id,i1,i2) | "ADDVM" => ADDVM(id,i1,i2)
                | "ADDMR" => ADDMR(id,i1,i2) | "ADDMV" => ADDMV(id,i1,i2) | "ADDMM" => ADDMM(id,i1,i2)
                | "SUBRR" => SUBRR(id,i1,i2) | "SUBRV" => SUBRV(id,i1,i2) | "SUBRM" => SUBRM(id,i1,i2)
                | "SUBVR" => SUBVR(id,i1,i2) | "SUBVV" => SUBVV(id,i1,i2) | "SUBVM" => SUBVM(id,i1,i2)
                | "SUBMR" => SUBMR(id,i1,i2) | "SUBMV" => SUBMV(id,i1,i2) | "SUBMM" => SUBMM(id,i1,i2)
                | _ => NOP
            end
      | parseLine _ = NOP

    fun fromString text =
        let
            val lines = String.fields (fn c => c = #"\n") text
            fun isClean c = Char.isSpace c orelse c = #","
            fun process (raw, (cnt, acc)) =
                case String.tokens isClean raw of
                    [] => (cnt, acc)
                  | tokens => (cnt+1, (cnt, parseLine tokens)::acc)
            val (_, prog) = List.foldl process (0, []) lines
        in List.rev prog end
end;

(* --- NEW FILE HANDLING LOGIC --- *)

fun runFile filename =
    let
        val ins = TextIO.openIn filename
        val content = TextIO.inputAll ins
        val _ = TextIO.closeIn ins

        val program = Parser.fromString content

        val initialMemory = []
        val initialRegisters = List.tabulate(10, fn _ => 0) (* currently 10 registers 1-10*)
    in
        execute program initialMemory initialRegisters
    end
    handle e => (print ("Error: " ^ General.exnMessage e ^ "\n"))

end;

Interpreter.runFile "code.txt";
