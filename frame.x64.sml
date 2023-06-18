structure X64Frame : FRAME =
struct

  datatype format = Int8 | Int16 | Int32 | Int64 | Float32 | Float64 | Address

  datatype register =
    (* 64bit *)
    RAX | RBX | RCX | RDX | RSI | RDI | RBP | RSP | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15 |
    (* 32bit *)
    EAX | EBX | ECX | EDX | ESI | EDI | EBP | ESP | R8D | R9D | R10D | R11D | R12D | R13D | R14D | R15D |
    (* 16bit *)
    AX | BX | CX | DX | SI | DI | BP | SP | R8W | R9W | R10W | R11W | R12W | R13W | R14W | R15W |
    (* 8bit *)
    AL | BL | CL | DL | SIL | DIL | BPL | SPL | R8B | R9B | R10B | R11B | R12B | R13B | R14B | R15B | AH | BH | CH | DH |
    (* 128bit FP *)
    XMM0 | XMM1 | XMM2 | XMM3 | XMM4 | XMM5 | XMM6 | XMM7 | XMM8 | XMM9 | XMM10 | XMM11 | XMM12 | XMM13 | XMM14 | XMM15

  datatype access =
      InRegister of { register: register, frame: int }
    | InFrame of int

  datatype frame = Frame of { name: Temp.label, args: access list, size: int }

  fun newFrame (name, argList) = 
    let

      val floatingPointArgRegisters = [XMM0, XMM1, XMM2, XMM3]
      val integerArgRegisters = [RCX, RDX, R8, R9]

      val stackSize = ref 0

      (* Spilling space is allocated in frame for register-allocated parameters *)
      (* First four integer and pointer parameters go in integerArgRegisters *)
      (* First four floating-point parameters go in integerArgRegisters *)
      fun allocateArgs ([], _, _) = []
        | allocateArgs (Int32::rest, [], availableFloat) =
            (stackSize := !stackSize + 4; (InFrame (!stackSize - 4))::(allocateArgs (rest, [], availableFloat)))
        | allocateArgs (Int32::rest, reg::availableInt, availableFloat) =
            (stackSize := !stackSize + 4; (InRegister { register = reg, frame = (!stackSize - 4) })::(allocateArgs (rest, availableInt, availableFloat)))
        | allocateArgs (Float32::rest, availableInt, []) =
            (stackSize := !stackSize + 4; (InFrame (!stackSize - 4))::(allocateArgs (rest, availableInt, [])))
        | allocateArgs (Float32::rest, availableInt, reg::availableFloat) =
            (stackSize := !stackSize + 4; (InRegister { register = reg, frame = (!stackSize - 4) })::(allocateArgs (rest, availableInt, availableFloat)))
        | allocateArgs (Address::rest, [], availableFloat) =
            (stackSize := !stackSize + 8; (InFrame (!stackSize - 8))::(allocateArgs (rest, [], availableFloat)))
        | allocateArgs (Address::rest, reg::availableInt, availableFloat) =
            (stackSize := !stackSize + 8; (InRegister { register = reg, frame = (!stackSize - 8) })::(allocateArgs (rest, availableInt, availableFloat)))
        | allocateArgs (_, _, _) = raise Utils.NotImplemented

      val args = allocateArgs (argList, integerArgRegisters, floatingPointArgRegisters)
    in
      Frame { name = name, args = args, size = !stackSize }
    end

end

structure Frame : FRAME = X64Frame

(* how do iterators work with stack frames? *)
(* how do closures work with stack frames? *)

