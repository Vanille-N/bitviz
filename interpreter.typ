#import "assembler.typ"
#import "memory/virtual.typ"
#import "values/values.typ"

#let r_sp = 13
#let r_lr = 14
#let r_pc = 15

/*
#let identity(v) = (ok: true, val: v)

#let chain-transforms(..funs) = v => {
  for fun in funs.pos() {
    v = fun(v)
    if not v.ok { return v }
    v = v.val
  }
  identity(v)
}


/// All the possible types of values:
/// - (lab: "foo") is the address of "foo" in the source
/// - (eq: "foo") equals the label itself, not its address
/// - (addr: 123) is the address 123, accounting for the convention
///   on assembler.data-offset and assembler.text-offset
/// - (int: "123") is the integer 123
/// - (hex: "1f2") is the hexadecimal integer 0x1f2
/// - (byte: (_,_,_,_,_,_,_,_)) is exactly one byte
/// - (bits: (_,_,...,_)) is an arbitrary amount of bits
/// - (instr: ..) is an executable instruction
/// - (meta: ..) is a print statement
/// - (reg: i) is the register ri
/// - (deref: (..,)) dereferencing a register possibly with offsets
#let cases(v) = (..arms) => {
  let arms = arms.named()
  if "lab" in v {
    (arms.lab)(v)
  } else if "addr" in v {
    (arms.addr)(v)
  } else if "int" in v {
    (arms.int)(v)
  } else if "hex" in v {
    (arms.hex)(v)
  } else if "byte" in v {
    (arms.byte)(v)
  } else if "bits" in v {
    (arms.bits)(v)
  } else if "instr" in v {
    (arms.instr)(v)
  } else if "meta" in v {
    (arms.meta)(v)
  } else if "eq" in v {
    (arms.eq)(v)
  } else if "reg" in v {
    (arms.reg)(v)
  } else if "deref" in v {
    (arms.deref)(v)
  } else if "poison" in v {
    (arms.poison)(v)
  } else {
    panic(v)
  }
}

#let byte-to-bits(v, size: 4) = {
  (ok: true, val: (bits: bitwise.cast(v.byte, 8 * size)))
}

#let lab-to-addr(machine) = v => {
  if v.lab in machine.data.tags {
    (ok: true, val: machine.data.tags.at(v.lab))
  } else if v.lab in machine.text.tags {
    (ok: true, val: machine.text.tags.at(v.lab))
  } else {
    (ok: false, msg: [No label <#lab> declared])
  }
}

#let int-to-addr(v) = {
  (ok: true, val: (addr: v.int))
}

#let hex-to-addr(v) = {
  (ok: true, val: (addr: eval("0x" + v.hex)))
}

#let bits-to-addr(v) = {
  let i = bitwise.to-int(v.bits)
  if i == none {
    (ok: false, msg: [cannot interpret abstract value #v as address])
  } else {
    (ok: true, val: (addr: i))
  }
}

#let byte-to-addr(v) = chain-transforms(byte-to-bits, bits-to-addr)(v)

#let instr-to-addr(v) = {
  (ok: false, msg: [Cannot interpret as an address])
}

#let reg-to-addr(machine, to-addr) = (v) => {
  let val = machine.regs.at(v.reg)
  to-addr(machine)(val)
}

#let deref-to-addr(machine, to-addr) = (v) => {
  let addr = 0
  for elt in v.deref {
    let ans = to-addr(machine)(elt)
    if not ans.ok { return ans }
    addr += ans.val.addr
  }
  (ok: true, val: (addr: addr))
}

// Interprets this value as an address.
// An address is one of:
// - a label (lab: _),
// - a label (eq: _),
// - an explicit (addr: _).
#let to-addr(machine) = v => {
  cases(v)(
    addr: x => (ok: true, val: x),
    lab: lab-to-addr(machine),
    int: int-to-addr,
    hex: hex-to-addr,
    byte: byte-to-addr,
    bits: bits-to-addr,
    instr: instr-to-addr,
    reg: reg-to-addr(machine, to-addr),
    deref: deref-to-addr(machine, to-addr),
  )
}

#let reg-to-addr = (machine) => reg-to-addr(machine, to-addr)

#let int-to-bits(size: 4) = (v) => {
  (ok: true, val: (bits: bitwise.convert-decimal(v.int, size: 8 * size)))
}

#let hex-to-bits(size: 4) = (v) => {
  let (bits, _) = bitwise.convert("0x" + v.hex)(none)
  (ok: true, val: (bits: bitwise.cast(bits, 8 * size)))
}

#let addr-to-bits(size: 4) = (v) => {
  int-to-bits((int: v.addr), size: size)
}

#let reg-to-bits(machine, size: 4, to-bits) = (v) => {
  let val = machine.regs.at(v.reg)
  to-bits(machine, size: size)(val)
}

#let to-bits(machine, size: 4) = (v) => {
  cases(v)(
    int: int-to-bits(size: size),
    hex: hex-to-bits(size: size),
    reg: reg-to-bits(machine, to-bits, size: size),
    bits: x => (ok: true, val: (bits: bitwise.cast(x.bits, 8 * size))),
    poison: _ => (ok: true, val: (bits: (none,) * 8 * size)),
  )
}

#let to-val(machine) = (v) => {
  cases(v)(
    int: identity,
    hex: identity,
    reg: r => identity(machine.regs.at(v.reg))
  )
}
*/

#let load-instr(machine) = (addr) => {
  let instr = virtual.vload(machine.vmem, addr, size: 4, section: <text>, algebra: values.algebra)
  if not instr.ok { return instr }
  (ok: true, val: instr.val)
}

#let print-op(op) = {
  if "reg" in op {
    "r" + str(op.reg) + (
      if "start" in op or "end" in op {
        "[" + if op.start != auto { op.start.int } else { "" } + ":" + if op.len != auto { op.len.int } else { "" } + "]"
      } else { "" }
    )
  } else if "eq" in op {
    "=" + op.eq
  } else if "deref" in op {
    "[" + op.deref.map(print-op).join(", ") + "]"
  } else if "lab" in op {
    op.lab
  } else if "int" in op {
    "#" + str(op.int)
  } else if "hex" in op {
    "#0x" + op.hex
  } else {
    panic(op)
  }
}

#let print-instruction(instr) = {
  let cmd = instr.instr
  if cmd in ("ldr", "mov") {
    cmd += (none, "b", "h", none, "").at(instr.size)
  }
  raw(cmd + " " + instr.ops.map(print-op).join(", "))
  linebreak()
}

#let fetch-and-incr-pc(machine) = {
  let pc = machine.regs.at(r_pc)
  let addr = values.to-int(pc, refs: machine.refs)
  if not addr.ok { return addr }
  let instr = load-instr(machine)(addr.val)
  if not instr.ok { return instr }
  instr = instr.val
  let pc = values.addi(pc, 4) // next instruction 1 word ahead
  if not pc.ok { return pc }
  machine.regs.at(r_pc) = pc.val
  (ok: true, val: (instr: instr, machine: machine))
}

#let load-value(machine, src, size: 4) = {
  if "eq" in src {
    assert(size == 4)
    (ok: true, val: (lab: (base: src.eq)))
  } else if "lab" in src {
    let addr = values.lab_.to-int((base: src.lab), refs: machine.refs)
    if not addr.ok { return addr }
    let val = virtual.vload(machine.vmem, addr.val, size: size, section: <text>, algebra: values.algebra)
    val
  } else if "deref" in src {
    let curr = 0
    for elt in src.deref {
      if "reg" in elt {
        let vreg = machine.regs.at(elt.reg)
        let i = values.to-int(vreg, refs: machine.refs)
        if not i.ok { return i }
        curr += i.val
      } else if "hex" in elt or "int" in elt {
        let v = values.to-int(elt)
        if not v.ok { return v }
        curr += v.val
      } else {
        panic(elt)
      }
    }
    let val = virtual.vload(machine.vmem, curr, size: size, algebra: values.algebra)
    val
  } else {
    panic(src)
    let addr = to-addr(machine)(src)
    if not addr.ok { return offset }
    let val = load-from-memory(machine)(addr.val.addr, size: size)
    val
  }
}

#let to-value(machine) = (v, size: 4) => {
  let val = if "reg" in v {
    machine.regs.at(v.reg)
  } else if "int" in v or "hex" in v {
    v
  } else {
    panic(v)
  }
  values.cast(val, size: size, refs: machine.refs, lossless: false, loose: true)
}

#let run(machine, entry) = {
  raw(str(entry) + ":")
  linebreak()
  machine.regs.at(r_pc) = (lab: (base: str(entry)))
  while true {
    let instr = fetch-and-incr-pc(machine)
    if not instr.ok { [Error: #{instr.msg}]; break }
    machine = instr.val.machine
    instr = instr.val.instr
    if not "instr" in instr { [Error: #instr is not executable]; break }
    if "print" in instr {
      panic("Unimplemented")
      /*
      let width = instr.print.width
      let regs = instr.print.regs
      let bits = ()
      for reg in regs {
        let val = interp-as-bits(registers.at(reg.reg))
        if "start" in reg and reg.start != auto {
          val = val.slice(int(reg.start.int))
        }
        if "len" in reg and reg.len != auto {
          val = val.slice(0, int(reg.len.int))
        }
        bits += val
      }
      if width == auto {
        width = bits.len()
      }
      text(fill: blue)[`print:` #raw(regs.map(print-op).join(", "))]
      bitwise.display(bits, size: 4.5mm, cols: width, colors: bitwise.rainbow(machine.data.prov-count))
      */
    } else if "instr" in instr {
      print-instruction(instr)
      let (instr, ops, ..options) = instr
      if instr == "ldr" {
        if ops.len() != 2 { [Error: `ldr` expects 2 operands]; break}
        let (dest, src) = ops
        if not "reg" in dest { [Error: `ldr` destination must be a register]; break }
        let val = load-value(machine, src, size: options.size)
        if not val.ok { [Error: #val.msg]; break }
        val = val.val
        machine.regs.at(dest.reg) = val
      } else if instr == "lsl" {
        panic("Unimplemented")
        /*
        let (dest, src, shift) = {
          if ops.len() == 2 {
            (ops.at(0),) +  ops
          } else if ops.len() == 3 {
            ops
          } else {
            return [Error: `lsl` expects 2 or 3 operands]
          }
        }
        if "reg" not in dest { return [Error: `lsl` destination must be a register] }
        let val = if "reg" in src {
          let val = registers.at(src.reg)
          if "bits" in val {
            val.bits
          } else {
            return [Error: `lsl` source must be a non-opaque value]
          }
        } else {
          panic(src)
        }
        let vshift = interp-as-index(shift, regs: registers)
        if type(vshift) != int {
          return [Error: cannot interpret #shift as a shift: #vshift]
        }
        let final = bitwise.lsl(val, vshift)
        registers.at(dest.reg) = (bits: final)
        */
      } else if instr == "lsr" {
        panic("Unimplemented")
        /*
        let (dest, src, shift) = {
          if ops.len() == 2 {
            (ops.at(0),) +  ops
          } else if ops.len() == 3 {
            ops
          } else {
            return [Error: `lsr` expects 2 or 3 operands]
          }
        }
        if "reg" not in dest { return [Error: `lsr` destination must be a register] }
        let val = if "reg" in src {
          let val = registers.at(src.reg)
          if "bits" in val {
            val.bits
          } else {
            return [Error: `lsr` source must be a non-opaque value]
          }
        } else {
          panic(src)
        }
        let vshift = interp-as-index(shift, regs: registers)
        if type(vshift) != int {
          return [Error: cannot interpret #shift as a shift: #vshift]
        }
        let final = bitwise.lsr(val, vshift)
        registers.at(dest.reg) = (bits: final)
        */
      } else if instr == "orr" {
        panic("Unimplemented")
        /*
        let (dest, src1, src2) = {
          if ops.len() == 2 {
            (ops.at(0),) +  ops
          } else if ops.len() == 3 {
            ops
          } else {
            return [Error: `orr` expects 2 or 3 operands]
          }
        }
        if "reg" not in dest { return [Error: `orr` destination must be a register] }
        if "reg" not in src1 { return [Error: `orr` operates only on registers] }
        if "reg" not in src2 { return [Error: `orr` operates only on registers] }
        let val1 = registers.at(src1.reg)
        let val2 = registers.at(src2.reg)
        let val1 = if "bits" in val1 {
          val1.bits
        } else {
          return [Error: `orr` values must not be opaque]
        }
        let val2 = if "bits" in val2 {
          val2.bits
        } else {
          return [Error: `orr` values must not be opaque]
        }
        let final = bitwise.bor(val1, val2)
        registers.at(dest.reg) = (bits: final)
        */
      } else if instr == "and" {
        panic("Unimplemented")
        /*
        let (dest, src1, src2) = {
          if ops.len() == 2 {
            (ops.at(0),) +  ops
          } else if ops.len() == 3 {
            ops
          } else {
            return [Error: `and` expects 2 or 3 operands]
          }
        }
        if "reg" not in dest { return [Error: `and` destination must be a register] }
        if "reg" not in src1 { return [Error: `and` operates only on registers] }
        if "reg" not in src2 { return [Error: `and` operates only on registers] }
        let val1 = registers.at(src1.reg)
        let val2 = registers.at(src2.reg)
        let val1 = if "bits" in val1 {
          val1.bits
        } else {
          return [Error: `and` values must not be opaque]
        }
        let val2 = if "bits" in val2 {
          val2.bits
        } else {
          return [Error: `and` values must not be opaque]
        }
        let final = bitwise.band(val1, val2)
        registers.at(dest.reg) = (bits: final)
        */
      } else if instr == "eor" {
        panic("Unimplemented")
        /*
        let (dest, src1, src2) = {
          if ops.len() == 2 {
            (ops.at(0),) +  ops
          } else if ops.len() == 3 {
            ops
          } else {
            return [Error: `eor` expects 2 or 3 operands]
          }
        }
        if "reg" not in dest { return [Error: `eor` destination must be a register] }
        if "reg" not in src1 { return [Error: `eor` operates only on registers] }
        if "reg" not in src2 { return [Error: `eor` operates only on registers] }
        let val1 = registers.at(src1.reg)
        let val2 = registers.at(src2.reg)
        let val1 = if "bits" in val1 {
          val1.bits
        } else {
          return [Error: `orr` values must not be opaque]
        }
        let val2 = if "bits" in val2 {
          val2.bits
        } else {
          return [Error: `eor` values must not be opaque]
        }
        let final = bitwise.bxor(val1, val2)
        registers.at(dest.reg) = (bits: final)
        */
      } else if instr == "mvn" {
        let (dest, src) = {
          if ops.len() == 2 {
            ops
          } else if ops.len() == 1 {
            ops + ops
          } else {
            [Error: `mvn` expects at most 2 operands]
            break
          }
        }
        if "reg" not in dest { [Error: `mvn` destination must be a register]; break }
        let val = to-value(machine)(src, size: 4)
        if not val.ok { [Error: #val.msg]; break }
        let neg = values.bnot(val.val, size: 4, refs: machine.refs)
        if not neg.ok { [Error: #neg.msg]; break }
        machine.regs.at(dest.reg) = neg.val
      } else if instr == "mov" {
        if ops.len() != 2 { [Error: `mov` expects 2 operands]; break }
        let (dest, src) = ops
        if "reg" not in dest { [Error: `mov` destination must be a register]; break }
        let val = to-value(machine)(src, size: options.size)
        if not val.ok { [Error: #val.msg]; break }
        val = val.val
        machine.regs.at(dest.reg) = val
      } else if instr == "b" {
        if ops.len() != 1 { [Error: `b` expects 1 operand]; break }
        let (tag,) = ops
        if "lab" not in tag { [Error: `b`'s destination must be a label]; break }
        if tag.lab == "exit" {
          break
        } else {
          panic("Unimplemented")
        }
      } else {
        panic(instruction)
      }
    } else {
      break
      panic(instruction)
    }
  }
  assembler.print-machine(machine)
}
