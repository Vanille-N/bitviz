#import "memory/virtual.typ"
#import "values/values.typ"

#let cc-section(vmem, elts, section: auto, refs: (:), allow-provenance: false) = {
  let prov-count = if allow-provenance { 1 } else { none }
  let new-refs = (:)
  for elt in elts {
    if "instr" in elt {
      vmem = virtual.vpush(vmem, elt, size: 4, section: section)
      if not vmem.ok { return vmem }
      vmem = vmem.val
    } else if "meta" in elt {
      panic(elt)
    } else if "lab" in elt {
      if elt.lab in refs or elt.lab in new-refs {
        return (ok: false, msg: [#{elt.lab} is already defined])
      }
      let offset = virtual.current-offset(vmem, section: section)
      new-refs.insert(elt.lab, offset)
    } else if "align" in elt {
      vmem = virtual.valign(vmem, size: elt.align, section: section)
      if not vmem.ok { return vmem }
      vmem = vmem.val
    } else if "size" in elt {
      if "val" not in elt {
        for _ in range(elt.size) {
          let ans = values.bits_.of-abstract-hex("??", bytes: 1)(prov-count)
          if not ans.ok { return ans }
          let (bits, new-prov) = ans.val
          prov-count = new-prov
          vmem = virtual.vpush(vmem, section: section, (bits: bits), size: 1)
          if not vmem.ok { return vmem }
          vmem = vmem.val
        }
      } else if "hex" in elt.val {
        let h = values.hex_.cast(elt.val.hex, bytes: elt.size)
        if not h.ok { return h }
        vmem = virtual.vpush(vmem, section: section, (hex: h.val), size: elt.size)
        if not vmem.ok { return vmem }
        vmem = vmem.val
      } else if "int" in elt.val {
        let i = values.int_.cast(elt.val.int, bytes: elt.size)
        if not i.ok { return i }
        vmem = virtual.vpush(vmem, section: section, (int: i.val), size: elt.size)
        if not vmem.ok { return vmem }
        vmem = vmem.val
      } else if "lab" in elt.val {
        if elt.size != 4 {
          return (ok: false, msg: [Addresses can only be words])
        }
        if elt.val.lab not in refs {
          return (ok: false, msg: [#elt.val.lab is not defined yet])
        }
        vmem = virtual.vpush(vmem, (lab: (base: elt.val.lab)), size: 4, section: section)
        if not vmem.ok { return vmem }
        vmem = vmem.val
      } else {
        panic(elt)
      }
    } else {
      panic(elt)
    }
  }
  refs += new-refs
  (ok: true, val: (vmem: vmem, refs: refs, prov-count: prov-count))
}

#let cc(ast) = {
  let vmem = virtual.new(
    (name: <data>, start: calc.pow(2, 15), size: calc.pow(2, 15)),
    (name: <text>, start: calc.pow(2, 16), size: calc.pow(2, 15)),
    (name: <stack>, start: calc.pow(2, 17), size: calc.pow(2, 15)),
  )

  let ans = cc-section(vmem, ast.data, section: <data>, allow-provenance: true)
  if not ans.ok { return ans }
  let (vmem, refs, prov-count) = ans.val

  let ans = cc-section(vmem, ast.text, refs: refs, section: <text>)
  if not ans.ok { return ans }
  let (vmem, refs) = ans.val
  (ok: true, val: (
    regs: (values.algebra.poison,) * 16,
    refs: refs,
    vmem: vmem,
    prov-count: prov-count,
  ))
}

#let print-bit(b) = {
  if b == true {
    $top$
  } else if b == false {
    $bot$
  } else {
    $(?#b)$
  }
}

#let print-bits(byte, width: 8, prov-count: 64) = {
  bitwise.display(byte, size: 4.5mm, cols: width, colors: bitwise.rainbow(prov-count))
}

#let print-val(val, prov-count: 64) = {
  if val == none {
    return
  }
  if "byte" in val {
    print-bits(val.byte, prov-count: prov-count)
  } else if "bits" in val {
    print-bits(val.bits, prov-count: prov-count, width: 32)
  } else if "instr" in val {
    [#val]
  } else if "meta" in val {
    [#val]
  } else if "poison" in val {
    [#emoji.skull.bones]
  } else if "lab" in val {
    [$=$#val.lab]
  } else if "addr" in val {
    [#val]
  } else if "int" in val {
    [#val]
  } else if "hex" in val {
    [#val]
  } else {
    panic(val)
  }
}

#let print-tags(tags) = {
  for (tag, val) in tags {
    [#tag: #val\ ]
  }
}

#let print-reg(reg, idx: [r0], prov-count: 64) = {
  let pp = values.display.with(bits-params: (size: 5mm, colors: values.bits_.rainbow(prov-count)))
  [#idx: #pp(reg)]
}

#let print-regs(regs, prov-count: 64) = {
  for (ri, reg) in regs.enumerate() {
    print-reg(reg, idx: [r#ri], prov-count: prov-count)
    linebreak()
  }
}

#let print-refs(refs) = {
  [#refs]
}

#let print-machine(mch) = {
  print-regs(mch.regs, prov-count: mch.prov-count)
  linebreak()
  virtual.vprint(mch.vmem, printer: values.display.with(bits-params: (size: 5mm, colors: values.bits_.rainbow(mch.prov-count))))
  linebreak()
  print-refs(mch.refs)
  linebreak()
}
