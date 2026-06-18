#import "concrete.typ"

#let new(..secs) = {
  let mem = (:)
  let end = 0
  for (name, start, size) in secs.pos().sorted(key: v => v.start) {
    if start < end {
      panic("Memory regions overlap")
    }
    end = start + size
    mem.insert(str(name), (
      start: start,
      max-size: size,
      mem: concrete.mempty(),
    ))
  }
  mem
}

#let vprint(vmem, printer: auto) = {
  for (name, (mem, start)) in vmem {
    [*#name*\ ]
    concrete.mprint(mem, start-index: start, printer: printer)
  }
}

#let valign(vmem, size: 4, section: auto) = {
  if section == auto { panic("valign cannot infer the section") }
  let (mem, max-size) = vmem.at(str(section))
  let ans = concrete.malign(mem, size: size)
  if not ans.ok { return ans }
  mem = ans.val
  if mem.len() > max-size {
    panic("memory overflow: " + str(section) + " has exceeded size " + str(max-size))
  }
  vmem.at(str(section)).mem = mem
  (ok: true, val: vmem)
}

#let vpush(vmem, val, size: 4, section: auto) = {
  if section == auto { panic("vpush cannot infer the section") }
  let (mem, max-size) = vmem.at(str(section))
  let ans = concrete.mpush(mem, val, size: size)
  if not ans.ok { return ans }
  mem = ans.val
  if mem.len() > max-size {
    panic("memory overflow: " + str(section) + " has exceeded size " + str(max-size))
  }
  vmem.at(str(section)).mem = mem
  (ok: true, val: vmem)
}

#let current-offset(vmem, section: auto) = {
  if section == auto { panic("vpush cannot infer the section") }
  let (mem, start) = vmem.at(str(section))
  start + mem.len()
}

#let infer-section(vmem, addr, section: auto) = {
  if section == auto {
    for (name, (start, max-size)) in vmem {
      if start <= addr and addr < start + max-size {
        return name
      }
    }
  } else {
    str(section)
  }
}

#let vload(vmem, addr, size: 4, section: auto, algebra: auto) = {
  assert(algebra != auto)
  section = infer-section(vmem, addr, section: section)
  let (mem, start) = vmem.at(section)
  if not (start <= addr and addr + size <= start + mem.len()) {
    return (ok: false, msg: [Segmentation fault: load at #addr of size #size is outside the range #start,#{start + mem.len()} for #section])
  }
  concrete.mload(mem, addr - start, size: size, algebra: algebra)
}

#let vstore(vmem, addr, val, size: 4, section: auto, algebra: auto) = {
  assert(algebra != auto)
  section = infer-section(vmem, addr, section: section)
  let (mem, start) = vmem.at(section)
  if not (start <= addr and addr + size <= start + mem.len()) {
    return (ok: false, msg: [Segmentation fault: store at #addr of size #size is outside the range #start,#{start + mem.len()} for #section])
  }
  mem = concrete.mstore(mem, addr - start, val, size: size, algebra: algebra)
  if not mem.ok { return mem }
  vmem.at(section).mem = mem.val
  (ok: true, val: vmem)
}
