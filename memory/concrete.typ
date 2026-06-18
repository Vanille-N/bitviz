#let deltas = {
  let max-size = 4
  let deltas = (1,)
  let next = 2
  while next <= max-size {
    deltas = deltas + (next,) + deltas
    next *= 2
  }
  deltas
}

#let mempty() = {
  ()
}

#let malign(arr, size: 4) = {
  let rem = calc.rem(arr.len(), size)
  for _ in range(size - rem) {
    arr.push((size: 1))
  }
  (ok: true, val: arr)
}

#let mpush(arr, v, size: 4) = {
  let rem = calc.rem(arr.len(), size)
  if rem != 0 {
    return (ok: false, msg: [Bad alignment. Insert a `.balign `#raw(str(rem))])
  }
  arr.push((size: size, val: v))
  for i in deltas.slice(0, size - 1) {
    arr.push((size: i, delta: -i))
  }
  (ok: true, val: arr)
}

#let mload(arr, idx, size: 4, algebra: auto) = {
  assert(algebra != auto)
  if calc.rem(idx, size) != 0 { return (ok: false, msg: [Unaligned load]) }
  let sz = 0
  let elts = ()
  while sz < size {
    let elt = arr.at(idx + sz)
    elts.push((idx + sz, calc.min(elt.size, size)))
    sz += elt.size
  }
  for i in range(elts.len()) {
    let (idx, size) = elts.at(i)
    let resolve(idx, size) = {
      let elt = arr.at(idx)
      if elt.size > size {
        let rec = resolve(idx, size * 2)
        if not rec.ok { return rec }
        let parent = rec.val
        let parts = (algebra.cleave)(size, parent)
        if not parts.ok { return parts }
        let (left, right) = parts.val
        (ok: true, val: left)
      } else if "delta" in elt {
        let rec = resolve(idx + elt.delta, size * 2)
        if not rec.ok { return rec }
        let parent = rec.val
        let parts = (algebra.cleave)(size, parent)
        if not parts.ok { return parts }
        let (left, right) = parts.val
        (ok: true, val: right)
      } else if "val" in elt {
        (ok: true, val: elt.val)
      } else {
        (ok: true, val: none) // poison value
      }
    }
    let rec = resolve(idx, size)
    if not rec.ok { return rec }
    elts.at(i) = (size: size, val: rec.val)
  }
  for (i, elt) in elts.enumerate() {
    if elt.val == none {
      elts.at(i).val = algebra.poison
    }
  }
  while elts.len() > 1 {
    let new = ()
    let i = 0
    while i < elts.len() {
      if i < elts.len() - 1 and elts.at(i).size == elts.at(i + 1).size {
        let l = elts.at(i)
        let r = elts.at(i + 1)
        let val = (algebra.concat)(l.size, l.val, r.val)
        if not val.ok { return val }
        new.push((size: l.size * 2, val: val.val))
        i += 2
      } else {
        new.push(elts.at(i))
        i += 1
      }
    }
    elts = new
  }
  assert(elts.len() == 1)
  let (elt,) = elts
  (ok: true, val: elt.val)
}

#let mstore(arr, idx, val, size: 4, algebra: (:)) = {
  if calc.rem(idx, size) != 0 { return (ok: false, msg: [Unaligned store]) }
  let sz = 0
  let elts = ()
  while sz < size {
    let elt = arr.at(idx + sz)
    elts.push(idx + sz)
    sz += elt.size
  }
  let concretize(arr, idx, size) = {
    let elt = arr.at(idx)
    if elt.size > size {
      let rec = concretize(arr, idx, size * 2)
      if not rec.ok { return rec }
      arr = rec.val
      let elt = arr.at(idx)
      let parts = (algebra.cleave)(size, arr.at(idx).val)
      if not parts.ok { return parts }
      let (left, right) = parts.val
      arr.at(idx) = (size: size, val: left)
      arr.at(idx + size) = (size: size, val: right)
      (ok: true, val: arr)
    } else if "delta" in elt {
      let rec = concretize(arr, idx + elt.delta, size * 2)
      if not rec.ok { return rec }
      arr = rec.val
      let elt = arr.at(idx)
      let parts = (algebra.cleave)(size, arr.at(idx + elt.delta).val)
      if not parts.ok { return parts }
      let (left, right) = parts.val
      arr.at(idx + elt.delta) = (size: size, val: left)
      arr.at(idx) = (size: size, val: right)
      (ok: true, val: arr)
    } else {
      (ok: true, val: arr)
    }
  }
  for idx in elts {
    let rec = concretize(arr, idx, size)
    if not rec.ok { return rec }
    arr = rec.val
  }
  arr.at(idx) = (size: size, val: val)
  for (i, delta) in deltas.slice(0, size - 1).enumerate() {
    arr.at(idx + i + 1) = (size: delta, delta: -delta)
  }
  (ok: true, val: arr)
}

#let mprint(mem, start-index: 0, printer: auto) = {
  let pad-len = " " * str(start-index + mem.len()).len()
  let padded(i) = {
    let s = str(start-index + i)
    pad-len.slice(s.len()) + s
  }
  for (idx, val) in mem.enumerate() {
    if "val" in val {
      let sz = (
        "1": "byte",
        "2": "half",
        "4": "word",
      ).at(str(val.size))
      let line = if printer == auto { [#val.val] } else { printer(val.val) }
      if line != none {
        {
          set text(fill: gray.darken(30%))
          `[`; raw(sz); ` @ `; raw(padded(idx)); `] `
        }
        line
        linebreak()
      }
    }
  }
}

