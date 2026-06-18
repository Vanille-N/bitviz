#let display(v) = {
  raw("0x" + v)
}

#let cast(v, bytes: 4, lossless: true, loose: false) = {
  if v.len() > bytes * 2 {
    (ok: false, msg: [Lossy cast])
  } else if loose {
    (ok: true, val: v)
  } else {
    (ok: true, val: "0" * (bytes * 2 - v.len()) + v)
  }
}

#let cleave(bytes, v) = {
  let v = cast(v, bytes: bytes * 2)
  if not v.ok { return v }
  // Reversed due to the big endian convention
  let left = v.val.slice(bytes * 2)
  let right = v.val.slice(0, bytes * 2)
  (ok: true, val: (left, right))
}

#let concat(bytes, v1, v2) = {
  let v1 = cast(v1, bytes: bytes)
  if not v1.ok { return v1 }
  let v2 = cast(v2, bytes: bytes)
  if not v2.ok { return v2 }
  // Reversed due to the big endian convention
  (ok: true, val: v2.val + v1.val)
}

#let to-int(h) = (ok: true, val: eval("0x" + h))
