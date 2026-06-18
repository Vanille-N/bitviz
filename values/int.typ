#let display(i) = str(i)

#let cast(i, bytes: 4, lossless: true, loose: true) = {
  let cutoff = calc.pow(256, bytes)
  if i < 0 {
    (ok: false, msg: [Can't handle negative integers])
  } else if i >= cutoff {
    if lossless {
      (ok: false, msg: [Integer can't fit on #bytes bytes])
    } else {
      (ok: true, val: calc.rem(i, cutoff))
    }
  } else {
    (ok: true, val: i)
  }
}

#let concat(bytes, v1, v2) = {
  (ok: true, val: v1 + v2 * 256)
}

#let cleave(bytes, v) = {
  let cutoff = calc.pow(256, bytes)
  let left = calc.rem(v, cutoff)
  let right = calc.div-euclid(v, cutoff)
  (ok: true, val: (left, right))
}

