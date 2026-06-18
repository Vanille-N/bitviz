#import "bit.typ"

#let val-of-digit(dig) = {
  if "0" <= dig and dig <= "9" {
    int(dig)
  } else if "a" <= dig and dig <= "f" {
    dig.to-unicode() - "a".to-unicode() + 10
  } else if "A" <= dig and dig <= "F" {
    dig.to-unicode() - "A".to-unicode() + 10
  } else if dig == "?" {
    "?"
  } else {
    none
  }
}

#let cast(val, bits: auto, bytes: auto, lossless: true, loose: false) = {
  if bits != auto and bytes != auto {
    panic("Specify only one of 'bits' or 'bytes'")
  }
  let nbits = if bits != auto { bits } else if bytes != auto { bytes * 8 } else { 32 }
  if val.len() > nbits {
    let extra = val.slice(nbits)
    if lossless and extra.any(b => b != false) {
      return (ok: false, msg: [Lossy cast])
    }
    (ok: true, val: val.slice(0, nbits))
  } else if loose {
    (ok: true, val: val)
  } else {
    (ok: true, val: val + (false,) * (nbits - val.len()))
  }
}

#let of-abstract(val, base-bits: 4, bytes: 4) = prov => {
  let digs = val.clusters().map(d => (d, val-of-digit(d)))
  for (d, v) in digs {
    if type(v) == int {
      if v >= calc.pow(2, base-bits) {
        panic(d + " is too large for " + str(base-bits) + " bits")
      }
    } else if v == "?" {
    } else {
      panic(d + " cannot be interpreted as a digit")
    }
  }
  let vals = digs.map(((_,v),) => v)
  let out = ()
  for val in vals.rev() {
    if val == "?" {
      if prov == none {
        return (ok: false, msg: [Does not accept wildcards])
      }
      for _ in range(base-bits) {
        out.push(bit.var(prov))
        prov += 1
      }
    } else {
      for _ in range(base-bits) {
        if calc.rem(val, 2) == 1 {
          out.push(bit.top)
        } else {
          out.push(bit.bot)
        }
        val = calc.div-euclid(val, 2)
      }
    }
  }
  let out = cast(out, bytes: bytes)
  if not out.ok { return out }
  (ok: true, val: (out.val, prov))
}

#let of-abstract-hex = of-abstract
#let of-abstract-bin = of-abstract.with(base-bits: 1)

#let of-hex(h, bytes: 4) = {
  let out = of-abstract-hex(h, bytes: bytes)(none)
  if not out.ok { return out }
  let (b, _) = out.val
  (ok: true, val: b)
}

#let to-int(bits) = {
  let n = 0
  for b in bits.rev() {
    if b == true {
      n = n * 2 + 1
    } else if b == false {
      n = n * 2
    } else {
      return none
    }
  }
  n
}

#let of-int(n, bytes: 4) = {
  let bits = ()
  while n > 0 {
    if calc.rem(n, 2) == 1 {
      bits.push(bit.top)
    } else {
      bits.push(bit.bot)
    }
    n = int(n / 2)
  }
  cast(bits, bytes: bytes)
}

#let band(lhs, rhs) = {
  if lhs.len() != rhs.len() {
    (ok: false, msg: [`and` on values of different size])
  } else {
    (ok: true, val: lhs.zip(rhs).map(((l,r),) => bit.band(l, r)))
  }
}

#let bor(lhs, rhs) = {
  if lhs.len() != rhs.len() {
    (ok: false, msg: [`or` on values of different size])
  } else {
    (ok: true, val: lhs.zip(rhs).map(((l,r),) => bit.bor(l, r)))
  }
}

#let bnot(val) = {
  (ok: true, val: val.map(v => bit.bnot(v)))
}

#let lsr(val, amount) = {
  let ans = val.slice(amount) + (false,) * amount
  (ok: true, val: ans)
}

#let lsl(val, amount) = {
  let ans = (false,) * amount + val.slice(0, -amount)
  (ok: true, val: ans)
}

#let bxor(lhs, rhs) = {
  if lhs.len() != rhs.len() {
    (ok: false, msg: [`xor` on values of different size])
  } else {
    (ok: true, val: lhs.zip(rhs).map(((l,r),) => bit.bxor(l,r)))
  }
}

#let split-by-chunks(val, bits: 8) = {
  let val = val
  let chunks = ()
  while val.len() > 0 {
    if val.len() >= bits {
      chunks.push(val.slice(0, bits))
      val = val.slice(bits)
    } else {
      chunks.push(cast(val, bits: bits).val)
      val = ()
    }
  }
  chunks
}

#let cleave(bytes, val) = {
  let val = cast(val, bytes: bytes * 2)
  if not val.ok { return val }
  let ans = split-by-chunks(val.val, bits: bytes * 8)
  (ok: true, val: ans)
}

#let concat(size, v1, v2) = {
  let v1 = cast(v1, bytes: size)
  if not v1.ok { return v1 }
  let v2 = cast(v2, bytes: size)
  if not v2.ok { return v2 }
  (ok: true, val: v1.val + v2.val)
}

#let display(size: 1cm, cols: 8, colors: _ => gray, val) = {
  box(table(columns: cols, inset: 0pt, stroke: none,
    ..for (idx, b) in val.enumerate() {
      let v = bit.eval(b)
      let (fill, vdisplay) = if v == false {
        (black, text(fill: white)[$bot$])
      } else if v == true {
        (white, $top$)
      } else if v == none {
        (gray, [?])
      } else {
        (colors(v), [#v])
      }
      (box(width: size, height: size, fill: fill, stroke: black)[#align(center + horizon)[#vdisplay]],)
    }
  ))
}

#let rainbow(num) = i => {
  color.hsl(250deg * (i - 1) / (num - 1), 100%, 50%)
}

