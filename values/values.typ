#import "bits.typ" as bits_
#import "hex.typ" as hex_
#import "int.typ" as int_
#import "poison.typ" as poison_
#import "lab.typ" as lab_

// All the possible types of values:
// - (lab: (base: "foo", offset: i)) is the address of "foo" in the source, offset by the integer i.
// - (addr: 123) is the memory address 123
// - (int: 123) is the integer 123
// - (hex: "1f2") is the hexadecimal integer 0x1f2
// - (bits: (_,_,...,_)) is a sequence of abstract bits
// - (instr: ..) is an executable instruction. This value is opaque.
// - (meta: ..) is a print statement. This value is opaque.
// - (poison: ()) is a poison value
//
// For each, we define a number of conversions
// and when applicable the relevant algebra operations:
// - cleave(size, v) -> (v1, v2)
// - concat(size, v1, v2) -> v
// - band, bor, bxor, bnot, ...

#let typeof(v) = v.keys().at(0)

#let cases(v) = (..arms) => {
  let arms = arms.named()
  let val-key = typeof(v)
  arms.at(val-key)(())
}

#let rewrap1(lab) = v => {
  if not v.ok { return v }
  let val = v.val
  (ok: true, val: (""+lab: val))
}

#let rewrap2(lab) = v => {
  if not v.ok { return v }
  let (v1, v2) = v.val
  (ok: true, val: ((""+lab: v1), (""+lab: v2)))
}

#let display(v, bits-params: (:)) = cases(v)(
  lab: _ => lab_.display(v.lab),
  int: _ => int_.display(v.int),
  hex: _ => hex_.display(v.hex),
  bits: _ => bits_.display(v.bits, ..bits-params),
  instr: _ => [#{v}],
  meta: _ => [#{v}],
  poison: _ => poison_.display(v.poison),
)

#let is-lower(t1, t2) = {
  (
    bits: () => true,
    poison: () => t2 != "bits",
  ).at(t1)()
}

#let downcast(v, bytes: 4) = {
  cases(v)(
    hex: _ => rewrap1("bits")(bits_.of-hex(v.hex, bytes: bytes)),
    poison: _ => rewrap1("bits")(poison_.to-bits(v, bytes: bytes)),
  )
}

#let cleave(bytes, v) = (
  cases(v)(
    hex: _ => rewrap2("hex")(hex_.cleave(bytes, v.hex)),
  )
)

#let concat(bytes, v1, v2) = {
  while true {
    if typeof(v1) == typeof(v2) {
      break
    } else if is-lower(typeof(v1), typeof(v2)) {
      v2 = downcast(v2, bytes: bytes)
      if not v2.ok { return v2 }
      v2 = v2.val
    } else if is-lower(typeof(v2), typeof(v1)) {
      v1 = downcast(v1, bytes: bytes)
      if not v1.ok { return v1 }
      v1 = v1.val
    } else {
      v1 = downcast(v1, bytes: bytes)
      v2 = downcast(v2, bytes: bytes)
      if not v1.ok { return v1 }
      v1 = v1.val
      if not v2.ok { return v2 }
      v2 = v2.val
    }
  }
  cases(v1)(
    bits: _ => rewrap1("bits")(bits_.concat(bytes, v1.bits, v2.bits)),
    poison: _ => rewrap1("poison")(poison_.concat(bytes, v1.poison, v2.poison)),
  )
}

#let addi(v, i) = cases(v)(
  lab: _ => rewrap1("lab")(lab_.addi(v.lab, i)),
)

#let to-int(v, refs: (:)) = cases(v)(
  lab: _ => lab_.to-int(v.lab, refs: refs),
  hex: _ => hex_.to-int(v.hex),
)

#let to-bits(v, size: 4, refs: (:)) = cases(v)(
  bits: _ => (ok: true, val: v),
  hex: _ => rewrap1("bits")(bits_.of-hex(v.hex)),
  poison: _ => rewrap1("bits")(poison_.to-bits((), bytes: size)),
)

#let bnot(v, size: 4, refs: (:)) = {
  let bits = to-bits(v, size: size, refs: refs)
  if not bits.ok { return bits }
  let neg = bits_.bnot(bits.val.bits)
  rewrap1("bits")(neg)
}

#let cast(v, size: 4, refs: (:), lossless: true, loose: false) = cases(v)(
  int: _ => rewrap1("int")(int_.cast(v.int, bytes: size, lossless: lossless, loose: loose)),
  hex: _ => rewrap1("hex")(hex_.cast(v.hex, bytes: size, lossless: lossless, loose: loose)),
  bits: _ => rewrap1("bits")(bits_.cast(v.bits, bytes: size, lossless: lossless, loose: loose)),
  poison: _ => (ok: true, val: v),
  lab: _ => {
    if size == 4 {
      (ok: true, val: v)
    } else {
      let i = lab_.to-int(v.lab, refs: refs)
      if not i.ok { return i }
      cast((int: i.val), size: size, refs: refs, lossless: lossless, loose: loose)
    }
  }
)

#let algebra = (
  cleave: cleave,
  concat: concat,
  poison: (poison: poison_.default),
  addi: addi,
  to-int: to-int,
)

