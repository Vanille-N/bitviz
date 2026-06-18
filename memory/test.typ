#import "concrete.typ"
#import "virtual.typ"

#let algebra = (
  cleave: (size, elt) => (ok: true, val: (elt.slice(0, size), elt.slice(size))),
  concat: (size, e1, e2) => (ok: true, val: e1 + e2),
  poison: (),
  display: elt => {
    if elt == () {
      $emptyset$
    } else {
      raw(elt)
    }
  }
)

= Concrete

#{
  let a = concrete.mempty()
  for e in (
    "abcd", "ef", "gh", "i", "j", "k", "l",
    "m", "n", "op", "qr", "s", "t", "x", "yz",
  ) {
    a = concrete.mpush(a, e, size: e.len())
  }
  [#a\ ]
  for step in (1, 2, 4) {
    for i in range(int(a.len() / step)) {
      let v = concrete.mload(a, step * i, size: step, algebra: algebra)
      assert(v.ok)
      (algebra.display)(v.val)
      [, ]
    }
    linebreak()
  }

  pagebreak()

  concrete.mprint(a, printer: algebra.display)
  linebreak()

  a = concrete.mstore(a, 0, "AB", size: 2, algebra: algebra).val
  a = concrete.mstore(a, 5, "F", size: 1, algebra: algebra).val
  a = concrete.mstore(a, 6, "G", size: 1, algebra: algebra).val
  a = concrete.mstore(a, 12, "MNOP", size: 4, algebra: algebra).val
  a = concrete.mstore(a, 4, "EF", size: 2, algebra: algebra).val
  a = concrete.mstore(a, 4, "EFGH", size: 4, algebra: algebra).val
  concrete.mprint(a, printer: algebra.display)
  linebreak()
}

= Virtual

#{
  let a = virtual.new(
    (name: <data>, start: calc.pow(2, 15), size: calc.pow(2, 15)),
    (name: <text>, start: calc.pow(2, 16), size: calc.pow(2, 15)),
    (name: <stack>, start: calc.pow(2, 17), size: calc.pow(2, 15)),
  )
  [#virtual.current-offset(a, section: <data>)\ ]
  a = virtual.vpush(a, section: <data>, "abcd", size: 4)
  [#virtual.current-offset(a, section: <data>)\ ]
  a = virtual.vpush(a, section: <data>, "ef", size: 2)
  [#virtual.current-offset(a, section: <data>)\ ]
  a = virtual.vpush(a, section: <data>, "g", size: 1)
  [#virtual.current-offset(a, section: <data>)\ ]
  a = virtual.vpush(a, section: <data>, "h", size: 1)
  [#virtual.current-offset(a, section: <text>)\ ]
  a = virtual.vpush(a, section: <text>, "ij", size: 2)
  [#virtual.current-offset(a, section: <text>)\ ]
  a = virtual.vpush(a, section: <text>, "kl", size: 2)

  virtual.vprint(a, printer: algebra.display)

  let v = virtual.vload(a, 32773, size: 1, algebra: algebra)
  assert(v.ok)
  (algebra.display)(v.val)
  linebreak()

  let v = virtual.vload(a, 65536, size: 4, algebra: algebra)
  assert(v.ok)
  (algebra.display)(v.val)
  linebreak()

  let v = virtual.vload(a, 32773, section: <text>, size: 1, algebra: algebra)
  assert(not v.ok)
  text(fill: red)[#{v.msg}]
  linebreak()

  a = virtual.vstore(a, 32773, "F", size: 1, algebra: algebra).val
  a = virtual.vstore(a, 65536, "IJKL", size: 4, algebra: algebra).val
  virtual.vprint(a, printer: algebra.display)
}
