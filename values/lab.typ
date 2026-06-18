#let display(v) = {
  if v.at("offset", default: 0) == 0 {
    [<#{v.base}>]
  } else {
    [<#{v.base}+#{v.offset}>]
  }
}

#let addi(v, i) = {
  v.offset = v.at("offset", default: 0) + i
  (ok: true, val: v)
}

#let to-int(v, refs: (:)) = {
  if v.base in refs {
    (ok: true, val: refs.at(v.base) + v.at("offset", default: 0))
  } else {
    (ok: false, msg: [Undefined reference <#{v.base}>])
  }
}

