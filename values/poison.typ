#let default = ()

#let display(()) = emoji.skull.bones

#let of-any(_) = ()

#let cleave(_, _) = (ok: true, val: ((), ()))

#let concat(_, _, _) = (ok: true, val: ())

#let to-bits((), bytes: 4) = (ok: true, val: (none,) * bytes * 8)
