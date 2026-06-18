// Abstraction domain: bool + var + none
#import "/libs/match.typ": *

#let var(prov) = prov

#let bot = false

#let top = true

#let eval(v) = match(v)(
  (true, _ => true),
  (false, _ => false),
  (int, v => v),
  (__, _ => none),
)

#let band(l, r) = match((l, r))(
  ((true, __), ((_,r),) => r),
  ((__, true), ((l,_),) => l),
  ((false, __), _ => false),
  ((__, false), _ => false),
  ((int, int), ((l,r),) => if l == r { l } else if l == -r { false } else { none }),
  (__, _ => none),
)

#let bor(l, r) = match((l, r))(
  ((false, __), ((_,r),) => r),
  ((__, false), ((l,_),) => l),
  ((true, __), _ => true),
  ((__, true), _ => true),
  ((int, int), ((l,r),) => if l == r { l } else if l == -r { true } else { none }),
  (__, _ => none),
)

#let bnot(v) = match(v)(
  (true, _ => false),
  (false, _ => true),
  (int, v => -v),
  (array, a => {a.last() *= -1; a}),
  (__, _ => none),
)

#let remove(arr, v) = {
  let idx = arr.position(x => x == v)
  let _ = arr.remove(idx)
  if arr.len() == 1 {
    arr.last()
  } else {
    arr
  }
}

#let bxor(l, r) = match((l,r))(
  ((false, __), ((_,r),) => r),
  ((__, false), ((l,_),) => l),
  ((true, __), ((_,r),) => bnot(r)),
  ((__, true), ((l,_),) => bnot(l)),
  ((int, int), ((l,r),) => if l == r { false } else if l == -r { true } else { (l,r) }),
  ((int, array), ((l,x),) => {
    if l in x {
      remove(x, l)
    } else if -l in x {
      remove(x, -l)
      x = bnot(x)
    } else {
      x + (l,)
    }
  }),
  ((array, int), ((x,r),) => {
    if r in x {
      remove(x, r)
    } else if -r in x {
      remove(x, -r)
      x = bnot(x)
    } else {
      x + (r,)
    }
  }),
  ((array, array), ((x,y),) => {
    for r in y {
      if r in x {
        x = remove(x, r)
      } else if -r in x {
        x = remove(x, -r)
        x = bnot(x)
      } else {
        x = x + (r,)
      }
      if type(x) == int {
        x = (x,)
      }
    }
    if x.len() == 1 {
      x = x.last()
    }
    x
  }),
)

#bxor(2, 1)
#bxor(2, 3)
#{(bxor(bxor(-2, 1), bxor(2, 3)),)}
