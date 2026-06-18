#let matches-arr(guard, v, rec:auto) = {
  if type(v) != array { return false }
  if guard.len() != v.len() { return false }
  guard.zip(v).all(((g, v),) => rec(g, v))
}

#let matches-func(guard, v, rec:auto) = {
  guard(v)
}

#let matches-dict(guard, v, rec:auto) = {
  if type(v) != dict { return false }
  for (field, subguard) in guard {
    if field not in v { return false }
    if not subguard(v.at(field)) { return false }
  }
  true
}

#let matches-type(guard, v, rec:auto) = {
  type(v) == guard
}

#let matches(guard, v) = {
  if type(guard) == array {
    matches-arr(guard, v, rec:matches)
  } else if type(guard) == type {
    matches-type(guard, v, rec:matches)
  } else if type(guard) == function {
    matches-func(guard, v, rec:matches)
  } else if type(guard) == dictionary {
    matches-dict(guard, v, rec:matches)
  } else {
    guard == v
  }
}

#let match(v) = (..arms,) => {
  for (guard, action) in arms.pos() {
    if matches(guard, v) {
      return action(v)
    }
  }
}

#let __ = _ => true
