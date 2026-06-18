# bitviz

`bitviz` is a vizualization tool for bitwise operations
on a 32-bit ARM architecture.

It includes
- a parser for ARM built using the parser combinator [`kleene`](https://typst.app/universe/package/kleene/),
- an [abstract interpretation of registers](values/) through
  bitblasting and approximations of bitwise operations,
- an interpreter for a representative subset of ARM instructions,
- tools to produce visual representations of the current state.

**NOTE:** `bitviz` is currently in an unstable state due to an ongoing refactoring.
The code in this repository will hit a `panic`.
In the meantime a [read-only snapshot on typst.app](https://typst.app/project/rLPISyq4ORKuQglW1XlB4c)
of a previous experimental version is available.

