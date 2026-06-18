typstc cmd file fmt="pdf":
  typst {{ cmd }} --root=. --font-path=. {{ file }} {{ replace(file, ".typ", "." + fmt) }}

sym: (typstc "watch" "main-sym.typ")

rot: (typstc "watch" "main-rot.typ")

syntax: (typstc "watch" "syntax.typ")

memory: (typstc "watch" "memory/concrete.typ")

values: (typstc "watch" "values/values.typ")

test: (typstc "watch" "main-test.typ")
