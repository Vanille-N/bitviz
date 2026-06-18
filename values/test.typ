= Bits

#import "bits.typ"

#let b1 = bits.of-int(42, bytes: 1).val
#bits.display(b1)

#let b2 = bits.of-hex("f1", bytes: 1).val
#bits.display(b2)

#let b3 = bits.of-abstract-hex("f?ae", bytes: 2)(1).val.at(0)
#bits.display(b3)

#bits.display(bits.bnot(b1).val)

#bits.display(bits.band(b1, b2).val)

#bits.display(bits.bor(b1, b2).val)

#bits.display(bits.bxor(b1, b2).val)

#bits.display(bits.lsr(b1, 1).val)

#bits.display(bits.lsl(b1, 1).val)

#let (left, right) = bits.cleave(1, b3).val
#bits.display(left)
#bits.display(right)

#let b4 = bits.concat(1, left, right).val
#bits.display(b4)

= Hex

#import "hex.typ"

#hex.display("4fae")

#hex.display(hex.cast("4f", bytes: 2).val)

#let (left, right) = hex.cleave(1, "4fa").val
#hex.display(left)
#hex.display(right)

#let h = hex.concat(1, left, right).val
#hex.display(h)

= Int

#import "int.typ"

#let i = int.concat(1, 2, 1).val
#int.display(i)

#let (left, right) = int.cleave(1, 258).val
#int.display(left)
#int.display(right)

= Poison

#import "poison.typ"

#poison.display(())

= Lab

#import "lab.typ"

#lab.display((base: "main"))
#lab.display((base: "main", offset: 8))

#lab.display(lab.addi((base: "main"), 4).val)

#lab.to-int((base: "main", offset: 4), refs: (main: 1000)).val


= Values

#import "values.typ"

#values.display((lab: (base: "main", offset: 4)))

#values.display((addr: 1004))

#values.display((int: 123))

#values.display((hex: "dead"))

#values.display((bits: bits.of-int(42, bytes: 2).val))

#values.display((instr: (exec: "ldr")))

#values.display((meta: (exec: "print")))

#values.display((poison: ()))

