#import "syntax.typ"
#import "assembler.typ"
#import "interpreter.typ"

#let run-main(code) = {
  let ans = syntax.arm(code)
  if not ans.ok { return ans.msg }
  let machine = assembler.cc(ans.val)
  if not machine.ok { return machine }
  let machine = machine.val
  assembler.print-machine(machine)
  interpreter.run(machine, <main>)
}
