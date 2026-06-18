#import "arm.typ"

#set page(columns: 2)

#arm.run-main(```
  .data
v: .skip 1

  .text
main:
  ldr r0, =v
  ldrb r0, [r0]
  @ print: r0[:8]
  
  ldr r2, .M4
  @ print: r2[:8]
  and r1, r0, r2
  mvn r2
  and r0, r2
  @ print/8: r0[:8], r1[:8]
  lsl r0, #4
  lsr r1, #4
  @ print/8: r0[:8], r1[:8]
  orr r0, r1
  @ print: r0[:8]

  ldr r2, .M2
  @ print: r2[:8]
  and r1, r0, r2
  mvn r2
  and r0, r2
  @ print/8: r0[:8], r1[:8]
  lsl r0, #2
  lsr r1, #2
  @ print/8: r0[:8], r1[:8]
  orr r0, r1
  @ print: r0[:8]

  ldr r2, .M1
  @ print: r2[:8]
  and r1, r0, r2
  mvn r2
  and r0, r2
  @ print/8: r0[:8], r1[:8]
  lsl r0, #1
  lsr r1, #1
  @ print/8: r0[:8], r1[:8]
  orr r0, r1
  @ print: r0[:8]
  
  b exit

.M1: .byte 0xAA
.M2: .byte 0xCC
.M4: .byte 0xF0
```)


