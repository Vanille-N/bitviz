#import "arm.typ"

#arm.run-main(```
  .data
A: .skip 1
B: .byte 0xAA
  .balign 4
C: .word 0xFFFF

  .text
main:
  //ldr r0, =A
  //ldr r1, .L_A
  //ldrb r2, [r0]
  //ldrh r3, [r0]
  //ldr r4, [r0]
  //ldrh r5, [r1, #0x4]
  //mov r7, #243
  //mov r6, #0xffae
  //mov r8, r2
  //mov r9, r0
  //movb r10, r0
  //movh r9, r4
  //movb r10, r4
  //mvn r11, r10
  //mvn r12, r5
  //mvn r12
  //mvn r13
  // add, sub, lsl, lsr, eor, orr, and, b
  b exit

.L_A: .word A
```)

