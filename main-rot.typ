#import "arm.typ"

//#set page(columns: 2)

#arm.run-main(```
  .data
VALUE: .skip 8

 .text
main:
  ldr r2, =VALUE
  ldr r5, [r2]
  ldr r6, [r2, #4]
  @ print/8: r5, r6

  ldr r2, .LD_mask_A
  @ print/8: r2
  // r3 = (r5 ^ (r5 >> 7)) & mask
  lsr r3, r5, #7
  eor r3, r5
  and r3, r2
  // r5 = r5 ^ r3 ^ (r3 << 7)
  eor r5, r3
  lsl r3, #7
  eor r5, r3
  // r3 = (r6 ^ (r6 >> 7)) & mask
  lsr r3, r6, #7
  eor r3, r6
  and r3, r2
  // r6 = r6 ^ r3 ^ (r3 << 7)
  eor r6, r3
  lsl r3, #7
  eor r6, r3
  @ print/8: r5, r6

  // Using mask 0x0000CCCC, transpose 4-bit blocks in r5 and r6
  ldr r2, .LD_mask_C
  @ print/8: r2
  // r3 = (r5 ^ (r5 >> 14)) & mask
  lsr r3, r5, #14
  eor r3, r5
  and r3, r2
  // r5 = r5 ^ r3 ^ (r3 << 14)
  eor r5, r3
  lsl r3, #14
  eor r5, r3
  // r3 = (r6 ^ (r6 >> 14)) & mask
  lsr r3, r6, #14
  eor r3, r6
  and r3, r2
  // r6 = r6 ^ r3 ^ (r3 << 14)
  eor r6, r3
  lsl r3, #14
  eor r6, r3
  @ print/8: r5, r6

  // Final small swap with masks 0xF0F0F0F0 and 0x0F0F0F0F
  ldr r2, .LD_mask_F
  @ print/8: r2
  // r3 = r5
  mov r3, r5
  // r5 = (r5 & mask) | ((r6 >> 4) & ~mask)
  and r5, r3, r2
  lsl r4, r6, #4
  mvn r2, r2
  and r4, r2
  orr r5, r4
  // r6 = ((r3 << 4) & mask) | (r6 & ~mask)
  and r6, r2
  lsr r4, r3, #4
  mvn r2, r2
  and r4, r2
  orr r6, r4
  @ print/8: r5, r6
  
  b exit

.LD_mask_A: .word 0x00AA00AA
.LD_mask_C: .word 0x0000CCCC
.LD_mask_F: .word 0x0F0F0F0F
```)
