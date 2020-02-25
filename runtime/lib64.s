  .global atoi
  .global print_int
  .global println
  .global print
  .global print_a1
  .global print_char
  .global strlen
  .global ltoa
  .global print_string

atoi:
addi	sp,sp,-48
sd	s0,40(sp)
addi	s0,sp,48
sd	a0,-40(s0)
sw	zero,-20(s0)
j	.L2

.L3:
lw	a4,-20(s0)
mv	a5,a4
slliw	a5,a5,0x2
addw	a5,a5,a4
slliw	a5,a5,0x1
sext.w	a4,a5
lbu	a5,-21(s0)
sext.w	a5,a5
addw	a5,a5,a4
sext.w	a5,a5
addiw	a5,a5,-48
sw	a5,-20(s0)

.L2:
ld	a5,-40(s0)
addi	a4,a5,1
sd	a4,-40(s0)
lbu	a5,0(a5)
sb	a5,-21(s0)
lbu	a5,-21(s0)
andi	a5,a5,255
bnez	a5, .L3
lw	a5,-20(s0)
mv	a0,a5
ld	s0,40(sp)
addi	sp,sp,48
ret
print_int:
addi sp, sp, -8
sd ra,0(sp)
lui a1, %hi(buf)
addi a1, a1, %lo(buf)
jal ra, ltoa
mv a1, a0
jal ra, print_a1
ld ra, 0(sp)
addi sp, sp, 8
jr ra
print_a1:
addi sp, sp, -8
sd ra,0(sp)
mv a0, a1
jal ra, strlen
ld ra, 0(sp)
addi sp, sp, 8
mv a2, a0
li a0, 1
li a3, 0
li a4, 0
li a5, 0
li a6, 0
li a7, 64
ecall
jr ra
print_char:
addi sp, sp, -8
sd ra, 0(sp)
la a1, buf
sb a0, 0(a1)
sb zero, 1(a1)
jal ra, print_a1
ld ra, 0(sp)
addi sp, sp, 8
jr ra

print:
addi    sp,sp,-32
sd      ra,24(sp)
sd      s0,16(sp)
addi    s0,sp,32
sd      a0,-24(s0)
ld      a0,-24(s0)
jal     ra,print_int
jal     ra,println
nop
ld      ra,24(sp)
ld      s0,16(sp)
addi    sp,sp,32
ret
 strlen:
addi	sp,sp,-48
sd	s0,40(sp)
addi	s0,sp,48
sd	a0,-40(s0)
sw	zero,-20(s0)
j	.L6

.L7:
lw	a5,-20(s0)
addiw	a5,a5,1
sw	a5,-20(s0)

.L6:
ld	a5,-40(s0)
addi	a4,a5,1
sd	a4,-40(s0)
lbu	a5,0(a5)
bnez	a5,.L7
lw	a5,-20(s0)
mv	a0,a5
ld	s0,40(sp)
addi	sp,sp,48
ret
ltoa:
mv a5,a0
addi a3,a1,20
sb zero,20(a1)
li a1,0
bltz a0,.L27

.L22:
li a2,10
j .L23

.L27:
neg a5,a0
li a1,1
j .L22

.L26:
mv a3,a0

.L23:
rem a4,a5,a2
addiw a4,a4,48
sb a4,-1(a3)
addi a0,a3,-1
div a5,a5,a2
bgtz a5,.L26
beqz a1,.L21
li a5,45
sb a5,-1(a0)
addi a0,a3,-2

.L21:
ret
println:
mv t6, ra
lui a1, %hi(nl)
addi a1, a1, %lo(nl)
jal ra, print_a1
jr t6
print_string:
addi sp, sp, -8
sd ra, 0(sp)
addi sp, sp, -8
sd s0, 0(sp)
addi sp, sp, -64
sd a0, 56(sp)
sd a1, 48(sp)
sd a2, 40(sp)
sd a3, 32(sp)
sd a4, 24(sp)
sd a5, 16(sp)
sd a6, 8(sp)
sd a7, 0(sp)
mv a1, a0
jal print_a1
ld a0, 56(sp)
ld a1, 48(sp)
ld a2, 40(sp)
ld a3, 32(sp)
ld a4, 24(sp)
ld a5, 16(sp)
ld a6, 8(sp)
ld a7, 0(sp)
addi sp, sp, 64
ld s0, 0(sp)
addi sp, sp, 8
ld ra, 0(sp)
addi sp, sp, 8
jr ra

	.section .rodata
	nl:
	.string "\n"

	.section .data
	buf:
	.string "XXXXXXXXXXXXXXXXXXXX"
