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
sw	s0,44(sp)
addi	s0,sp,48
sw	a0,-36(s0)
sw	zero,-20(s0)
j	.L2

.L3:
lw	a5,-20(s0)
slli	a4,a5,0x3
lw	a5,-20(s0)
slli	a5,a5,0x1
add	a5,a4,a5
lw	a4,-36(s0)
lbu	a4,0(a4)
add	a5,a5,a4
addi	a5,a5,-48
sw	a5,-20(s0)
lw	a5,-36(s0)
addi	a5,a5,1
sw	a5,-36(s0)

.L2:
lw	a5,-36(s0)
lbu	a5,0(a5)
bnez	a5,.L3
lw	a5,-20(s0)
mv	a0,a5
lw	s0,44(sp)
addi	sp,sp,48
ret

print_int:
addi sp, sp, -8
sw ra,0(sp)
lui a1, %hi(buf)
addi a1, a1, %lo(buf)
jal ra, itoa
mv a1, a0
jal ra, print_a1
lw ra, 0(sp)
addi sp, sp, 8
jr ra
print_a1:
addi sp, sp, -8
sw ra,0(sp)
mv a0, a1
jal ra, strlen
lw ra, 0(sp)
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
sw ra, 0(sp)
la a1, buf
sb a0, 0(a1)
sb zero, 1(a1)
jal ra, print_a1
lw ra, 0(sp)
addi sp, sp, 8
jr ra

print:
addi    sp,sp,-32
sw      ra,24(sp)
sw      s0,16(sp)
addi    s0,sp,32
sw      a0,-24(s0)
lw      a0,-24(s0)
jal     ra,print_int
jal     ra,println
nop
lw      ra,24(sp)
lw      s0,16(sp)
addi    sp,sp,32
ret
strlen:
addi	sp,sp,-48
sw	s0,44(sp)
addi	s0,sp,48
sw	a0,-36(s0)
sw	zero,-20(s0)
j	.strlenL2
.strlenL3:
lw	a5,-20(s0)
addi	a5,a5,1
sw	a5,-20(s0)
.strlenL2:
lw	a5,-36(s0)
addi	a4,a5,1
sw	a4,-36(s0)
lbu	a5,0(a5)
bnez	a5,.strlenL3
lw	a5,-20(s0)
mv	a0,a5
lw	s0,44(sp)
addi	sp,sp,48
jr	ra
itoa:
addi	sp,sp,-48
sw	s0,44(sp)
addi	s0,sp,48
sw	a0,-36(s0)
sw	a1,-40(s0)
lw	a5,-40(s0)
addi	a5,a5,10
sw	a5,-20(s0)
lw	a5,-20(s0)
sb	zero,0(a5)
sb	zero,-21(s0)
lw	a5,-36(s0)
bgez	a5,.itoaL2
li	a5,1
sb	a5,-21(s0)
lw	a5,-36(s0)
neg	a5,a5
sw	a5,-36(s0)
.itoaL2:
lw	a5,-36(s0)
bnez	a5,.itoaL5
lw	a5,-20(s0)
addi	a5,a5,-1
sw	a5,-20(s0)
lw	a5,-20(s0)
li	a4,48
sb	a4,0(a5)
lw	a5,-20(s0)
j	.itoaL4
.itoaL6:
lw	a4,-36(s0)
li	a5,10
rem	a5,a4,a5
andi	a5,a5,0xff
lw	a4,-20(s0)
addi	a4,a4,-1
sw	a4,-20(s0)
addi	a5,a5,48
andi	a4,a5,0xff
lw	a5,-20(s0)
sb	a4,0(a5)
lw	a4,-36(s0)
li	a5,10
div	a5,a4,a5
sw	a5,-36(s0)
.itoaL5:
lw	a5,-36(s0)
bgtz	a5,.itoaL6
lbu	a5,-21(s0)
beqz	a5,.itoaL7
lw	a5,-20(s0)
addi	a5,a5,-1
sw	a5,-20(s0)
lw	a5,-20(s0)
li	a4,45
sb	a4,0(a5)
.itoaL7:
lw	a5,-20(s0)
.itoaL4:
mv	a0,a5
lw	s0,44(sp)
addi	sp,sp,48
jr	ra


println:
mv t6, ra
lui a1, %hi(nl)
addi a1, a1, %lo(nl)
jal ra, print_a1
jr t6
print_string:
addi sp, sp, -8
sw ra, 0(sp)
addi sp, sp, -8
sw s0, 0(sp)
addi sp, sp, -64
sw a0, 56(sp)
sw a1, 48(sp)
sw a2, 40(sp)
sw a3, 32(sp)
sw a4, 24(sp)
sw a5, 16(sp)
sw a6, 8(sp)
sw a7, 0(sp)
mv a1, a0
jal print_a1
lw a0, 56(sp)
lw a1, 48(sp)
lw a2, 40(sp)
lw a3, 32(sp)
lw a4, 24(sp)
lw a5, 16(sp)
lw a6, 8(sp)
lw a7, 0(sp)
addi sp, sp, 64
lw s0, 0(sp)
addi sp, sp, 8
lw ra, 0(sp)
addi sp, sp, 8
jr ra
	.section .rodata
	nl:
	.string "\n"

	.section .data
	buf:
	.string "XXXXXXXXXXXXXXXXXXXX"
