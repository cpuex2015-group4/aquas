.data
.word 0x00000000

.text
_leml_entry:
	addi %r4,%r0,$13
	addi %r1,%r0,$0
	addi %r2,%r0,$2
loop:
	beq %r0,%r4,loop_end
	add %r3,%r0,%r1
	add %r1,%r0,%r2
	add %r2,%r1,%r3
	subi %r4,%r4,$1
	j loop
loop_end:
	out %r1
	hlt
