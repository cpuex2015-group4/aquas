.data
one:
	.word 0x3f800000
.text
_leml_entry:
	li %r4,$10
	add.s %f1,%f0,%f0
	ld.s %f2,%r0,one
loop:
	beq %r0,%r4,loop_end
	mr.s %f3,%f1
	mr.s %f1,%f2
	add.s %f2,%f1,%f3
	subi %r4,%r4,$1
	j loop
loop_end:
	hlt
