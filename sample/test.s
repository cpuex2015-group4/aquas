.data
.word 0x38FFFFFF

.text 
_leml_entry:
	in    %r5
	out    %r4
	itof  %f2, %r3
l1:
	ftoi  %r1, %f0
	beq  %r1, %r4, l1
	beq.s  %f1, %f31, l1
	add   %r3, %r5, %r5
	muln.s %f4, %f4, %f30 
	nop
	hlt
