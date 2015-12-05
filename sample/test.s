.data
.word 0x38FFFFFF

.text 
_leml_entry:
	in    %r5
	out    %r4
	itof  %f2, %r3
	ftoi  %r1, %f0
	nop
	hlt
