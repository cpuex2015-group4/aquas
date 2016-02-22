.data
.text
	.extern _leml_entry
fib.10:
	li      %r1, $1
	ble     %r9, %r1, nle_else.24
	subi    %r10, %r9, $1
	st      %r9, %r7, $-1  # save n.11
	mr      %r9, %r10
	subi    %r7, %r7, $2
	st      %r4, %r7, $0
	jal     fib.10
	ld      %r4, %r7, $0
	addi    %r7, %r7, $2
	mr      %r9, %r3
	ld      %r10, %r7, $-1  # restore n.11
	subi    %r10, %r10, $2
	st      %r9, %r7, $-2  # save Ti6.15
	mr      %r9, %r10
	subi    %r7, %r7, $3
	st      %r4, %r7, $0
	jal     fib.10
	ld      %r4, %r7, $0
	addi    %r7, %r7, $3
	mr      %r9, %r3
	ld      %r10, %r7, $-2  # restore Ti6.15
	add     %r3, %r10, %r9
	jr      %r4
nle_else.24:
	mr      %r3, %r9
	jr      %r4
_leml_entry: # main entry point
	# main program start
	li      %r7, $31
	sll     %r7, %r7, $15
	addi    %r7, %r7, $32767
	li      %r5, min_caml_heap_pointer
	li      %r9, $30
	subi    %r7, %r7, $1
	st      %r4, %r7, $0
	jal     fib.10
	ld      %r4, %r7, $0
	addi    %r7, %r7, $1
	mr      %r9, %r3
	subi    %r7, %r7, $1
	st      %r4, %r7, $0
	ld      %r4, %r7, $0
	addi    %r7, %r7, $1
	# main program end
	hlt

min_caml_heap_pointer:
