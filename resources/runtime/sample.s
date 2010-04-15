	.file	"sample.c"
	.text
.globl mj_main
	.type	mj_main, @function
mj_main:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
	movl	$10, (%esp)
	call	mj_println
	leave
	ret
	.size	mj_main, .-mj_main
	.ident	"GCC: (GNU) 4.2.4 (Ubuntu 4.2.4-1ubuntu3)"
	.section	.note.GNU-stack,"",@progbits
