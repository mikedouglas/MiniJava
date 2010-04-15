	.file	"example-frame.c"
	.text
.globl sample_fun
	.type	sample_fun, @function
sample_fun:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$16, %esp
	movl	12(%ebp), %edx
	movl	8(%ebp), %eax
	subl	%edx, %eax
	movl	%eax, -4(%ebp)
	movl	12(%ebp), %edx
	movl	8(%ebp), %eax
	addl	%edx, %eax
	movl	%eax, -8(%ebp)
	movl	-4(%ebp), %eax
	imull	-8(%ebp), %eax
	leave
	ret
	.size	sample_fun, .-sample_fun
.globl mj_main
	.type	mj_main, @function
mj_main:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
	movl	$20, 4(%esp)
	movl	$10, (%esp)
	call	sample_fun
	movl	%eax, (%esp)
	call	mj_println
	leave
	ret
	.size	mj_main, .-mj_main
	.ident	"GCC: (Ubuntu 4.3.3-5ubuntu4) 4.3.3"
	.section	.note.GNU-stack,"",@progbits
