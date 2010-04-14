	.file	"runtime.c"
	.text
.globl main
	.type	main, @function
main:
	leal	4(%esp), %ecx
	andl	$-16, %esp
	pushl	-4(%ecx)
	pushl	%ebp
	movl	%esp, %ebp
	pushl	%ecx
	subl	$4, %esp
	call	mj_main
	movl	$0, %eax
	addl	$4, %esp
	popl	%ecx
	popl	%ebp
	leal	-4(%ecx), %esp
	ret
	.size	main, .-main
	.section	.rodata
.LC0:
	.string	"%d\n"
	.text
.globl mj_println
	.type	mj_println, @function
mj_println:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$8, %esp
	movl	8(%ebp), %eax
	movl	%eax, 4(%esp)
	movl	$.LC0, (%esp)
	call	printf
	leave
	ret
	.size	mj_println, .-mj_println
.globl mj_new_object
	.type	mj_new_object, @function
mj_new_object:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$24, %esp
	movl	8(%ebp), %eax
	movl	%eax, (%esp)
	call	malloc
	movl	%eax, -4(%ebp)
	movl	$0, -8(%ebp)
	jmp	.L6
.L7:
	movl	-8(%ebp), %eax
	addl	-4(%ebp), %eax
	movb	$0, (%eax)
	addl	$1, -8(%ebp)
.L6:
	movl	-8(%ebp), %eax
	cmpl	8(%ebp), %eax
	jl	.L7
	movl	-4(%ebp), %eax
	leave
	ret
	.size	mj_new_object, .-mj_new_object
.globl mj_new_array
	.type	mj_new_array, @function
mj_new_array:
	pushl	%ebp
	movl	%esp, %ebp
	subl	$24, %esp
	movl	8(%ebp), %eax
	addl	$1, %eax
	sall	$2, %eax
	movl	%eax, (%esp)
	call	malloc
	movl	%eax, -4(%ebp)
	movl	-4(%ebp), %edx
	movl	8(%ebp), %eax
	movl	%eax, (%edx)
	addl	$4, -4(%ebp)
	movl	$0, -8(%ebp)
	jmp	.L10
.L11:
	movl	-8(%ebp), %eax
	sall	$2, %eax
	addl	-4(%ebp), %eax
	movl	$0, (%eax)
	addl	$1, -8(%ebp)
.L10:
	movl	-8(%ebp), %eax
	cmpl	8(%ebp), %eax
	jl	.L11
	movl	-4(%ebp), %eax
	leave
	ret
	.size	mj_new_array, .-mj_new_array
	.ident	"GCC: (Ubuntu 4.3.3-5ubuntu4) 4.3.3"
	.section	.note.GNU-stack,"",@progbits
