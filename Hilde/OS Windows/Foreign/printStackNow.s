.text
	.align 4
	.global _printStackNow
_printStackNow:
	movl 4(%esp),%eax
	pushl _stack_trace_depth
	movl %eax,_stack_trace_depth
	pushl %esi
	pushl %edi
	pushl %ebp
	call write_profile_stack
	popl %ebp
	popl %edi
	popl %esi
	popl %eax
	movl %eax,_stack_trace_depth
	ret

	.align 4
	.global _stackPrintSize
_stackPrintSize:
	movl 4(%esp),%eax
	pushl _stack_trace_depth
	movl %eax,_stack_trace_depth
	popl %eax
	ret

