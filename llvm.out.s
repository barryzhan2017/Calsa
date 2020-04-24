	.section	__TEXT,__text,regular,pure_instructions
	.macosx_version_min 10, 15
	.globl	_main                   ## -- Begin function main
	.p2align	4, 0x90
_main:                                  ## @main
	.cfi_startproc
## %bb.0:                               ## %entry
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	8(%rsp), %rdi
	movq	16(%rsp), %rsi
	callq	_add
	xorl	%eax, %eax
	addq	$24, %rsp
	retq
	.cfi_endproc
                                        ## -- End function

.subsections_via_symbols
