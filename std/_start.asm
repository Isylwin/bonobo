        global _start
        extern banana
        
        section .text

_start:
        call    banana
        mov     rdi, rax
        mov     rax, 60
        syscall
