        global assert
        
        section .text

assert:
        cmp     rdi, 0
        je      __assert_f
        ret
__assert_f:
        mov     rax, 60
        mov     rdi, 1
        syscall
