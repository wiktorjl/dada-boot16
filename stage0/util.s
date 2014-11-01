; Stage 0 bootloader utils

;----------------------------------------------------------------------
; printn
; Desc: Print string with new line
; Param: SI - string, stack: 0/1 print new line 
;----------------------------------------------------------------------
%define PRINTNL 0x07
%define PRINTNONL 0x00

printn:
    push    bp
    mov     bp, sp
printn_begin:
    lodsb
    or  al, al
    jz  printn_done
    mov ah, 0x0e
    int 0x10
    jmp     printn_begin
printn_done:
    mov     bx, WORD [bp+4]
    cmp     bx, 0x07
    jne     printn_ret
    mov     ah, 0x0e
    mov     al, 0x0A
    int     0x10
    mov     al, 0x0D
    int     0x10
printn_ret:
    pop     bp
    ret
