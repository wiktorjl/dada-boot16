bits 32

X db 0
Y db 0

%define VIDEOADDR 0xB8000
%define NUMCOLS 80
%define NUMROWS 25

clear_screen:
    pusha
    cld
    mov edi, VIDEOADDR 
    mov cx, 2000
    mov ah, 30
    mov al, ' '
    rep stosw
    mov byte [X], 0
    mov byte [Y], 0
    popa
    ret


; ebx = string
print_string:
    pusha
    push ebx
    pop edi

.loop:
    mov bl, byte[edi]
    cmp bl, 0
    je .done

    call print_char
    inc edi
    jmp .loop
            
.done:
    popa
    ret
    
; ebx = char
print_char:
    pusha
    cld


    ; position = x + y * NUMCOLS
    ; each char is 2 bytes
    mov edi, VIDEOADDR

    ; eax = 2 * NUMCOLS * y
    xor eax, eax
    xor ecx, ecx
    mov cl, byte[Y]
    mov eax, 2*NUMCOLS
    mul ecx
    push eax

    ; edi = x * 2 + VIDEOADDR 
    xor eax, eax
    xor ecx, ecx
    mov al, byte[X]
    mov ecx, 2
    mul ecx
    pop ecx
    add eax, ecx
    add edi, eax 
   

    cmp bl, 0xA
    je .next_line

    ; print char from bl
    mov dl, bl
    mov dh, 30
    mov word[edi],dx

    ; increment x
    inc byte[X]
    jmp .done

.next_line:
    mov byte[X], 0
    inc byte[Y]

    ; restore and return

.done:
    popa
    ret

update_cursor:
    pusha
    
    xor eax, eax
    xor ecx, ecx
    
    mov cl, byte[Y]
    mov eax, NUMCOLS
    mul ecx
    xor ecx, ecx
    mov cl, byte[X]
    add eax, ecx
    mov ecx, eax

    mov al, 0xf
    mov dx, 0x3d4
    out dx, al

    mov dx, 0x3d5
    mov al, cl
    out dx, al


    mov al, 0xe
    mov dx, 0x3d4
    out dx, al

    mov dx, 0x3d5
    mov al, ch
    out dx, al

    popa
    ret

