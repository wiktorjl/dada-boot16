org 0x7e00
bits 16

jmp stage1

%include 'stage1/fat12.s'
%include 'stage0/util.s'
%include 'stage1/vga.s'

bits 16

; GDT stuff goes here.
; First is null descriptor, 8 bytes of 0's
gdt_start:
descriptor_null:
    dw 0
    dw 0
    dw 0
    dw 0

descriptor_code:
    dw 0xFFFF; limit address 0-15
    dw 0x0; base address 0-15
    db 0x0; base address 16-23
    db 10011010b; type (segment type), S (descriptor type), DPL (ring), P (segment present)
    db 11001111b; seg length, A, 0 (zero), D (operand size), G (granularity)
    db 0; base address 24-31


descriptor_data:
    dw 0xFFFF; limit address 0-15
    dw 0x0; base address 0-15
    db 0x0; base address 16-23
    db 10010010b; type (segment type), S (descriptor type), DPL (ring), P (segment present)
    db 11001111b; seg length, A, 0 (zero), D (operand size), G (granularity)
    db 0; base address 24-31
gdt_end:


gdt_struct:
    dw gdt_end - gdt_start - 1
    dd gdt_start


msg_enteringpmode db "Entering PMode", 0
msg_enteredpmode db "Entered PMode", 0
msg_installingGDT db "Installing GDT", 0
msg_installedGDT db "Installed GDT", 0
msg_enablinga20 db "Enabling gate A20", 0
msg_enableda20 db "Enabled gate A20", 0
msg_criticalerror db "Critical error. Halting!", 0

stage1:
    ; Stage 1 located at 0x7e00
    mov ax, 0x0 ; 0x7e0
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax


    ; Install GDT
    push PRINTNL
    mov si, msg_installingGDT
    call printn
    add sp, 2

    cli
    lgdt [gdt_struct]
    sti

    push PRINTNL
    mov si, msg_installedGDT
    call printn
    add sp, 2
     
    ; Enable gate A20
    push PRINTNL
    mov si, msg_enablinga20
    call printn
    add sp, 2
    mov ax, 0x2401
    int 0x15    
    jc critical_error


    call find_kernel

    ; Set protected mode
    push PRINTNL
    mov si, msg_enteringpmode
    call printn
    add sp, 2

    cli
    mov eax, cr0 
    or eax, 1
    mov cr0, eax

    jmp 0x8:pmode_start
    
; Inform of unrecoverable error
critical_error:
    push PRINTNL
    mov si, msg_criticalerror
    call printn
    add sp, 2

; Halt
halt:
    hlt
    jmp halt

bits 32
msg_starting db "32-bit bootloader running...",0xA,0 

pmode_start:

    mov ax, 0x10
    mov ds, eax
    mov ss, eax
    mov es, eax
    mov esp, 0x90000

    call clear_screen
    
    mov ebx, msg_starting
    call print_string
    call update_cursor

.halt:
    hlt
    jmp .halt    
 
