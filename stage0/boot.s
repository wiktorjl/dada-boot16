
; Stage 0 bootloader
; Wiktor Lukasik (wiktor@lukasik.org)
;
; ==============================================================================
; MEMORY MAP
; ------------------------------------------------------------------------------
; 0x9200    - STAGE1
; 0x7E00    - BOOT SECTOR
; 0x7C00    - GAP
; 0x5000    - STACK
; 0x3300    - FAT12
; 0x2100    - ROOT TABLE END
; 0x0500    - ROOT TABLE BEGIN
;
; ==============================================================================
; HELPFUL LINKS:
; ------------------------------------------------------------------------------
; int 13h interrupt list - http://en.wikipedia.org/wiki/INT_13H
; x86 instruction set - http://x86.renejeschke.de/
; memory map - http://wiki.osdev.org/Memory_Map_(x86)
;
; ==============================================================================
; DESCRIPTION
; ------------------------------------------------------------------------------
; Boot sector is 512 bytes, which is really tiny and not enought for much more than
; code to read a larger binary from floppy. So, we need a multi stage boot loading 
; process. Let's call this stage stage0.
;
; stage0's purpose is to read the list of files from a fat12 partition on a floppy
; and find binary file belonging to stage1. The very last thing in stage0 is to 
; load stage1 into memory and jump into it.
;
; stage1 will attempt to perform some setup so we can boot into kernel proper.
;
%macro save_ds_es_cx 0
    push ds
    push es
    push cx
%endmacro  

%macro  restore_cx_es_ds 0
    pop cx
    pop es
    pop ds
%endmacro

org 0x7c00
bits	16

; Local constants
bytesPerRootEntry   equ 0x20
rootDirSegmentStart equ 0x50
stackSegmentStart   equ 0x500
stackSize           equ 0x1d00
stage1SegmentStart  equ 0x210

; First three bytes is a jump instruction over
; BPB to the actuall code
start:	jmp loader

; Name of the system
bsOEM			DB " WOS1    "

; BIOS parameter block
bpbBytesPerSector: 	    DW 512
bpbSectorsPerCluster: 	DB 1
bpbReservedSectors: 	DW 1
bpbNumberOfFATs: 	    DB 2
bpbRootEntries: 	    DW 224
bpbTotalSectors: 	    DW 2880
bpbMedia: 	            DB 0xf8
bpbSectorsPerFAT: 	    DW 9
bpbSectorsPerTrack: 	DW 18
bpbHeadsPerCylinder: 	DW 2
bpbHiddenSectors: 	    DD 0
bpbTotalSectorsBig:     DD 0
bsDriveNumber: 	        DB 0
bsUnused: 	            DB 0
bsExtBootSignature: 	DB 0x29
bsSerialNumber:         DD 0x10203040
bsVolumeLabel: 	        DB "VOSBOOTDISK"
bsFileSystem: 	        DB "FAT12   "
fileName		        DB "...........", 0
stage1Name              DB "STAGE1  BIN", 0
section .text

;===============================================================================
; Entry point
; 
loader:

; Setup stack
    mov	ax, stackSegmentStart ; set stack segment to 0x5000 (16 * 0x500)
    mov	ss, ax
    mov	ax, stackSize ; stack size = 0x1000
    mov	sp, ax
    mov	bp, sp ; not sure we need to set-up bp, but do it anyway

    ;xor	ax, ax ; zero out data and extra segments
    ;mov	ds, ax
    ;mov	es, ax
    ;mov fs, ax
    ;mov gs, ax

; Load root and fat tables
    call load_root ; no need for fat table if stage1 is smallter than a sector and is located in root dir, but load anyway
    call load_fat

; Check if any file in the root directory matches stage1 file name
    xor cx, cx
    ;mov cx, [bpbRootEntries]; There can be up to 0xe0 entries in root directory
    mov cx, 1

.next_filename:
    ; Copy file name
    ; DS:SI -> ES:DI
    save_ds_es_cx ;save ds and es as we will use them to copy string

    ; Set DS (source segment) 
    mov ax, rootDirSegmentStart 
    mov ds, ax ; set data segment to point to root entry segment - this is source segment for string copy

    ; Set SI (source index)
    ; si = starting address of root directory entry
    ; ax = current item index
    ; 0x20 = size of root entry
    ; si = offset in root table = (a - 1) * 0x20
    mov ax, cx
    dec ax 
    mov bx, 0x20 
    mul bx 
    mov si, ax 

    ; Store logical cluster number of current entry
    push ax ; save source index
    mov ax, rootDirSegmentStart ; set up data segment for reading root table
    mov bx, 16 
    mul bx
    mov bx, ax
    pop ax
    add bx, ax ;bx = offest into root table
    add bx, 0x1A ;plus 26th byte gives first logical cluster of this file
    sub bx, rootDirSegmentStart * 16 ; this is awkward, i know
     
    mov ax, [bx] ; load value of first logical cluster into ax
    mov WORD[curRootByte], ax ; and save it
    
    ; Set loop counter - we want to copy 11 chars, 8 for filename and 3 for extension
    xor cx, cx
    mov cx, 11 ; each file name is up to 11 chars long
    
    ; Set ES:DI (destination)
    xor ax, ax
    mov es, ax ; es is destination segment for string copy - zero it out
    mov di, fileName ; instead use di (destination index) alone

    rep movsb ; copy string (cx characters from ds:si to es:di)  
    restore_cx_es_ds ; most recently cx was used to count number of chars in filename - now restore cx from stack that indicates number of files left

    cmp byte[fileName], 0x00 ; skip file if name starts with 0x00 - from fat 12 specs
    je .skip
    cmp byte[fileName], 0xe5 ; skip file if name starts with 0xe5 - from fat 12 specs
    je .skip

    ; Check if current file name is the same as stage1 
    pusha
    call filename_compare
    cmp ax, 1
    jz .found_stage1
    popa

    .skip:
    inc cx
    cmp cx, [bpbRootEntries]
    jnz .next_filename
    jmp stop

.found_stage1:
    popa
    xor cx, cx ; save index into file buffer
    push cx

; This is really uhly, needs refactoring.
.read_stage1_chunk:
    mov ax, WORD[curRootByte+0x500] ; get value from FAT index

    xor bx, bx ; Get address of next chunk
    mov ds, bx
    add ax, 31
    call lbatochs 
    xor bx, bx
    mov es, bx

    pop ax ; Increment index into FAT index, save it, and calculate file buffer start position
    mov dx, ax
    inc dx
    push dx
    mov cx, 0x200
    mul cx
    mov cx, 0x7e00
    add ax, cx
    mov bx, ax

    ; Read one sector
    mov ah, 0x02
    mov al, 0x1
    mov ch, [absoluteTrack]
    mov cl, [absoluteSector]
    mov dh, [absoluteHead]
    mov dl, 0
    int 0x13

    ; calculate next sector
    ; see http://www.brokenthorn.com/Resources/ for more details on how this works
    xor ax, ax
    mov ds, ax
    mov bx, WORD[curRootByte+0x500]
    mov cx, bx
    shr cx, 1
    add bx, cx
    mov cx, 0x2100
    add bx, cx
    mov cx, WORD[bx]
    mov ax, WORD[curRootByte+0x500]
    test ax, 0x0001
    jnz .odd
.even:
    and cx, 0000111111111111b
    jmp .done
.odd:
    shr cx, 0x4
.done:
    cmp cx, 0xff0
    mov WORD[curRootByte+0x500], cx
    jb .read_stage1_chunk

jump_to_stage1:    ; Jump to more code
   jmp 0x7e00 ; jump to stage1 now


load_fat:
    pusha
    call reset_floppy
    popa

    pusha

    xor ax, ax
    mov ax, 1
    call lbatochs

    mov bx, 0x210
    mov es, bx
    xor bx, bx
    mov ds, bx

    mov ah, 0x02 ; read inst
    mov al, 0x09 ; sectors to read
    mov ch, [absoluteTrack]; cylinder
    mov cl, [absoluteSector]; sector
    mov dh, [absoluteHead]; head
    mov dl, 0; drive
    int 0x13


    
.end_fat_read:
    popa
    ret    

stop:
	cli
	hlt

;===============================================================================
;Utils
;
;Reset floppy before reading, ag=dl=0DRIVE    
reset_floppy: 
    mov ah, 0
    mov dl, 0
    int 0x13
    jc reset_floppy ;CF is set on error
    ret

;LOGICAL CLUSTER TO CHS
;AX - cluster number
lbatochs:
    xor dx, dx                              ; prepare dx:ax for operation
    div WORD [bpbSectorsPerTrack]           ; calculate
    inc dl                                  ; adjust for sector 0
    mov BYTE [absoluteSector], dl
    xor dx, dx                              ; prepare dx:ax for operation
    div WORD [bpbHeadsPerCylinder]          ; calculate
    mov BYTE [absoluteHead], dl
    mov BYTE [absoluteTrack], al
    ret

; Compares two file names, one from root table, other one from fileName
filename_compare:
    push ebp
    mov ebp, esp

    ; Compare strings in DS:SI and ES:DI
    mov cx, 11          ; Compare 11 chars - needed by repe
    xor ax, ax          
    mov ds, ax          ; no need for values in segments ds and es
    mov es, ax
    mov si, fileName    ; set source and destination for string comparison
    mov di, stage1Name
    repe cmpsb           ; compare strings

    jnz .not_equal
    
    mov ax, 1 
    jmp .return

.not_equal: ; did not find file
    mov ax, 0
.return:
    pop ebp
    ret


;===============================================================================
;Root table loading  
;Steps:
;  1. Compute size
;  2. Find start position
;  3. Load

load_root:
; GET SIZE OF ROOT DIRECTORY (in # of sectors, store in CX)
; size of root = (0x20 (32) bytes per root entry * number of root entries) / 512 bytes in a sector 
; size of root = (32 * 224) / 512 = 14 sectors
    mov ax, bytesPerRootEntry
    mul word [bpbRootEntries]
    div word [bpbBytesPerSector]
    xchg ax, cx  ; CX should be 14, as per FAT12 docs   


; GET STARTING POINT OF ROOT DIRECTORY (store in AX)
; start of root = number of fats * sectors per fat + number of reserved sectors
; start of root = 2 * 9 + 1 = 19th sector (0x2600)
    xor ax, ax
    mov al, byte [bpbNumberOfFATs]
    mul word[bpbSectorsPerFAT]
    add ax, word [bpbReservedSectors]

; LOAD SECTOR OF ROOT DIRECTORY (at ES:BX=0x500)
; Set up destination buffer
    mov bx, rootDirSegmentStart
    mov es, bx
    xor bx, bx   

.load_root_loop:
; Save some registers
    push ax
    push bx
    push cx
; Calculate location on the floppy to read from based on logical cluster
; stored in ax (which points to beginning of root table).
    call    lbatochs

; Read root table from floppy
    mov ah, 0x02 ; read inst
    mov al, 0x01 ; sectors to read
    mov ch, [absoluteTrack]; cylinder
    mov cl, [absoluteSector]; sector
    mov dh, [absoluteHead]; head
    mov dl, 0; drive
    int 0x13

; Restore some registers after reading
    pop cx
    pop bx
    pop ax
    add bx, WORD [bpbBytesPerSector]

; Increase counter AX that holds # of sectors read and loop until it 
; matches CX which is how many sectors we have in root table 
    inc ax
    loop .load_root_loop

; Done, root table loaded
    ret 


curRootByte    dw 0x00
absoluteSector db 0x00
absoluteHead   db 0x00
absoluteTrack  db 0x00
times	510	- ($-$$) db 0


dw 0xAA55
