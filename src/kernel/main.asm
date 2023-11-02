org 0x7C00          ; specifies address in which instructions are loaded in RAM. x7C00 is standard.

bits 16              ; bits 16 indicates the code should be ran in 16 bit real mode

; 0x0D represents carriage return character, 0A represents a line feed character. In most systems
; these two together represent a new line. ENDL is now a shorthand for these two specific bytes
%define ENDL 0x0D, 0x0A 

start:
    jmp main

puts: 
    push si ; si = source index. source register. commonly used as pointer to array or string in memory
    push ax ; save registers in function call by pushing into stack

.loop:
    lodsb         ; load a byte from address pointed by si. Byte value is stored in AL register
                  ; si is then incremented or decremented depending on direction flag
    or al, al     ; OR destination, source.   compares both registers, and saves r1 bitwise or r2 into r1. 
    jz .done

    mov ah, 0x0e  ; calls bios function call for interrupt
    mov bh, 0     ; AH register used to specificy the function call for BIOS interrupt calls.
    int 0x10      ; bh register used for the page number when dealing with text output on older systems
    jmp .loop

.done:
    pop ax   
    pop si
    ret       ; return from current function and resume execution at the address stored on the stack

main:

    ; set up data segments
    mov ax, 0            ; cant write to ds / es directly.   
    mov ds, ax           ; ds = data segment.
    mov es, ax           ; ax = general register
                         ; es = extra segment. setting to 0 means 0x0000 since it should point to a segment
                         ; resets the registers to value of 0
    ; setup stack 
    mov ss, ax           ; ss = stack segment
    mov sp, 0x7C00       ; stack grows downwards from where we are loaded in memory
                         ; stack starts at end of first sector and grows down towards address 0x0000

                         ; setting these segments to be segment 0, points them to the first 64 kb of memory

                         ; x86 divides memory into segments e.g data, stack, code 
                         ; each register has a diffrent base address. help determine base address of segment in use
                         ; you might need a data or stack segment if youre running multiple programs
                         ; you might not if youre only running one, since the address location is constant
    ; print message
    mov si, msg_hello ; moves the address of msg_hello to si
    call puts

    hlt

.halt:
    jmp .halt
                                     ; data section gets placed in memory immediately after code is
msg_hello: db 'Hello world', ENDL, 0 ; 0 represents null terminating char, the commas are different 
                                     ; parts of the string

times 510-($-$$) db 0    ; ensures that the bootloader CODE is atleast 512 bytes long with the last two...
dw 0AA55h                ; ...storing 0xAA55 in the last two bytes of the boot sector. 
                         ; this is called boot signature and is used by the bios to recognize valid boot sector
                         ; if found, the bios considers the sector as bootable and attempts to load and execute 
                         ; the code in the boot sector
                         ; boot sector is not in the hard drive, but in the floppy drive here atleast
                         ; boot sector must be 512 bytes to be recongized and ran by the bios