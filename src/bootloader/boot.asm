org 0x7C00          ; specifies address in which instructions are loaded in RAM. x7C00 is standard.
                    ; called a directive, gives clue to assembler how code should be assembled 
                    ; this does NOT get translated into machine code, assembler specific, diff assemblers
                    ; have diff directives
bits 16             ; bits 16 indicates the code should be ran in 16 bit real mode
                    ; x86 should always start in 16 bit real mode, for backwards compatability purposes

; in ascii, 0x0D represents carriage return character, 0A represents a line feed character. In most systems
; these two together represent a new line. ENDL is now a shorthand for these two specific bytes
%define ENDL 0x0D, 0x0A

; FAT12 HEADER (neccessary things in the boot sector)
; we need to include this because we overwrite these first sector of the img file with our bootloader
; this section contains important headers used by fat 12, so by overwriting it, we've broken the file system
jmp short start
nop

bdb_oem:                               db 'MSWIN4.1'   ; 8 bytes
bdb_bytes_per_sector:                  dw 512
bdb_sectors_per_clustor:               db 1
bdb_reserved_sectors:                  dw 1
bdb_fat_count:                         db 2
bdb_dir_entries_count:                 dw 0E0h
bdb_total_sectors:                     dw 2880          ; 2880 sectors * 512 bytes = 1.44 MB total
bdb_media_descriptor_type:             db 0F0h          ; F0 = 3.5 in floppy disk
bdb_sectors_per_fat:                   dw 9             ; 9 sectors / fat
bdb_sectors_per_track:                 dw 18
bdb_heads:                             dw 2
bdb_hidden_sectors:                    dd 0
bdb_large_sector_count:                dd 0

; extended boot record
ebr_drive_number:                      db 0             ; 0x00 floppy. 
                                       db 0             ; reserved
ebr_signature:                         db 29h   
ebr_volume_id:                         db 12h, 34h, 56h, 78h ; serial number, value doesnt matter
ebr_volume_label:                      db 'KRISPOOL OS' ; 11 BYTES padding with spaces
ebr_system_id:                         db 'FAT12   '    ; 8 BYTES

;
; code goes here
;

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
    pop bx
    pop ax   
    pop si
    ret       ; return from current function and resume execution at the address stored on the stack

main:

    mov ax, 0            ; cant write to ds / es directly.   
    mov ds, ax           ; ds = data segment.
    mov es, ax            ; ax = general register

    mov ss, ax           ; ss = stack segment
    mov sp, 0x7C00       ; stack grows downwards from where we are loaded in memory

    mov [ebr_drive_number], dl

    mov ax, 1            ; LBA=1, second sector from disk 
    mov cl, 1            ; 1 sector to read
    mov bx, 0x7E00       ; data should be after bootloader
    call disk_read 

    mov si, msg_hello    ; moves the address of msg_hello to si
    call puts
    
    cli                  ; disables interrupts, this way CPU cant get out of halt state
    hlt 

;
; error handlers
;

floppy_error: 
    mov si, msg_read_failed
    call puts
    jmp wait_key_and_reboot

wait_key_and_reboot:
    mov ah, 0
    int 16h               ; waits for keypress
    jmp 0FFFFh:0          ; jump to beginning of BIOS, should reboot

.halt:
    cli
    hlt

;
; Disk Routines
;

; converts an LBA address to a CHS address
; parameters:
;    - ax : LBA address
; Returns:
;    - cx [bits 0-5]: sector number
;    - cx [bits 6-15]: cylinder
;    - dh: head

lba_to_chs: ; calculates the conversion and stores the result in the expected register 

    push ax                             
    push dx

    xor dx, dx                          ; dx = 0
    div word [bdb_sectors_per_track]    ; ax = LBA / SectorsPerTrack
                                        ; div divides ax register by another operand, word is used to tell its size
                                        ; dx = LBA % SectorsPerTrack. (remainder from div is stored in dx)

    inc dx                              ; dx = (LBA % SectorsPerTrack + 1) = sector
    mov cx, dx                          ; cx = sector

    xor dx, dx                          ; dx = 0
    div word [bdb_heads]                ; ax = (LBA / SectorsPerTrack) / Heads = cylinder
                                        ; dx = (LBA / SectorsPerTrack) % Heads = head
    mov dh, dl                          ; dh = head
    mov ch, al                          ; ch = cylinder (lower 8 bits)
    shl ah, 6
    or cl, ah                           ; put upper 2 bits of cylinder in CL

    pop ax
    mov dl, al                          ; restore DL
    pop ax
    ret

; reads sectors from a disk
; parameters:
;   - ax: LBA address
;   - cl: number of sectors to read (up to 128)
;   - dl: drive number
;   - es:bx: memory address where to store read data

disk_read:

    push ax                              ; save registers we will modify
    push bx  
    push cx
    push dx
    push di

    push cx                              ; temporarily save CL (number of sectors to read)
    call lba_to_chs                      ; compute CHS
    pop ax                               ; AL = number of sectors to read
    
    mov ah, 02h                          ; sets up bios interrupt call, giving info on what kind
    mov di, 3                            ; retry count

.retry: 
    pusha                                ; save all registers, we dont know which registers bios modifies
    stc                                  ; set carry flag, some BIOS'es dont set it
    
    int 13h                              ; 13h is a bios function used to read disk sectors
    
    jnc .done                            ; jump if carry is not set

    ; read failed
    popa
    call disk_reset

    dec di
    test di, di
    jnz .retry
    
.fail: 
    ; all attempts failed
    jmp floppy_error

.done:
    popa

    pop di                              ; restore registers modified
    pop dx  
    pop cx
    pop bx
    pop ax
    ret

;
; Resets disk controller
; parameters
;   dl: driver number 
;
disk_reset:
    pusha
    mov ah, 0
    stc
    int 13h
    jc floppy_error
    popa
    ret

                ; data section gets placed in memory immediately after code is
msg_hello:       db 'Hello world!', ENDL, 0 ;0 represents null terminating char, commas = different parts of str 
msg_read_failed: db 'Read from disk failed!', ENDL, 0

times 510-($-$$) db 0    ; ensures that the bootloader CODE is atleast 512 bytes long with the last two...
dw 0AA55h                ; ...storing 0xAA55 in the last two bytes of the boot sector. 
                         ; this is called boot signature and is used by the bios to recognize valid boot sector
                         ; if found, the bios considers the sector as bootable and attempts to load and execute 
                         ; the code in the boot sector
                         ; boot sector is not in the hard drive, but in the floppy drive here atleast
                         ; boot sector must be 512 bytes to be recongized and ran by the bios