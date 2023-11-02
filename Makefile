ASM = nasm
SRC_DIR = src
BUILD_DIR = build

.PHONY: all floppy_image kernel bootloader clean always
# phony means we can reference the target by name e.g kernel instead of like src/kernel/kernel.bin
# kinda like a #define is c

floppy_image: $(BUILD_DIR)/main_floppy.img


# if = input file    of = output file
# dd = data duplicator. often used for making disk or drives backups
# /dev/zero = special file in unix that provides endless stream of null bytes

# mkfs.fat = make file system fat
# mcopy is copying kernel.bin to the fat file system inside the img file, making it accessable inside the F.S
# does in fact take up 1.44mb in storage space from the file system
# img file = binary files that store raw disk images of floppy disks, hard drives, and optical disks. 
$(BUILD_DIR)/main_floppy.img: bootloader kernel # bs = block size.  count = block count  2880 * 512 bytes = 1.4 MB
	dd if=/dev/zero of=$(BUILD_DIR)/main_floppy.img bs=512 count=2880
	mkfs.fat -F 12 -n "NBOS" $(BUILD_DIR)/main_floppy.img
	dd if=$(BUILD_DIR)/bootloader.bin of=$(BUILD_DIR)/main_floppy.img conv=notrunc
	mcopy -i $(BUILD_DIR)/main_floppy.img $(BUILD_DIR)/kernel.bin "::kernel.bin"

bootloader: $(BUILD_DIR)/bootloader.bin

$(BUILD_DIR)/bootloader.bin: always # just assemble with nasm 
	$(ASM) -g $(SRC_DIR)/bootloader/boot.asm -f bin -o $(BUILD_DIR)/bootloader.bin

kernel: $(BUILD_DIR)/kernel.bin

$(BUILD_DIR)/kernel.bin: always # just assemble with nasm
	$(ASM) -g $(SRC_DIR)/kernel/main.asm -f bin -o $(BUILD_DIR)/kernel.bin 

always: # makes dir if it doesnt exist
	mkdir -p $(BUILD_DIR)

clean: 
	rm -rf $(BUILD_DIR)/*