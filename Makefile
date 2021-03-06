NASM_CMD = nasm
NASM_FLAGS = -g -f bin

.PHONY: all stage0 stage1

all: clean 

stage0:
	@echo Building stage0...
	$(NASM_CMD) $(NASM_FLAGS) stage0/boot.s -o data/stage0.bin
	@echo Creating new floppy image...
	cat data/stage0.bin data/new_floppy_data_master.img > data/floppy.img
	ls -la data/floppy.img
	@echo Floppy ready.

stage1:
	@echo Building stage1...
	$(NASM_CMD) $(NASM_FLAGS) stage1/stage1.s -o data/stage1.bin
	sh ./scripts/build-stage1.sh

clean:
	@echo Cleaning stage0
	rm -rfv data/stage0.bin
	rm -rfv data/stage1.bin
	rm -rfv data/floppy.img

