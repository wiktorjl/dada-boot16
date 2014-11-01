boot16
======

Adventures in 16 bit real mode assembly programming.

Stage 0 fits in 512 bytes and finds stage 1 binary on a FAT12 fs, then it jumps into it. Stage 1 finds kernel.bin, jumps switches into protected mode and jumps into the kernel. The kernel itself is just an example and is provided as a binary blob.
