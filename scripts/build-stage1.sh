#!/bin/sh

sudo ./scripts/mnt.sh
sudo cp -v data/stage1.bin /media/floppy/
sudo cp -v data/random_kernel.bin /media/floppy/kernel.bin
sleep 1
sudo umount /media/floppy
