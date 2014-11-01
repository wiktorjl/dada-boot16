#!/bin/sh

rm data/new_floppy.img
/sbin/mkdosfs -C -f 2 -F 12 -h 0 -r 224 -R 1 -S 512 data/new_floppy.img 1440

