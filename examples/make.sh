#!/bin/sh

for i in *.bin ; do
    B=$(basename $i .bin)
    make ${B}.m4a
done
