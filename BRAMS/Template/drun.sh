#!/bin/bash
bsub -q moorcroft_6100b -J debug.brams406 -n 48 -a openmpi -Is /bin/bash
