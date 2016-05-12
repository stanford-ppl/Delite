#!/bin/bash
FPGA_DIR=RunRules/DFE

ARGS="$@"
make -C ${FPGA_DIR} runsim RUNARGS="$ARGS"
