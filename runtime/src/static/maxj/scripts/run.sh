#!/bin/bash
DIR=RunRules/Simulation
bash ./sedify.sh

ARGS="$@"
make -C ${DIR} runsim RUNARGS="$ARGS"
