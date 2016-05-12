#!/bin/bash

if [ $# -eq 0 ]; then
	echo "Randomizing socket name.."
	HASH=$(cat /dev/urandom | tr -dc 'a-zA-Z' | fold -w 10 | head -n 1)
	echo $HASH
	PORT_NAME='maxcportsim'
	PORT_NAME+=$HASH
elif [ $# -eq 1 ]; then
	PORT_NAME=$1
else
	echo "Usage: $0 <name>"
	exit -1
fi


sed "s/<socketName>.*<\/socketName>/<socketName>${PORT_NAME}<\/socketName>/g" RunRules/Simulation/RunRules.settings > rtmp
sed "s/^PORT_NAME_SIM?=.*$/PORT_NAME_SIM?=${PORT_NAME}/g" RunRules/Simulation/Makefile.include > mtmp

mv rtmp RunRules/Simulation/RunRules.settings 
mv mtmp RunRules/Simulation/Makefile.include
