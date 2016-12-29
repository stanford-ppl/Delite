#!/bin/bash

# Infer app name
appName=(`grep -r "TopModule for application: " chisel/src/kernels | sed "s/.*TopModule for application: //g"`)
echo "Running $appName with args $1 $2 $3 $4 $5 $6..."

## Sed test script to poke and expect for proper app
# Make sure this test has been written
pokeExists=(`cat chisel/src/app-test/apps/TopModule.scala | grep "${appName}() {" | wc -l`)
peekExists=(`cat chisel/src/app-test/apps/TopModule.scala | grep "${appName}Check() {" | wc -l`)
ok=$(($pokeExists+$peekExists))
if [[ $ok != 2 ]]; then
  echo "ERROR: Missing poke (${pokeExists}) or peek (${peekExists}) tests for $appName"
  exit 1
fi
# Swap function calls in main test
sed -i "s/.*() \/\/ Poke inputs/${appName}() \/\/ Poke inputs/g" chisel/src/app-test/apps/TopModule.scala
sed -i "s/.*() \/\/ Expect outputs/${appName}Check() \/\/ Expect outputs/g" chisel/src/app-test/apps/TopModule.scala
# Edit input args
for i in `seq 1 6`; do
  argtag=$(echo "\$$i")
  cmd="echo $argtag"
  input=(`eval "$cmd"`)
  if [[ ! -n $input ]]; then
    input=999
  else
    input=$input
  fi
  cmd="sed -i \"s/val input${i} = [0-9]\+/val input${i} = $input/g\" chisel/src/app-test/apps/TopModule.scala"
  eval "$cmd"
done
# Get TopModule input/output names
#  (Assumes only one in and one out)
inputName=(`cat chisel/src/kernels/IOBundle.scala | grep "Input" | cut -d ' ' -f 4`)
outputName=(`cat chisel/src/kernels/IOBundle.scala | grep "Output" | cut -d ' ' -f 4`)
sed -i "s/poke(c\.io\.ArgIn\..*,/poke(c\.io\.ArgIn\.${inputName},/g" chisel/src/app-test/apps/TopModule.scala
sed -i "s/expect(c\.io\.ArgOut\..*,/expect(c\.io\.ArgOut\.${outputName},/g" chisel/src/app-test/apps/TopModule.scala

sbt "test:run-main app.Launcher TopModule"

