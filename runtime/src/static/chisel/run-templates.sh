#!/usr/bin/env bash
# set -ev

# sed Launcher to create a launcher for each test
file=${DELITE_HOME}/runtime/src/static/tests/templates/Launcher.scala
# Get list of args
startArgs=(`grep -n "\/\/ Start args" $file | sed "s/:\/\/ Start args//g"`)
endArgs=(`grep -n "\/\/ End args" $file | sed "s/:\/\/ End args//g"`)
tests=(`sed -n ${startArgs},${endArgs}p $file | grep val | sed "s/.*val //g" | sed "s/ =.*//g"`)
# Edit launcher
startLaunch=(`grep -n "\/\/ Start launcher" $file | sed "s/:.*\/\/ Start launcher//g"`)
endLaunch=(`grep -n "\/\/ End launcher" $file | sed "s/:.*\/\/ End launcher//g"`)
lines=(`cat $file | wc -l`)
newfile=file=${DELITE_HOME}/runtime/src/static/tests/templates/expandedlauncher
sed -n 1,${startLaunch}p $file > $newfile
for t in ${tests[@]}; do
echo "  templates = templates ++ Arguments.${t}.zipWithIndex.map{ case(arg,i) => 
    (s\"${t}\$i\" -> { (backendName: String) =>
        Driver(() => new ${t}(arg), \"verilator\") {
          (c) => new ${t}Tests(c)
        }
      }) 
  }.toMap
" >> $newfile
done
sed -n ${endLaunch},$((lines+1))p $file >> $newfile
cp $newfile $file
rm $newfile

if [[ -n $1 ]]; then
	sbt "test:run-main templates.Launcher $1"
else
	sbt "test:run-main templates.Launcher all"
fi

