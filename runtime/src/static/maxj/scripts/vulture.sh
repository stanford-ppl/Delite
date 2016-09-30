#!/bin/bash

if [ $# -ne 1 ]; then
  echo "Usage: $0 <screen_session_name>"
  exit -1
fi

SESSION_NAME=$1

# Create a new screen session in detached mode
screen -d -m -S $SESSION_NAME

# For each subdirectory in the current directory, create a new screen window and launch job
for f in *; do
  if [ -d $f ]; then
    CMD="cd $f; source scripts/setup.sh; make synth"

    # Creates a new screen window with title '$f' in existing screen session
    screen -S $SESSION_NAME -X screen -t $f

    # Launch $CMD in newly created screen window
    screen -S $SESSION_NAME -p $f -X stuff "$CMD\n"
  fi
done
