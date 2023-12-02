#!/bin/sh

SCRIPT_DIR=$(dirname "$(readlink "$0")")
TEMPLATE="$SCRIPT_DIR/template/DayXX.hs"

i=1
while [ $i -le 25 ]; do
  num=$(printf "%02d" "$i")
  tgt="$SCRIPT_DIR/src/AoC/Challenge/Day$num.hs"
  if [ ! -f "$tgt" ]; then
    sed "s/XX/$num/" < "$TEMPLATE" > "$tgt"
  fi
  i=$((i+1))
done
