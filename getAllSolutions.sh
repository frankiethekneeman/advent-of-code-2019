#!/bin/bash
for day in $(seq 1  25)
do
  for part in $(seq 1 2)
  do 
    echo "DAY $day PART $PART:"
    (
        cd day$day/part$part
        time ghc solution.hs
        time ./solution ../input
    )
  done
done
