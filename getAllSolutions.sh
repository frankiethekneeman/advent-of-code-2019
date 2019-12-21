#!/bin/bash
for day in $(seq -f "%02g" 1  25)
do
  for part in $(seq 1 2)
  do 
    echo "DAY $day PART $part:"
    if [ -d day$day/part$part ]
    then
      (
          cd day$day/part$part
          time ghc solution.hs
          time ./solution ../input
      )
    fi
  done
done
