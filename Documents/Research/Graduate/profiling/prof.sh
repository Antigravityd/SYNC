#!/bin/bash

for infile in $(find ~/phits327A/phits/recommendation -regex '.*\.inp')
do
    echo "Executing $infile"
    ~/phits327A/phits/phits_LinGfort_DBG < ${infile}
    gprof -b -p --directory-path=~/phits327A/phits --inline-file-names ~/phits327A/phits/phits_LinGfort_DBG gmon.out > $(basename ${infile}).prof
done

rm *.out *.eps *.dat *.inp *.6
