#!/bin/bash

phits_MPI="/home/dnw/phits327A/phits/bin/phits326_lin_mpi.exe"
phits_OMP="/home/dnw/phits327A/phits/bin/phits326_lin_openmp.exe"
phits_single="/home/dnw//phits327A/phits/bin/phits326_lin.exe"

while getopts ":MOh" option; do
    case $option in
	\?) echo -n "phits: unrecognized argument "
	    echo -n ${OPTARG}
	    echo "; see phits -h for help."
	    exit;;

	M)  phitsin_path="$(pwd)/phits.in"
	    # borrowed from calandoa on SO
	    nextopt=${!OPTIND}
	    if [[ $nextopt != -* && $nextopt != ${@: -1} ]] ; then
		OPTIND=$((OPTIND + 1))
		np=$nextopt
	    else
		np=$(nproc --all)
	    fi
	    echo "file = $(readlink -f ${@: -1})" > ${phitsin_path}
	    mpirun -np ${np} ${phits_MPI}
	    exit;;

	O) ${phits_OMP} < ${@: -1}
	   exit;;

	h) echo "phits: nicer command-line interface to JAEA's Particle Heavy Ion Transport code System"
	   echo "Usage: phits -[MOh] [file]"
	   echo "Options:"
	   echo "    none: run the standard PHITS single-core binary on [file]"
	   echo "    -M [proc]: copy the path of [file] to phits.in and run a PHITS MPI binary on all cores of the current machine. If specified, use [proc] processes; otherwise, use every core."
	   echo "    -O: run a PHITS OpenMP binary on [file]."
	   echo "    -h: display this message."
	   exit;;
    esac
done

# no option given; use single-thread binary

if [[ -n $1 ]] ; then
    ${phits_single} < $1
else
    echo "Error: no input file specified."
fi
