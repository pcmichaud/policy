#!/bin/bash
#$ -N scenarios
#$ -l m_mem_free=5G
#$ -m e -M pcmichaud@gmail.com
#$ -o logs/EER-revision/$JOB_NAME-$JOB_ID.log

mpiexec ./runscenario baseline
mpiexec ./runscenario prog1
mpiexec ./runscenario prog2
mpiexec ./runscenario prog3
mpiexec ./runscenario prog4
mpiexec ./runscenario prog5
mpiexec ./runscenario prog6
mpiexec ./runscenario prog7
mpiexec ./runscenario prog8
mpiexec ./runscenario prog9
mpiexec ./runscenario prog10
mpiexec ./runscenario prog11





 
