#!/bin/bash
#SBATCH -A jzwart
#SBATCH --time=5
#SBATCH --nodes=2
#SBATCH --tasks-per-node=40

module load cray-R

setenv R_LIBS ~/Rlib

Rscript calibrate_sntemp_denali.R
