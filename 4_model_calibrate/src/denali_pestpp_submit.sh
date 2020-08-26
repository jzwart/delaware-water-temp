#!/bin/bash
#SBATCH -N 2
#SBATCH -A iidd
#SBATCH -p workq
#SBATCH --mail-type=begin,end,fail
#SBATCH --mail-user=jzwart@usgs.gov

cd /caldera/projects/usgs/water/iidd/datasci/delaware-water-temp/4_model_calibrate/tmp

module load pestpp/4.2.16

pestpp-ies pestpp/subbasin_4182.pst
