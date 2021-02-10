#!/bin/bash

#SBATCH --job-name=grp_vars

# one task for threaded parallelization
#SBATCH --tasks=1

# Number of CPU cores per task
#SBATCH --cpus-per-task=64

# dispatch job to large memory node
#SBATCH -p bigmem
#SBATCH --qos bigmem_access

# memory request
#SBATCH --mem=512gb

# 18 hour time limit
#SBATCH --time=18:00:00

# write outputs to directory to avoid cluttering top level directory
#SBATCH -o logs/group-variable-creation-%j.out # Standard output
#SBATCH -e logs/group-variable-creation-%j.out # Standard error

# create log directory
mkdir -p logs

# load the appropriate R module
module load r/3.5.0
module load geos/3.6.2
module load gdal/2.2.3
module load proj/4.9.3

# Use Rscript to run group variable creation
Rscript --vanilla Group\ Variable\ Creation.R
