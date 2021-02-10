#!/bin/bash

#SBATCH --job-name=myers_vars

# one task for threaded parallelization
#SBATCH --tasks=1

# Number of CPU cores per task
#SBATCH --cpus-per-task=16

# memory request
#SBATCH --mem=28gb

# 18 hour time limit
#SBATCH --time=4:00:00

# write outputs to directory to avoid cluttering top level directory
#SBATCH -o logs/myers-variable-creation-%j.out # Standard output
#SBATCH -e logs/myers-variable-creation-%j.out # Standard error

# create log directory
mkdir -p logs

# load the appropriate R module
module load r/3.5.0
module load geos/3.6.2
module load gdal/2.2.3
module load proj/4.9.3

# Use Rscript to run group variable creation
Rscript --vanilla Myers\ Variable\ Creation.R
