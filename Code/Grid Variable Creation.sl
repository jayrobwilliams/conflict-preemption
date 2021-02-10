#!/bin/bash

#SBATCH --job-name=grd_vars

# one task for threaded parallelization
#SBATCH --tasks=1

# Number of CPU cores per task
#SBATCH --cpus-per-task=16

# memory request
#SBATCH --mem=128gb

# three day time limit
#SBATCH --time=5-0

# write outputs to directory to avoid cluttering top level directory
#SBATCH -o logs/grid-variable-creation-%j.out # Standard output
#SBATCH -e logs/grid-variable-creation-%j.out # Standard error

# create log directory
mkdir -p logs

# load the appropriate R module
module load r/3.5.0
module load geos/3.6.2
module load gdal/2.2.3
module load proj/4.9.3

# Use Rscript to run group variable creation
Rscript --vanilla Grid\ Variable\ Creation.R
