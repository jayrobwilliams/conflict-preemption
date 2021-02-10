#!/bin/bash

#SBATCH --job-name=gadm_vars

# one task for threaded parallelization
#SBATCH --tasks=1

# Number of CPU cores per task
#SBATCH --cpus-per-task=64

# dispatch job to large memory node
#SBATCH -p bigmem
#SBATCH --qos bigmem_access

# memory request
#SBATCH --mem=264gb

# two day time limit
#SBATCH --time=2-0

# write outputs to directory to avoid cluttering top level directory
#SBATCH -o logs/gadm-variable-creation-%j.out # Standard output
#SBATCH -e logs/gadm-variable-creation-%j.out # Standard error

# create log directory
mkdir -p logs

# load the appropriate R module
module load r/3.5.0
module load geos/3.6.2
module load gdal/2.2.3
module load proj/4.9.3

# Use Rscript to run cost surface creation
Rscript --vanilla GADM\ Variable\ Creation.R
