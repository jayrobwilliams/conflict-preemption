#!/bin/bash

#SBATCH --job-name=over_hi

# one task for threaded parallelization
#SBATCH --tasks=1

# Number of CPU cores per task
#SBATCH --cpus-per-task=8

# dispatch job to large memory node
#SBATCH -p bigmem
#SBATCH --qos bigmem_access

# memory request
#SBATCH --mem=2048gb

# three day time limit
#SBATCH --time=3-0

# load the appropriate R module
module load r/3.5.0
module load geos/3.6.2
module load gdal/2.2.3
module load proj/4.9.3

# Use Rscript to run cost surface creation
Rscript --vanilla Overlap\ Correction\ hi-res.R
