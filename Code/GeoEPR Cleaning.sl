#!/bin/sh

#SBATCH --job-name=GeoEPR_cl

# one task for threaded parallelization
#SBATCH --tasks=1

# Number of CPU cores per task
#SBATCH --cpus-per-task=1

# memory request
#SBATCH --mem=4gb

# one day time limit
#SBATCH --time=1-0

# load the appropriate R module
module load r/3.5.0
module load geos/3.6.2
module load gdal/2.2.3
module load proj/4.9.3

# Use Rscript to run cost surface creation
Rscript GeoEPR\ Cleaning.R
