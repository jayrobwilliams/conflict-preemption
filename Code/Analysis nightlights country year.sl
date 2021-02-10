#!/bin/bash

#SBATCH --job-name=nl_cy

# one task for threaded parallelization
#SBATCH --tasks=1

# send to general partition
#SBATCH -p general

# Number of CPU cores per task
#SBATCH --cpus-per-task=20

# memory request
#SBATCH --mem=40gb

# five hour time limit
#SBATCH --time=6:00:00

# write outputs to directory to avoid cluttering top level directory
#SBATCH -o logs/nl-cy-%j.out # Standard output
#SBATCH -e logs/nl-cy-%j.out # Standard error

# create log directory
mkdir -p logs

# load the appropriate R module
module load r/3.5.0

# Use Rscript to run analysis
Rscript --vanilla Analysis\ nl_country_year.R
