#!/bin/bash

#SBATCH --job-name=nl_ex_cy

# one task for threaded parallelization
#SBATCH --tasks=1

# Number of CPU cores per task
#SBATCH --cpus-per-task=20

# memory request
#SBATCH --mem=48gb

# five hour time limit
#SBATCH --time=4:00:00

# write outputs to directory to avoid cluttering top level directory
#SBATCH -o logs/nl-ex-cy-%j.out # Standard output
#SBATCH -e logs/nl-ex-cy-%j.out # Standard error

# create log directory
mkdir -p logs

# load the appropriate R module
module load r/3.5.0

# Use Rscript to run cost surface creation
Rscript --vanilla Analysis\ nl_excl_country_year.R
