#!/bin/bash

#SBATCH --job-name=nl_ga_cy

# one task for threaded parallelization
#SBATCH --tasks=1

# send to general partition
#SBATCH -p general

# Number of CPU cores per task
#SBATCH --cpus-per-task=4

# memory request
#SBATCH --mem=4gb

# eleven day time limit
#SBATCH --time=1-0

# write outputs to directory to avoid cluttering top level directory
#SBATCH -o logs/nl-gadm-cy-%j.out # Standard output
#SBATCH -e logs/nl-gadm-cy-%j.out # Standard error

# load the appropriate R module
module load r/3.5.0

# Use Rscript to run analysis
Rscript --vanilla Analysis\ nl_gadm_country_year.R
