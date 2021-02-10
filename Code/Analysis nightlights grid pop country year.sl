#!/bin/bash

#SBATCH --job-name=nl_grp_cy

# one task for threaded parallelization
#SBATCH --tasks=1

# send to general partition
#SBATCH -p general

# Number of CPU cores per task
#SBATCH --cpus-per-task=4

# memory request
#SBATCH --mem=8gb

# eleven day time limit
#SBATCH --time=11-0

# write outputs to directory to avoid cluttering top level directory
#SBATCH -o logs/nl-grid-pop-cy-%j.out # Standard output
#SBATCH -e logs/nl-grid-pop-cy-%j.out # Standard error

# load the appropriate R module
module load r/3.5.0

# Use Rscript to run analysis
Rscript --vanilla Analysis\ nl_grid_pop_country_year.R
