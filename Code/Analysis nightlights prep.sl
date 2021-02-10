#!/bin/bash

#SBATCH --job-name=nl_prep

# one task for threaded parallelization
#SBATCH --tasks=1

# send to general partition
#SBATCH -p general

# Number of CPU cores per task
#SBATCH --cpus-per-task=1

# memory request
#SBATCH --mem=2gb

# ten minute time limit
#SBATCH --time=0:10:00

# write outputs to directory to avoid cluttering top level directory
#SBATCH -o logs/nl-prep-%j.out # Standard output
#SBATCH -e logs/nl-prep-%j.out # Standard error

# create log directory
mkdir -p logs

# load the appropriate R module
module load r/3.5.0

# Use Rscript to run analysis
Rscript --vanilla Analysis\ nl_prep.R
