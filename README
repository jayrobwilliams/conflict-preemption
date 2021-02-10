This is the repository for "The Curse of Geography: How Governments Preempt Secession Attempts."

# Replication

All analyses reported in the paper and supplemental information can be replicated using the following data files found in the Input Data directory:
- groups_nightlights.csv
- gadm data.csv
- grid data.csv
- myers_data.csv

The primary analysis dataset groups_nightlights.csv is created by nl_prep.R, which merges variables from many additional data sources into the primary geospatial data output, group data.csv.

## Results replication

This section assumes that you are starting from the datasets in Input Data and not creating all datasets from scratch. All R scripts are located in the Code subdirectory. To replicate the results:

1. Run Analysis nl_prep.R via Analysis nightlights prep.sl
2. Run all remaining Analysis_nl_*.R files via their respective SLURM scripts
3. Run results_plots.R
4. Run acd_trend.R, grid_plots.R, Map Creation.R, nl_sudan.R, separatism.R, AND MORE?? to generate supplemental results

To create group data.csv, gadm data.csv, and grid data.csv from original spatial data sources, see the section below.

## Geospatial data replication

To recreate all geospatial data from scratch, the workflow is as follows:

1. Linearly interpolate population rasters with Raster Prep/Count Interpolation.R
2. Perform intercalibration between satellites on nightlights data with Raster Prep/Nightlights Intercalibration.R
3. Create overlap-corrected rasters with Raster Prep/Overlap Correction hi-res.R and Raster Prep/Overlap Correction lo-res.R
  a. Note! These R scripts were originally run on an older cluster, and the resources requested by the SLURM submission scripts are a best estimate of the resources needed to complete the job since re-running them on Longleaf would be computationally wasteful. These jobs may fail due to out of memory errors and require adjustment of the requested resources in their SLURM submission scripts.
4. Fix topology errors in GeoEPR with GeoEPR Cleaning.R
5. Generate geospatial datasets with Group Variable Creation.R, GADM Variable Creation.R, and Grid Variable Creation.R

Once this is done, use Analysis nl_prep.R as described above to generate the analysis dataset and proceed with analyses.

# Computing environment

Replicating these scripts requires access to a high performance computing (HPC) environment, such as a university cluster. Most R scripts are written to be ran via job scheduler software in an HPC environment. These job submission scripts are written for the SLURM Workload Manage and have a '.sl' file extension. They may require adapting to work with a different HPC environment even if that environment uses SLURM. Of special note are the following scirpts:

- Code/Group Variable Creation.sl
- Code/GADM Variable Creation.sl
- Code/Raster Prep/Overlap Correction hi-res.sl
- Code/Raster Prep/Overlap Correction lo-res.sl
- Code/Raster Prep/Nightlights Intercalibration.sl

They are configured to use the 'bigmem' partition on UNC's Longleaf cluster and will likely require editing to use the appropriate large memory partition in other HPC environments. Additionally, the code to load R and any required geospatial libraries uses the "module" command and may need to be modified depending on the HPC environment. To submit a job to the SLURM scheduler, use the following command: "sbatch <script>.sl".

The majority of results in this are the product of Markov chain Monte Carlo estimation of Bayesian models. As such, even with attempts at exact reproducibility via the setting of random number generator seeds, it is possible that results will fail to replicate exactly. This can be the results of operating system differences and differences in external library as well as R package versions.

All analyses were carried out using R 3.5.0 AND MORE HERE

# Analysis scripts

The following R scripts can be run in a standard desktop computing environemnt and thus do not have corresponding SLURM scripts:

- Code/acd_trend.R
- Code/grid_plots.R
- Code/Map Creation.R
- Code/nl_sudan.R
- Code/results_plots.R
- Code/separatism.R

The following R scripts are auxiliary files called from other scripts and should not be run directly:

- Code/cshapes Recode.R
- Code/sfFunctions.R
- Code/Raster Prep/lintemp.R

All other R scripts should be run via their respective SLURM submission scripts.

# Datasets

The following datasets are needed to fully replicate the data creation and analyses:

- cshapes 0.6
- GeoEPR 2014
- GPW v3
- GPW v4
- NOAA DMSP OLS v4
- PRIO PETRODATA VERSIONXXX
- GADM 3.4
- UCDP/PRIO Armed Conflict Data 18.1
- UCDP/PRIO Dyadic Data 18.1
- EPR 2014
- ACD2EPR 2018
- FORGE 1.0
- Strategies of Resistance Data Project 2019
- de Jaun and Pierskalla 2015 replication data