This R script performs intercalibration on DMSP-OLS nightlights data to allow for use in time series analysis. Due to the computationally intensive nature of the operations involved, it is designed to be run on a multicore cluster in a Unix environment.

Due to sensor drift over time within satellites and differences in settings between satellites, the digital number (DN) values in the data are not directly comparable. Following Wu et al., 2013, this script performs intercalibration of the data using the invariant region method. Based on the fact that there are regions of the earth where light levels are relatively stable, we can assume that differences in values for these areas are due to sensor error. By picking a reference image, we can regress the observed values in all other images on this reference and obtain coefficients used to calibrate all other images. After intercalibration is complete, years with data from two different satellites are averaged to obtain more accurate measurements. Once this is done, we can make meaningful comparisons across years in the data and employ them in time series analysis.

Where Wu et al., 2013 use a power regression model with one added to both the reference and uncalibrated MORE ON MODEL


This script comes packaged with RDS files for polygons of Mauritius, Japan, and Puerto Rico. The script subsets Japan to Okinawa, and then uses Mauritius, Okinawa, and Puerto Rico as the invariant regions as in Wu et al., 2013.

- The script requires the 'sp', 'raster', and 'doParallel' packages and all of their dependencies.

- The script expects two subdirectories titled "Stable Lights" and "Calibrated" in the working directory.

- Place the DMSP-OLS nightlights rasters for the years you want to intercalibrate in the Stable Lights subdirectory.

- Move the raster for ONE year to the Calibrated subdirectory. This will be your reference image that all other images will be calibrated relative to.

- Do not rename any of the stable lights raster files after downloading them from the DMSP site. Code in this script relies on the structured nature of the filenames.



Before running, multiple changes are required to the script:

- Edit the first line of the script so that the path after the hashbang points to your Rscript interpreter.

- On line 28 decide whether you wish to retain the input rasters after intercalibration is complete.

- On line 31 decide whether you want the script to archive the output for easier retrieval from the cluster.

- On line 42 set the number of cores in the makeCluster() function to the number of cores you will be requesting on the cluster. The detectCores() function is not reliable on clusters, so you need to hard code this.

- On completion, this script will create a subdirectory titled “Output” filled with files titled nl_YYYY.

- If keep.input == T, the original files will be retained.

- If archive.output == T, the Output directory will tarred and gzipped.




notes:

- This code is written for data where there are never more than two rasters for the same year. If DSMP data are released in the future where there are years with rasters from three different satellites, this script will not work.

- Other choices of invariant region(s) can be used. The code just needs to be adapted to employ them instead. This may be preferable if you are only interested in performing intercalibrations on a subset of the data instead of the entire world.

