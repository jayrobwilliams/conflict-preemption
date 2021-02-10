#### Temporal Interpolation ####################################################
# Perform cell-wise linear interpolation between multiple raster layers, and 
# extrapolation beyond the upper limit of input data. Output is saved in .tif 
# format.
#
# Arguments
# s: a rasterStack containing the time slices to be interpolated 
#
# xin: a numeric vector that indicates the times associated with layers in s (in
# the same order as the layers of s - see names(s))
#
# xout: a numeric vector that indicates the times to be interpolated to (NB: if
# xout extends beyond the latest time slice in xin, it will be extrapolated
# using the rate of change from the period between the last and second to last
# time in xin.) 
#
# outdir: the directory to which files will be written (recursively created if 
# not already in existence) (character string)
#
# prefix: the output files will have pattern prefix_x.tif, where x is the
# timestep (potentially multiple digits), and prefix is a string that you 
# specify here (character string) 
#
# progress: show progress bar (TRUE/FALSE) 
#
# writechange: write the change grids that define the change in cell value per
# timestep between each pair of time slices, (TRUE/FALSE). If TRUE, these are
# written to outdir.
#
# returnstack: if TRUE, returns the interpolated grids (at timesteps xout) as a 
# rasterStack (TRUE/FALSE)
#
# ...: additional arguments passed to writeRaster

interpolateTemporal <- function(s, xin, xout, outdir, prefix, progress=TRUE, 
                                writechange=TRUE, returnstack=FALSE, ...) {
  require(raster)
  require(rgdal)
  if(nlayers(s) != length(xin)) stop('Length of xin must equal the number of layers in s.')
  if(nlayers(s) < 2) stop('stack s must have at least 2 layers.')
  if(!all(findInterval(xout, range(xin), rightmost.closed=TRUE) == 1)) {
    if(any(xout < min(xin))) {
      stop('This function does not extrapolate backwards (i.e. below the earliest element in xin). All elements of xout must be greater that min(xin).')
    } else {      
      warning('Some values of xout require extrapolation beyond the range of xin.\nThe rate of change for extrapolation is assumed to be equal to that for the period between the last and second-to-last elements of xin (after temporal ordering).')
    }
  }
  outdir <- normalizePath(sub('/$|\\\\$', '', outdir), winslash='/', 
    mustWork=FALSE)
  if(!file.exists(outdir)) dir.create(outdir, recursive=TRUE)
  xout <- unique(xout)
  if(is.unsorted(xin)) {
    s <- s[[order(xin)]]
    xin <- sort(xin)
  }
  len <- diff(xin)
  base <- findInterval(xout, xin)
  lower <- unique(base[base < nlayers(s)])
  s.change <- stack(sapply(if(length(lower) > 0) lower else nlayers(s) - 1, 
    function(x) {
      message(sprintf('Calculating change grid for %s to %s.', xin[x], xin[x+1]))
      overlay(s[[x]], s[[x+1]], fun=function(x1, x2) (x2-x1)/len[x],
        filename=ifelse(writechange, 
        file.path(outdir, sprintf('changegrid_%s_%s', xin[x], xin[x+1])), 
        ''), recycle=FALSE, format='GTiff', ...)
  }))
  
  multi <- xout - xin[base]
  chg.ind <- ifelse(base > nlayers(s.change), nlayers(s.change), base)
  message('Calculating grids for years specified in xout...')
  if(progress) pb <- txtProgressBar(0, length(xout), style=3)
  invisible(sapply(seq_along(xout), function(x) {
    out.rast <- if(xout[x] %in% xin) {
      s[[base[x]]]
    } else {
      overlay(s[[base[x]]], s.change[[chg.ind[x]]],
        fun=function(x1, x2) x1 + (x2*multi[x]))
    }
    writeRaster(out.rast, 
      filename=file.path(outdir, sprintf('%s_%s', prefix, xout[x])), 
      format='GTiff', ...)
    if(progress) setTxtProgressBar(pb, x)
  }))
  if(isTRUE(returnstack)) {
    f <- file.path(outdir, paste0(prefix, '_', xout, '.tif'))
    return(stack(f[order(as.numeric(sub('.*_(\\d+)\\.tif$', '\\1', f)))]))
  }
}