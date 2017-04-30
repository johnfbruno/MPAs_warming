##########################
# Function to get raster means from combined, non-overlapping raster extents
#
# Author: Mark Ruddy
# Date: 2017-04-28
#
##########################

multi.extent.means <- function(r, z) { # r=raster, z=zones dataframe
  
  source("../functions/extent.mean.R")
  xlims <- c(-180, 180) # Longitudinal limits
  d <- dim(z)[1]
  
  # Get extents from limits
  for (i in 1:d) {
    ylims <- z[i,"limits"][[1]]
    e <- list(extent(xlims, ylims), extent(xlims, lapply(ylims, function(x) rev(-x)))) # Extents. Need to reverse order and sign of second extent for Southern hemisphere.
    out <- extent.mean(r = r, e = e)
    z$means[i] <- out[[1]]
    z$count[i] <- out[[2]]
  }
  
  z

}
