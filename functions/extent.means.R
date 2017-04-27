##########################
# Function to get total means from various combined, non-overlapping raster extents
#
# Author: Mark Ruddy
# Date: 2017-04-27
#
##########################


extent.means <- function(raster, extents) { # raster and list of extents
  e <- extents
  v <- unlist(lapply(e, function(x) extract(raster, x)))
  vm <- mean(v, na.rm = TRUE)
  vm
}



