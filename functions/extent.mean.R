##########################
# Function to get raster mean from combined, non-overlapping raster extents
#
# Author: Mark Ruddy
# Date: 2017-04-27
#
##########################


extent.mean <- function(r, e) { # r=raster and e=list of extents
  v <- unlist(lapply(e, function(x) extract(r, x)))
  n <- length(v[!is.na(v)])  # count cells
  vm <- mean(v, na.rm = TRUE)
  out <- list(vm, n)
  out
}


