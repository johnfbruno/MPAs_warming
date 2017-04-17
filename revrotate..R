revrotate<-function (x, ...) 
{
  .local <- function (x, filename = "", ...) 
  {
    e <- extent(x)
    ext1 <- extent(-180, 0, -90, 90)
    if (is.null(intersect(e, ext1))) {
      r1 <- NULL
    }
    else {
      r1 <- crop(x, ext1)
    }
    ext2 <- extent(0, 180 + res(x)[1], -90, 90)
    if (is.null(intersect(e, ext2))) {
      r2 <- NULL
    }
    else {
      r2 <- crop(x, ext2)
      r2 <- shift(r2, -360)
    }
    ln <- names(x)
    if (is.null(r1)) {
      out <- r2
    }
    else if (is.null(r2)) {
      out <- r1
    }
    else {
      out <- merge(r1, r2, overlap = FALSE)
    }
    names(out) <- names(x)
    out@z <- x@z
    p <- projection(out)
    extent(out)<-c(0,360,ymin(x),ymax(x))
    if (length(grep("\\+over", p)) > 0) {
      projection(out) <- gsub("[[:space:]]\\+over", "", 
                              p)
    }
    if (filename != "") {
      out <- writeRaster(out, filename, ...)
    }
    return(out)
  }
  .local(x, ...)
}