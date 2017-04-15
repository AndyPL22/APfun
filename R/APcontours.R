#' AP Contours
#'
#' Creates contours with rounded values
#' @param inRaster RasterLayer. A digital surface or digital elevation model
#' @param interval numeric. Interval for contour intervals
#' @export

APcontours <- function(inRaster, interval){

contLevel <- seq(AProunder(min(raster::getValues(inRaster), na.rm = TRUE), interval, "up"),
                 AProunder(max(raster::getValues(inRaster), na.rm = TRUE), interval, "down"),
                 by = interval)
raster::rasterToContour(inRaster, levels = contLevel)
}
