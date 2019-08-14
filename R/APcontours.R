#' AP Contours
#'
#' Creates contours with rounded values
#' @param inRaster RasterLayer. A digital surface or digital elevation model
#' @param interval numeric. Interval for contour intervals
#' @param max.contour.segments numeric. Maximum number of segments for a single contour line. If set to 'NULL',
#' default value will be 25,000.
#' @export

APcontours <- function(inRaster, interval, max.contour.segments = NULL){

  # Seg maximum number of segments
  if(!is.null(max.contour.segments)){
    oo <- options(max.contour.segments = max.contour.segments)
    on.exit(options(oo))
  }

  # Get minimum and maximum raster values
  rasInfo <- tryCatch(rgdal::GDALinfo(inRaster@file@name), warning = function(x) FALSE, error = function(e) FALSE)
  rasterRange <- if(class(rasInfo) == "GDALobj"){
    unlist(attributes(rasInfo)$df[,c("Bmin", "Bmax")])
  }else{
    raster::cellStats(inRaster, stat = "range")
  }

  # Get sequence of contour levels
  contLevels <- seq(AProunder(rasterRange[1], interval, "up"),
                   AProunder(rasterRange[2], interval, "down"),
                   by = interval)

  # Generate contours
  cont <- raster::rasterToContour(inRaster, levels = contLevels)

  # Make 'level' value numeric
  cont[["level"]] <- as.numeric(as.character(cont[["level"]]))

  return(cont)
}
