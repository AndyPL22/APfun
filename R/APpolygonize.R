#' AP Polygonize
#'
#' This function uses the \emph{gdal_polygonize.py} GDAL utility. Its implementation
#' was adapted from the solution developed by John Baumgartner and Francisco Rodriguez-Sanchez.
#'
#' This function needs OSGeo4W to be installed. The OSGeo4W installation path,
#' set to 'C:/OSGeo4W64' by default, will then be used to find the \emph{gdal_polygonize.bat}
#' file.
#'
#' @param inRaster a RasterLayer or a path to a raster file
#' @param readToMemory logical. Read output polygons into memory as a SpatialPolygonsDataFrame
#' @param outFile character. Optional path for saving output as an Esri Shapefile.
#' @param OSGeoPath character. Path to the OSGeo4W installation directory
#' @param connectivity numeric. Can be either set to 4 (rook's case) or 8 (queen's case)
#'
#' @return SpatialPolygonsDataFrame
#'
#' @seealso \itemize{
#' \item GDAL: \url{https://gdal.org/}
#' \item OSGeo4W download page: \url{https://trac.osgeo.org/osgeo4w/}
#' \item John Baumgartner's blog post on \emph{gdal_polygonize}: \url{https://johnbaumgartner.wordpress.com/2012/07/26/getting-rasters-into-shape-from-r/}
#' }
#'
#' @importFrom methods is
#'
#' @export

APpolygonize <- function(inRaster, readToMemory = TRUE, outFile = NULL, OSGeoPath = "C:/OSGeo4W64", connectivity = 4){

  if(!connectivity %in% c(4,8)) stop("Connectivity can only be 4 or 8")

  # Check if OSGeo files exist
  if(!file.exists(OSGeoPath)) stop("Could not find folder '", OSGeoPath, "'")

  # Get path for 'gdal_polygonize.bat'
  batPath <- file.path(OSGeoPath, "bin", "gdal_polygonize.bat")
  if(!file.exists(batPath)) stop("Could not find required file '", batPath, "'")

  # Create temporary output file if needed
  if(is.null(outFile)){

    outFile <- tempfile(fileext = ".shp")
    on.exit(unlink(list.files(dirname(outFile), pattern=basename(outFile), full.names = TRUE)))

  }else{

    if(!file.exists(dirname(outFile))) stop("Could not find output folder")
  }

  # Write temporary raster if needed
  if(is(inRaster, 'Raster')){

    # Check if 'inRaster' is already on the disk AND has an accepted file format
    deniedFormat <- c("grd")
    if(raster::fromDisk(inRaster) && !tools::file_ext(inRaster@file@name) %in% deniedFormat){
      rastPath <- normalizePath(inRaster@file@name)
    }else{
      rastPath <- tempfile(fileext='.asc')
      raster::writeRaster(inRaster, rastPath)
      on.exit(unlink(rastPath), add = TRUE)
    }
  }else{
    if(is.character(inRaster)){
      rastPath <- normalizePath(inRaster)
      inRaster <- raster::raster(inRaster)
    }else{
      stop("'inRaster' must be a file path (character string), or a Raster object.")
    }
  }

  # Set connectivity switch
  if(connectivity == 8) connectivity <- "-8 " else connectivity <- ""

  # Make vector of arguments
  args <- sprintf('%1$s "%2$s" -f "%3$s" "%4$s" -q',
                  connectivity, rastPath, "ESRI Shapefile", outFile)

  # Run OSGeo function (silence output by setting 'stdout' to TRUE)
  outputText <- withr::with_envvar(c(OSGEO4W_ROOT = OSGeoPath), system2(batPath, args = args, stdout = TRUE))

  # Check if successful
  if(!file.exists(outFile)) stop("Failed to create output file")

  # Read output shapefile
  if(readToMemory){
    shp <- APSHPread(outFile)
    raster::crs(shp) <- raster::crs(inRaster)
    return(shp)
  }
}
