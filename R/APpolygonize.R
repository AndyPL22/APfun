#' AP Polygonize
#'
#' This function uses the \emph{gdal_polygonize.py} GDAL utility. Its implementation
#' was adapted from the solution developed by John Baumgartner and Francisco Rodriguez-Sanchez.
#'
#' This function needs OSGeo4W to be installed. The OSGeo4W installation path,
#' set to 'C:\\OSGeo4W64' by default, will then be used to find the \emph{OSGeo4W.bat} and \emph{gdal_polygonize.py}
#' files. Python must be installed in order to run \emph{gdal_polygonize.py}.
#'
#' @param inRaster a RasterLayer or a path to a raster file
#' @param OSGeoPath character. Path to the OSGeo4W installation directory
#' @param connectivity numeric. Can be either set to 4 (rook's case) or 8 (queen's case)
#'
#' @return SpatialPolygonsDataFrame
#'
#' @seealso \itemize{
#' \item GDAL: \url{http://www.gdal.org/}
#' \item OSGeo4W download page: \url{https://trac.osgeo.org/osgeo4w/}
#' \item John Baumgartner's blog post on \emph{gdal_polygonize}: \url{https://johnbaumgartner.wordpress.com/2012/07/26/getting-rasters-into-shape-from-r/}
#' }
#'
#' @importFrom methods is
#'
#' @export

APpolygonize <- function(inRaster, OSGeoPath = "C:\\OSGeo4W64", connectivity = 4){

  if(!connectivity %in% c(4,8)) stop("Connectivity can only be 4 or 8")

  # Check if OSGeo files exist
  if(!file.exists(OSGeoPath)) stop("Could not find folder '", OSGeoPath, "'")
  batpath <- file.path(OSGeoPath,"OSGeo4W.bat")
  if(!file.exists(batpath)) stop("Could not find required file '", batpath, "'")
  polpath <- file.path(OSGeoPath, "bin\\gdal_polygonize.py")
  if(!file.exists(polpath)) stop("Could not find required file '", polpath, "'")
  pypath <- file.path(OSGeoPath, "apps\\Python27\\lib\\site-packages\\osgeo\\__init__.py")
  if(!file.exists(pypath)) stop("Could not find required file '", pypath, "'. Python may not have been installed with OSGeo")

  # Create temporary outfile if needed
  outshape <- tempfile()
  on.exit(unlink(list.files(dirname(outshape), pattern=basename(outshape), full.names = TRUE)))

  # Write temporary raster if needed
  if(is(inRaster, 'Raster')){
    rastpath <- tempfile(fileext='.asc')
    raster::writeRaster(inRaster, rastpath)
    on.exit(unlink(rastpath), add = TRUE)
  }else{
    if(is.character(inRaster)){
      rastpath <- normalizePath(inRaster)
      inRaster <- raster::raster(inRaster)
    }else{
      stop("'inRaster' must be a file path (character string), or a Raster object.")
    }
  }

  # Set connectivity switch
  if(connectivity == 8) connectivity <- "-8 " else connectivity <- ""

  # Run OSGeo function
  system2(batpath, args=sprintf('"%1$s" %2$s "%3$s" -f "%4$s" "%5$s.shp" -q',
                        sub('\\.py$', '', polpath), connectivity, rastpath, "ESRI Shapefile", outshape))

  # Read output shapefile
  shp <- rgdal::readOGR(dirname(outshape), layer = basename(outshape), verbose= FALSE)
  raster::crs(shp) <- raster::crs(inRaster)
  names(shp) <- "layer"
  return(shp)
}
