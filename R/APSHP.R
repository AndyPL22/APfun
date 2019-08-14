#' AP Get Shapefile files
#'
#' Get all files associated to a shapefile
#'
#' @param filePath character. Path to file
#'
#' @examples
#' \dontrun{
#' APSHPfiles("C:/Geodata/myfile.shp")
#' }
#'
#' @export

APSHPfiles <- function(filePath){

  fn <- function(path) paste0(tools::file_path_sans_ext(path), ".", APfun::SHPextensions)

  if(is.null(names(filePath))){
    files <- unlist(lapply(filePath, fn))
  }else{
    files <- utils::stack(lapply(filePath, fn))
    files <- structure(files[,"values"], names = as.character(files[,"ind"]))
  }

  files[file.exists(files)]

}

#' AP Delete Shapefile
#'
#' Delete a Shapefile and all associated files
#'
#' @param filePath character. Path to file
#'
#' @examples
#' \dontrun{
#' APSHPdel("C:/Geodata/myfile.shp")
#' }
#' @export

APSHPdel <- function(filePath){

  unlink(APSHPfiles(filePath))

}

#' AP Read Shapefile
#'
#' Read a Shapefile from a path
#'
#' @param filePath character. Path to file
#' @param warnings logical. If FALSE, then warnings will be suppressed
#'
#' @return SpatialPolygonsDataFrame
#'
#' @examples
#' \dontrun{
#' inPoly <- APSHPread("C:/Geodata/myfile.shp")
#' }
#'
#' @export

APSHPread <- function(filePath, warnings = FALSE){

  if(length(filePath) > 1) stop("Please input only one file path")
  if(!file.exists(filePath)) stop("File does not exist: ", filePath)

  if(!warnings){
    oldw <- getOption("warn")
    on.exit(  options(warn = oldw))
    options(warn = -1)
  }

  rgdal::readOGR(
    dsn     = dirname(filePath),
    layer   = tools::file_path_sans_ext(basename(filePath)),
    verbose = FALSE,
    addCommentsToPolygons = FALSE,
    stringsAsFactors      = FALSE
  )

}


#' AP Save to SHP
#'
#' Save a Spatial type object to disk as a Shapefile.
#'
#' @param object a Spatial object
#' @param outfile path for file to be saved
#' @param overwrite logical. Allow function to overwrite existing file. If set to 'prompt', it will
#' ask user whether or to overwrite
#'
#' @examples
#' \dontrun{
#' APSHPsave(inPoly, outfile = "C:/Geodata/myfile.shp")
#' }
#'
#' @export

APSHPsave <- function(object, outfile, overwrite = FALSE){

  if(!inherits(object, "Spatial")) stop("Input must be a Spatial object")
  if(!dir.exists(dirname(outfile))) stop("Output folder not found")

  if(any(duplicated(names(object)))) stop("Duplicated column names, cannot write file")

  rgdal::writeOGR(
    object, dirname(outfile),
    basename(tools::file_path_sans_ext(outfile)),
    driver = "ESRI Shapefile",
    overwrite = overwrite
  )
}

#' Shapefile extensions
#'
#' A vector of extensions for the various file types associated with Esri Shapefiles.
#'
#' @format Character vector
#'
#' @source https://en.wikipedia.org/wiki/Shapefile

"SHPextensions"
