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

  files <- unlist(lapply(filePath, function(path) paste0(tools::file_path_sans_ext(path), ".", APfun::SHPextensions)))

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

  if(warnings){
    rgdal::readOGR(dirname(filePath), tools::file_path_sans_ext(basename(filePath)),
                   verbose = FALSE,
                   stringsAsFactors = FALSE)
  }else{
    suppressWarnings(rgdal::readOGR(dirname(filePath), tools::file_path_sans_ext(basename(filePath)),
                   verbose = FALSE,
                   stringsAsFactors = FALSE))
  }
}

#' AP Convert KLM to SHP
#'
#' Read a KML file and save it to a SHP file and/or read it to memory. Requires the use of
#' 'ogrinfo.exe'.
#'
#' @param inFile path to a KML file
#' @param OSGeoPath character. Path to the OSGeo4W installation directory.
#' @param saveToFile logical. Save object as a Shapefile. File will be saved with the same name and in the
#' same folder as the \code{inFile}.
#' @param readToMemory logical. Input will be return as Spatial object.
#' @param dropDescription logical. If set to TRUE, the Description field of the KML file will be dropped
#'
#' @examples
#' \dontrun{
#' APKML2SHP("C:/Geodata/myfile.kml", dropDescription = FALSE)
#' }
#'
#' @export

APKML2SHP <- function(inFile, OSGeoPath = "C:\\OSGeo4W64", saveToFile = TRUE, readToMemory = FALSE, dropDescription = TRUE){

  # Check if 'ogrinfo.exe' exists
  ogrinfo <- file.path(OSGeoPath,"bin", "ogrinfo.exe")
  if(!file.exists(ogrinfo)) stop("Could not find file: ", ogrinfo)

  # Get layer name using 'ogrinfo.exe'
  kmlHeader <- system2(ogrinfo, args= inFile, stdout = TRUE)
  layerName <- strsplit(kmlHeader[3], "1: ")[[1]]
  if(length(kmlHeader) != 3 || length(layerName) != 2) stop("Could not read layer name from header:", "\n", paste(kmlHeader, collapse = "\n"))
  layerName <- layerName[2]

  # Read KML file
  inSHP <- rgdal::readOGR(inFile, layerName)

  # Drop messy 'Description' field
  if(dropDescription) inSHP@data <- subset(inSHP@data, select = names(inSHP) != "Description")

  if(saveToFile) rgdal::writeOGR(inSHP, dirname(inFile), basename(tools::file_path_sans_ext(inFile)), driver = "ESRI Shapefile")

  if(readToMemory) return(inSHP)

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

  if(overwrite == "prompt"){
    if(file.exists(outfile)){
      userInput <- yesno::yesno(paste0("File '",outfile, "' already exists.\nOverwrite?"))
      if(userInput){
        overwrite <- TRUE
      }else{
        stop()
      }
    }else{
      overwrite <- FALSE
    }
  }

  rgdal::writeOGR(object, dirname(outfile), basename(tools::file_path_sans_ext(outfile)),
                  driver = "ESRI Shapefile", overwrite = overwrite)
}

#' Shapefile extensions
#'
#' A vector of extensions for the various file types associated with Esri Shapefiles.
#'
#' @format Character vector
#'
#' @source https://en.wikipedia.org/wiki/Shapefile

"SHPextensions"
