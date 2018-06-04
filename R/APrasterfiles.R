#' AP Get Raster Files
#'
#' Get list of auxiliary raster files
#'
#' @param filePath character. Path to file
#'
#' @examples
#' \dontrun{
#' APSHPfiles("C:/Geodata/myfile.shp")
#' }
#'
#' @export

APrasterFiles <- function(filePath){

  # Get input file extension
  inExt <- tools::file_ext(filePath)

  # Check list of possible auxiliary files
  fileExt <- APfun::rasterExtensions[[inExt]]

  # If none are found, simply use the input file extension
  if(is.null(fileExt))  fileExt <- inExt

  files <- paste0(tools::file_path_sans_ext(filePath), ".", fileExt)

  files[file.exists(files)]

}
