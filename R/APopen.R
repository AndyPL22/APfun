#' AP Folder Open
#'
#' Open a folder in Windows Explorer
#' @param x character. File path. If path leads to a directory, it will open that directory. If it directs to a file, it
#' will open that file's directory

#' @export

APopen <- function(x){

  # Check if input is a directory. If not get its directory
  if(!file.info(x)[,"isdir"]) x <- dirname(x)

  # Check if file exists
  if(!file.exists(x)) stop("File does not exist")

  # Substitute forward slashes with backslashes
  x <- gsub("/", "\\\\", x)

  invisible(suppressWarnings(shell(paste("explorer", x), intern=TRUE)))

}
