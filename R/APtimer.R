#' AP Timer
#'
#' Basic timer.
#' @param marker Optional object of class \code{'POSIXct'}
#' @return If \code{marker=NULL}, then \code{APtimer} returns an object of class \code{'POSIXct'}.
#' When this same object is used as an input later on, then \code{APtimer} will print the time elapsed
#' since it was evaluated
#' @export

APtimer <- function(marker = NULL, hush = FALSE){

  # If no time marker is given, then create a new one
  if(is.null(marker)){

    if(!hush) cat("Started timer at:", format(Sys.time(), "%Y-%m-%d, %X"), "\n")

    return(proc.time())

  # If a marker is given, then return the time elapsed since that marker
  }else{

    elapsed.seconds <- (proc.time() - marker)["elapsed"]

    if(elapsed.seconds < 60){
      elapsed.print <- paste(round(elapsed.seconds, 2), "seconds")}
    if(elapsed.seconds >= 60){
      elapsed.print <- paste(round(elapsed.seconds/60, 2), "minutes")}
    if(elapsed.seconds >= 3600){
      elapsed.print <- paste(round(elapsed.seconds/3600, 2), "hours")}

    if(!hush){
      cat("Finished at:", format(Sys.time(), "%Y-%m-%d, %X"), "\n")
      cat("Time elapsed:", elapsed.print,"\n")
    }else{
      return(elapsed.print)
    }
  }
}
