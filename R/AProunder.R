#' AP Rounder
#'
#' Provides extra options for rounding numbers, such as rounding a value to uneven intervals
#' and setting those intervals to 'snap' or pass through a defined origin value. Can also be used on
#' Extent objects from the \code{raster} package.
#'
#' @param value numeric or Extent object. Input value
#' @param interval numeric. The interval to which the input value should be rounded
#' @param direction character. The rounding direction. Can be \code{'closest'}, \code{'up'} or \code{'down'} for numeric
#' \code{value} arguments or \code{'closest'}, \code{'in'} or \code{'out'} for Extent objects.
#' @param snap numeric. An origin value through which the interval with pass through. Default is 0.
#' @return Rounded number or Extent object
#' @export

AProunder <- function(value, interval, direction = "closest", snap = 0){

  if(sign(interval) == -1) stop("'interval' must be set to a positive value")

  # Take the modulo of the snap value and the interval
  snap.base <- snap %% interval

  # Account for a bug in the modulus operator which will sometimes add tiny fractional value to the modulo
  snap.base <- round(snap.base, 5)

  if(is(value, "Extent")){

      if(direction == "closest"){ return(raster::extent(
        easyRound(value@xmin, interval, direction, snap.base),
        easyRound(value@xmax, interval, direction, snap.base),
        easyRound(value@ymin, interval, direction, snap.base),
        easyRound(value@ymax, interval, direction, snap.base)
      ))
      }else if(direction == "in"){ return(raster::extent(
        easyRound(value@xmin, interval, "up",      snap.base),
        easyRound(value@xmax, interval, "down",    snap.base),
        easyRound(value@ymin, interval, "up",      snap.base),
        easyRound(value@ymax, interval, "down",    snap.base)
        ))
      }else if(direction == "out"){ return(raster::extent(
        easyRound(value@xmin, interval, "down",    snap.base),
        easyRound(value@xmax, interval, "up",      snap.base),
        easyRound(value@ymin, interval, "down",    snap.base),
        easyRound(value@ymax, interval, "up",      snap.base)
      ))
      }else stop("'direction' must be set to 'closest', 'in' or 'out")

  }else{

    return(easyRound(value, interval, direction, snap.base))
  }
}

# Create function that will correctly round 0.5 to 1, instead of 0
round2 <- function(x, n){
  posneg = sign(x)
  z = abs(x)*10^n
  z = z + 0.5
  z = trunc(z)
  z = z/10^n
  z*posneg
}

easyRound <- function(value, interval, direction, snap.base){

  if(!direction %in% c("closest", "up", "down")) stop("'direction' must be set to 'closest', 'up' or 'down")

  if(snap.base == 0){

    if(direction == "closest") return(interval * round2( value / interval, 0))
    if(direction == "up")      return(interval * ceiling(value / interval))
    if(direction == "down")    return(interval * floor(  value / interval))

  }else{

    # Round the input value upwards to the nearest interval
    value.ceiling <- interval * ceiling(value / interval)

    # Create series of three intervals and add modulo of snap value
    int.snap <- round(c(value.ceiling - interval * 2, value.ceiling - interval, value.ceiling) + snap.base, 5)

    # If the input value is already aligned with the interval and the snap value, return the value as-is
    if(0 %in% (value - int.snap)){

      return(value)

   }else{

      # Select the two intervals above and below the input value by removing the third which is furthest
      int.snap <- int.snap[-which.max(abs(value - int.snap))]

      # If the direction is set to "round", return the closest value
      if(direction == "closest"){
        return(int.snap[which.min(abs(value - int.snap))])}

      # If the direction is set to "up" or "down", return the corresponding value
      if(direction == "up"){
        return(int.snap[2])}
      if(direction == "down"){
        return(int.snap[1])}
    }
  }
}





