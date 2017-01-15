#' AP Rounder
#'
#' Provides extra options for rounding numbers, such as rounding a value to uneven intervals
#' and setting those intervals to 'snap' or pass through a defined origin value.
#' @param value numeric input value value
#' @param interval numeric. The interval to which the input value should be rounded
#' @param direction character. The rounding direction. Can be \code{'closest'}, \code{'up'} or \code{'down'}
#' @param snap numeric. An origin value through which the interval with pass through. Default is 0.
#' @return Rounded number
#' @export

AProunder <- function(value, interval, direction = "closest", snap = 0){

  # Gatekeeper
  if(!direction %in% c("closest", "up", "down")) stop("'direction' must be set to 'closest', 'up' or 'down")
  if(sign(interval) == -1) stop("'interval' must be set to a positive value")

  # Create function that will correctly round 0.5 to 1, instead of 0
  round2 <- function(x, n){
    posneg = sign(x)
    z = abs(x)*10^n
    z = z + 0.5
    z = trunc(z)
    z = z/10^n
    z*posneg
  }

  # Take the modulo of the snap value and the interval
  snap.base <- snap %% interval

  # Account for a bug in the modulus operator which will sometimes add tiny fractional value to the modulo
  snap.base <- round(snap.base, 5)

  # If the snap base is 0
  if(snap.base == 0){

    if(direction == "closest"){
      return(interval * round2(value / interval, 0))}
    if(direction == "up"){
      return(interval * ceiling(value / interval))}
    if(direction == "down"){
      return(interval * floor(value / interval))}
  }

  # If the snap base is not 0
  if(snap.base != 0){

    # # Round the input value to the nearest interval
    # value.round <- interval * round2(value / interval, 0)
    #
    # # Add the modulo of the snap value and the interval to the rounded input value
    # value.round.snap <- value.round + snap.base
    # if(value.round.snap >= value){
    #   int.round.snap <- c(value.round.snap - interval, value.round.snap)}
    # if(value.round.snap < value){
    #   int.round.snap <- c(value.round.snap, value.round.snap + interval)}
    #
    # # Substract those values from the input value
    # int.round.diff <- value - int.round.snap

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
