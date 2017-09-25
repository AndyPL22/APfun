#' AP Precision
#'
#' Prints input value with a set number digits.
#' @param x numeric. Input value value
#' @param digits numeric. Number of digits to display
#'
#' @export

APprecise <- function(x, digits = 16){
  oldDigits <- options()$digits
  options(digits = digits)
  print(x)
  options(digits = oldDigits)
}
