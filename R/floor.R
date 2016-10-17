#' A function to extend the functionaliy of the base::floor function
#'
#' @param x A numeric
#' @param digits The number of digits to the left of the decimal place to round to
#' @return A numeric
#' @details Usually used for purchase data when you need to floor the cents
#' @examples
#' x <- 99.9999
#' floor(x, 2)
#' # is equivalent to:
#' base::floor(100 * x)
#' @export
floor <- function(x, digits = 0) base::floor(x * 10^digits) / 10^digits
