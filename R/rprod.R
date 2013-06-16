#' Computes rate product of three currencies
#' 
#' The function returns the rate product of three currencies. Rate products
#' larger than 1 signal arbitrage opportunity.
#' @param x A data frame with first column being the date-time stamp and 
#' columns 2 through 7 being the bid and ask quotes of the three currencies.
#' @param rate1 Specifies the first currency rate in the form of a 6-character 
#' currency rate identifier. For example, the eurodollar rate is specified by "EURUSD",
#' the pound-dollar rate by "GBPUSD", and similarly for other currencies. Any
#' identifier is accepted as long as the first three characters refer to the base
#' currency and last three to the quote currency. Therefore "FOOBAR" is accepted, and
#' it is up to the user to deal with semantics.
#'  @param rate2 See 'rate1'
#'  @param rate3 See 'rate1' 
#' @export
#'
#'
rprod <- function(x, rate1, rate2, rate3) {
  # to compute rate product, the rates must have proper form
  basecurr1 <- substr(rate1, 1, 3)
  quotecurr1 <- substr(rate1, 4, 6)
  basecurr2 <- substr(rate2, 1, 3)
  quotecurr2 <- substr(rate2, 4, 6)
  basecurr3 <- substr(rate3, 1, 3)
  quotecurr3 <- substr(rate3, 4, 6)
  
}