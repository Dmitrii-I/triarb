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
  # get the base and quote currencies of the rates first:
  rates <- c(rate1, rate2, rate3)
  base_currs <- sapply(rates, function(x) substr(x, 1, 3))
  quote_currs <- sapply(rates, function(x) substr(x, 4, 6))
  # each of the three currencies must appear exactly twice,
  # either as base or quote, otherwise no triangular arbitrage possible
  if (!all(table(c(base_currs, quote_currs)) == 2)) 
    stop("No triangular arbitrage possible given the specified currencies")
  
  # rate product 1
  if (quote_currs[1] == base_currs[2]) {
    if (base_currs[1] == quote_currs[3]) { 
      rp1 <- x[, 2] * x[, 4] * x[, 6] 
    } else {
      rp1 <- x[, 2] * x[, 4] / x[, 7]
    }
  } else if (quote_currs[1] == quote_currs[2]) {
    if (base_currs[1] == base_currs[3]) {
      rp1 <- x[, 2] / x[, 5] / x[, 7]
    } else {
      rp1 <- x[, 2] / x[, 5] * x[, 6]
    }
  } else {
    if (quote_currs[1] == base_currs[1]) {
      rp1 <- x[, 2] * x[, 6] / x[, 5]
    } else {
      rp1 <- x[, 2] / x[, 7] / x[, 5]
    }
  }  
  
  return(data.frame(x$timestamp, rp1))
  
}