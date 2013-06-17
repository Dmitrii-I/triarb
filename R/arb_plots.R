#' Visualize triangular arbitrage opportunities
#' 
#' This function plots a bar chart of arbitrage opportunities found in a currencies triple. 
#' @param x A data frame returned by 'rprod'
#' @export
#'
#'
#'@seealso \code{\link{rprod}}
#' @return Two plots, showing arbitrage opportunities for each of two possible roundtrips
#' @examples
#' data(forex_quotes)
#' rate_products <- rprod(ticks, "EURUSD", "GBPUSD", "EURGBP")
#' arb_plots(rate_products)
#' 
arb_plots <- function(x) {
    par(mfrow=c(2, 1))
    
    arb1 <- x[x[, 2] > 1, c(1, 2)]
    barplot(sort((arb1[, 2] - 1) * 10000, decreasing=TRUE), xlab="Arbitrage opportunities, sorted", 
            ylab="Profit in pips", main=substitute(paste(n, " arbitrage opportunities in ", x, " roundtrip"), 
                                                   list(n=nrow(arb1), x=names(arb1)[2])))
    
    arb2 <- x[x[, 3] > 1, c(1, 3)]
    barplot(sort((arb2[, 2] - 1) * 10000, decreasing=TRUE), xlab="Arbitrage opportunities, sorted", 
            ylab="Profit in pips", main=substitute(paste(n, " arbitrage opportunities in ", x, " roundtrip"), 
                                                   list(n=nrow(arb2), x=names(arb2)[2])))
    

}